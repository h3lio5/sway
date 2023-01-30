use crate::{
    asm_generation::{
        asm_builder::{AsmBuilder, AsmBuilderResult},
        from_ir::{ir_type_size_in_bytes, StateAccessType, Storage},
        fuel::{
            abstract_instruction_set::AbstractInstructionSet,
            compiler_constants,
            data_section::{DataId, DataSection, Entry},
            register_sequencer::RegisterSequencer,
        },
        ProgramKind,
    },
    asm_lang::{virtual_register::*, Label, Op, VirtualImmediate12, VirtualImmediate18, VirtualOp},
    decl_engine::DeclRef,
    error::*,
    fuel_prelude::fuel_crypto::Hasher,
    metadata::MetadataManager,
    size_bytes_in_words,
};
use sway_error::warning::CompileWarning;
use sway_error::{error::CompileError, warning::Warning};
use sway_ir::*;
use sway_types::{span::Span, Spanned};

use either::Either;
use std::{collections::HashMap, sync::Arc};

pub struct FuelAsmBuilder<'ir> {
    pub(super) program_kind: ProgramKind,

    // Data section is used by the rest of code gen to layout const memory.
    pub(super) data_section: DataSection,

    // Register sequencer dishes out new registers and labels.
    pub(super) reg_seqr: RegisterSequencer,

    // Label maps are from IR functions or blocks to label name.  Functions have a start and end
    // label.
    pub(super) func_label_map: HashMap<Function, (Label, Label)>,
    pub(super) block_label_map: HashMap<Block, Label>,

    // Reg map is tracking IR values to VM values.  Ptr map is tracking IR pointers to local
    // storage types.
    pub(super) reg_map: HashMap<Value, VirtualRegister>,
    pub(super) ptr_map: HashMap<LocalVar, Storage>,

    // The currently compiled function has an end label which is at the end of the function body
    // but before the call cleanup, and a copy of the $retv for when the return value is a reference
    // type and must be copied in memory.  Unless we have nested function declarations this vector
    // will usually have 0 or 1 entry.
    pub(super) return_ctxs: Vec<(Label, VirtualRegister)>,

    // Stack size and base register for locals.
    pub(super) locals_ctxs: Vec<(u64, VirtualRegister)>,

    // IR context we're compiling.
    pub(super) context: &'ir Context,

    // Metadata manager for converting metadata to Spans, etc.
    pub(super) md_mgr: MetadataManager,

    // Final resulting VM bytecode ops; entry functions with their function and label, and regular
    // non-entry functions.
    pub(super) entries: Vec<(Function, Label, Vec<Op>, Option<DeclRef>)>,
    pub(super) non_entries: Vec<Vec<Op>>,

    // In progress VM bytecode ops.
    pub(super) cur_bytecode: Vec<Op>,
}

pub type FuelAsmBuilderResult = (
    DataSection,
    RegisterSequencer,
    Vec<(Function, Label, AbstractInstructionSet, Option<DeclRef>)>,
    Vec<AbstractInstructionSet>,
);

impl<'ir> AsmBuilder for FuelAsmBuilder<'ir> {
    fn func_to_labels(&mut self, func: &Function) -> (Label, Label) {
        self.func_to_labels(func)
    }

    fn compile_function(&mut self, function: Function) -> CompileResult<()> {
        self.compile_function(function)
    }

    fn finalize(&self) -> AsmBuilderResult {
        self.finalize()
    }
}

impl<'ir> FuelAsmBuilder<'ir> {
    pub fn new(
        program_kind: ProgramKind,
        data_section: DataSection,
        reg_seqr: RegisterSequencer,
        context: &'ir Context,
    ) -> Self {
        FuelAsmBuilder {
            program_kind,
            data_section,
            reg_seqr,
            func_label_map: HashMap::new(),
            block_label_map: HashMap::new(),
            reg_map: HashMap::new(),
            ptr_map: HashMap::new(),
            return_ctxs: Vec::new(),
            locals_ctxs: Vec::new(),
            context,
            md_mgr: MetadataManager::default(),
            entries: Vec::new(),
            non_entries: Vec::new(),
            cur_bytecode: Vec::new(),
        }
    }

    // This is here temporarily for in the case when the IR can't absolutely provide a valid span,
    // until we can improve ASM block parsing and verification mostly. It's where it's needed the
    // most, for returning failure errors.  If we move ASM verification to the parser and semantic
    // analysis then ASM block conversion shouldn't/can't fail and we won't need to provide a
    // guaranteed to be available span.
    fn empty_span() -> Span {
        let msg = "unknown source location";
        Span::new(Arc::from(msg), 0, msg.len(), None).unwrap()
    }

    pub(super) fn insert_block_label(&mut self, block: Block) {
        if &block.get_label(self.context) != "entry" {
            let label = self.block_to_label(&block);
            self.cur_bytecode.push(Op::unowned_jump_label(label))
        }
    }

    pub fn finalize(&self) -> AsmBuilderResult {
        AsmBuilderResult::Fuel((
            self.data_section.clone(),
            self.reg_seqr,
            self.entries
                .clone()
                .into_iter()
                .map(|(f, l, ops, test_decl_ref)| {
                    (f, l, AbstractInstructionSet { ops }, test_decl_ref)
                })
                .collect(),
            self.non_entries
                .clone()
                .into_iter()
                .map(|ops| AbstractInstructionSet { ops })
                .collect(),
        ))
    }

    pub(super) fn compile_instruction(
        &mut self,
        instr_val: &Value,
        func_is_entry: bool,
    ) -> CompileResult<()> {
        let Some(instruction) = instr_val.get_instruction(self.context) else {
            return err(vec![], vec![CompileError::Internal(
                "Value not an instruction.",
                self.md_mgr
                    .val_to_span(self.context, *instr_val)
                    .unwrap_or_else(Self::empty_span),
            )]);
        };

        // The only instruction whose compilation returns a CompileResult itself is AsmBlock, which
        // we special-case here.  Ideally, the ASM block verification would happen much sooner,
        // perhaps during parsing.  https://github.com/FuelLabs/sway/issues/801
        if let Instruction::AsmBlock(asm, args) = instruction {
            self.compile_asm_block(instr_val, asm, args)
        } else {
            // These matches all return `Result<(), CompileError>`.  Most actually just return `()`
            // and are wrapped with `Ok(..)`.
            match instruction {
                Instruction::AsmBlock(..) => unreachable!("Handled immediately above."),
                Instruction::BitCast(val, ty) => Ok(self.compile_bitcast(instr_val, val, ty)),
                Instruction::BinaryOp { op, arg1, arg2 } => {
                    Ok(self.compile_binary_op(instr_val, op, arg1, arg2))
                }
                Instruction::Branch(to_block) => Ok(self.compile_branch(to_block)),
                Instruction::Call(func, args) => Ok(self.compile_call(instr_val, func, args)),
                Instruction::CastPtr(val, ty) => Ok(self.compile_cast_ptr(instr_val, val, ty)),
                Instruction::Cmp(pred, lhs_value, rhs_value) => {
                    Ok(self.compile_cmp(instr_val, pred, lhs_value, rhs_value))
                }
                Instruction::ConditionalBranch {
                    cond_value,
                    true_block,
                    false_block,
                } => self.compile_conditional_branch(cond_value, true_block, false_block),
                Instruction::ContractCall {
                    params,
                    coins,
                    asset_id,
                    gas,
                    ..
                } => Ok(self.compile_contract_call(instr_val, params, coins, asset_id, gas)),
                Instruction::FuelVm(fuel_vm_instr) => match fuel_vm_instr {
                    FuelVmInstruction::GetStorageKey => self.compile_get_storage_key(instr_val),
                    FuelVmInstruction::Gtf { index, tx_field_id } => {
                        Ok(self.compile_gtf(instr_val, index, *tx_field_id))
                    }
                    FuelVmInstruction::Log {
                        log_val,
                        log_ty,
                        log_id,
                    } => Ok(self.compile_log(instr_val, log_val, log_ty, log_id)),
                    FuelVmInstruction::ReadRegister(reg) => {
                        Ok(self.compile_read_register(instr_val, reg))
                    }
                    FuelVmInstruction::Revert(revert_val) => {
                        Ok(self.compile_revert(instr_val, revert_val))
                    }
                    FuelVmInstruction::Smo {
                        recipient_and_message,
                        message_size,
                        output_index,
                        coins,
                    } => Ok(self.compile_smo(
                        instr_val,
                        recipient_and_message,
                        message_size,
                        output_index,
                        coins,
                    )),
                    FuelVmInstruction::StateClear {
                        key,
                        number_of_slots,
                    } => self.compile_state_clear(instr_val, key, number_of_slots),
                    FuelVmInstruction::StateLoadQuadWord {
                        load_val,
                        key,
                        number_of_slots,
                    } => self.compile_state_access_quad_word(
                        instr_val,
                        load_val,
                        key,
                        number_of_slots,
                        StateAccessType::Read,
                    ),
                    FuelVmInstruction::StateLoadWord(key) => {
                        self.compile_state_load_word(instr_val, key)
                    }
                    FuelVmInstruction::StateStoreQuadWord {
                        stored_val,
                        key,
                        number_of_slots,
                    } => self.compile_state_access_quad_word(
                        instr_val,
                        stored_val,
                        key,
                        number_of_slots,
                        StateAccessType::Write,
                    ),
                    FuelVmInstruction::StateStoreWord { stored_val, key } => {
                        self.compile_state_store_word(instr_val, stored_val, key)
                    }
                },
                Instruction::GetElemPtr {
                    base,
                    elem_ptr_ty,
                    indices,
                } => self.compile_get_elem_ptr(instr_val, base, elem_ptr_ty, indices),
                Instruction::GetLocal(local_var) => {
                    Ok(self.compile_get_local(instr_val, local_var))
                }
                Instruction::IntToPtr(val, _) => Ok(self.compile_int_to_ptr(instr_val, val)),
                Instruction::Load(src_val) => self.compile_load(instr_val, src_val),
                Instruction::MemCopyBytes {
                    dst_val_ptr,
                    src_val_ptr,
                    byte_len,
                } => {
                    Ok(self.compile_mem_copy_bytes(instr_val, dst_val_ptr, src_val_ptr, *byte_len))
                }
                Instruction::MemCopyVal {
                    dst_val_ptr,
                    src_val_ptr,
                } => Ok(self.compile_mem_copy_val(instr_val, dst_val_ptr, src_val_ptr)),
                Instruction::Nop => Ok(()),
                Instruction::PtrToInt(ptr_val, int_ty) => {
                    Ok(self.compile_ptr_to_int(instr_val, ptr_val, int_ty))
                }
                Instruction::Ret(ret_val, ty) => Ok(if func_is_entry {
                    self.compile_ret_from_entry(instr_val, ret_val, ty)
                } else {
                    self.compile_ret_from_call(instr_val, ret_val)
                }),
                Instruction::Store {
                    dst_val_ptr,
                    stored_val,
                } => self.compile_store(instr_val, dst_val_ptr, stored_val),
            }
            .into()
        }
    }

    fn compile_asm_block(
        &mut self,
        instr_val: &Value,
        asm: &AsmBlock,
        asm_args: &[AsmArg],
    ) -> CompileResult<()> {
        let mut warnings: Vec<CompileWarning> = Vec::new();
        let mut errors: Vec<CompileError> = Vec::new();
        let mut inline_reg_map = HashMap::new();
        let mut inline_ops = Vec::new();
        for AsmArg { name, initializer } in asm_args {
            assert_or_warn!(
                ConstantRegister::parse_register_name(name.as_str()).is_none(),
                warnings,
                name.span().clone(),
                Warning::ShadowingReservedRegister {
                    reg_name: name.clone()
                }
            );
            let arg_reg = match initializer {
                Some(init_val) => {
                    let init_val_reg = self.value_to_register(init_val);
                    match init_val_reg {
                        VirtualRegister::Virtual(_) => init_val_reg,
                        VirtualRegister::Constant(_) => {
                            let const_copy = self.reg_seqr.next();
                            inline_ops.push(Op {
                                opcode: Either::Left(VirtualOp::MOVE(
                                    const_copy.clone(),
                                    init_val_reg,
                                )),
                                comment: "copy const asm init to GP reg".into(),
                                owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
                            });
                            const_copy
                        }
                    }
                }
                None => self.reg_seqr.next(),
            };
            inline_reg_map.insert(name.as_str(), arg_reg);
        }

        let realize_register = |reg_name: &str| {
            inline_reg_map.get(reg_name).cloned().or_else(|| {
                ConstantRegister::parse_register_name(reg_name).map(VirtualRegister::Constant)
            })
        };

        // For each opcode in the asm expression, attempt to parse it into an opcode and
        // replace references to the above registers with the newly allocated ones.
        let asm_block = asm.get_content(self.context);
        for op in &asm_block.body {
            let replaced_registers = op
                .args
                .iter()
                .map(|reg_name| -> Result<_, CompileError> {
                    realize_register(reg_name.as_str()).ok_or_else(|| {
                        CompileError::UnknownRegister {
                            span: reg_name.span(),
                            initialized_registers: inline_reg_map
                                .keys()
                                .copied()
                                .collect::<Vec<_>>()
                                .join("\n"),
                        }
                    })
                })
                .filter_map(|res| match res {
                    Err(e) => {
                        errors.push(e);
                        None
                    }
                    Ok(o) => Some(o),
                })
                .collect::<Vec<VirtualRegister>>();

            // Parse the actual op and registers.
            let op_span = self
                .md_mgr
                .md_to_span(self.context, op.metadata)
                .unwrap_or_else(Self::empty_span);
            let opcode = check!(
                Op::parse_opcode(
                    &op.name,
                    &replaced_registers,
                    &op.immediate,
                    op_span.clone(),
                ),
                return err(warnings, errors),
                warnings,
                errors
            );

            inline_ops.push(Op {
                opcode: either::Either::Left(opcode),
                comment: "asm block".into(),
                owning_span: Some(op_span),
            });
        }

        // Now, load the designated asm return register into the desired return register, but only
        // if it was named.
        if let Some(ret_reg_name) = &asm_block.return_name {
            // Lookup and replace the return register.
            let ret_reg = match realize_register(ret_reg_name.as_str()) {
                Some(reg) => reg,
                None => {
                    errors.push(CompileError::UnknownRegister {
                        initialized_registers: inline_reg_map
                            .keys()
                            .map(|name| name.to_string())
                            .collect::<Vec<_>>()
                            .join("\n"),
                        span: ret_reg_name.span(),
                    });
                    return err(warnings, errors);
                }
            };
            let instr_reg = self.reg_seqr.next();
            inline_ops.push(Op {
                opcode: Either::Left(VirtualOp::MOVE(instr_reg.clone(), ret_reg)),
                comment: "return value from inline asm".into(),
                owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
            });
            self.reg_map.insert(*instr_val, instr_reg);
        }

        self.cur_bytecode.append(&mut inline_ops);

        ok((), warnings, errors)
    }

    fn compile_bitcast(&mut self, instr_val: &Value, bitcast_val: &Value, to_type: &Type) {
        let val_reg = self.value_to_register(bitcast_val);
        let reg = if to_type.is_bool(self.context) {
            // This may not be necessary if we just treat a non-zero value as 'true'.
            let res_reg = self.reg_seqr.next();
            self.cur_bytecode.push(Op {
                opcode: Either::Left(VirtualOp::EQ(
                    res_reg.clone(),
                    val_reg,
                    VirtualRegister::Constant(ConstantRegister::Zero),
                )),
                comment: "convert to inversed boolean".into(),
                owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
            });
            self.cur_bytecode.push(Op {
                opcode: Either::Left(VirtualOp::XORI(
                    res_reg.clone(),
                    res_reg.clone(),
                    VirtualImmediate12 { value: 1 },
                )),
                comment: "invert boolean".into(),
                owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
            });
            res_reg
        } else {
            // This is a no-op, although strictly speaking Unit should probably be compiled as
            // a zero.
            val_reg
        };
        self.reg_map.insert(*instr_val, reg);
    }

    fn compile_binary_op(
        &mut self,
        instr_val: &Value,
        op: &BinaryOpKind,
        arg1: &Value,
        arg2: &Value,
    ) {
        let val1_reg = self.value_to_register(arg1);
        let val2_reg = self.value_to_register(arg2);
        let res_reg = self.reg_seqr.next();
        let opcode = match op {
            BinaryOpKind::Add => Either::Left(VirtualOp::ADD(res_reg.clone(), val1_reg, val2_reg)),
            BinaryOpKind::Sub => Either::Left(VirtualOp::SUB(res_reg.clone(), val1_reg, val2_reg)),
            BinaryOpKind::Mul => Either::Left(VirtualOp::MUL(res_reg.clone(), val1_reg, val2_reg)),
            BinaryOpKind::Div => Either::Left(VirtualOp::DIV(res_reg.clone(), val1_reg, val2_reg)),
        };
        self.cur_bytecode.push(Op {
            opcode,
            comment: String::new(),
            owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
        });

        self.reg_map.insert(*instr_val, res_reg);
    }

    fn compile_branch(&mut self, to_block: &BranchToWithArgs) {
        self.compile_branch_to_phi_value(to_block);

        let label = self.block_to_label(&to_block.block);
        self.cur_bytecode.push(Op::jump_to_label(label));
    }

    fn compile_cast_ptr(&mut self, instr_val: &Value, val: &Value, _ptr_ty: &Type) {
        // The val type and ptr_ty should be validated/verified already by IR.
        let val_reg = self.value_to_register(val);
        self.reg_map.insert(*instr_val, val_reg);
    }

    fn compile_cmp(
        &mut self,
        instr_val: &Value,
        pred: &Predicate,
        lhs_value: &Value,
        rhs_value: &Value,
    ) {
        let lhs_reg = self.value_to_register(lhs_value);
        let rhs_reg = self.value_to_register(rhs_value);
        let res_reg = self.reg_seqr.next();
        match pred {
            Predicate::Equal => {
                self.cur_bytecode.push(Op {
                    opcode: Either::Left(VirtualOp::EQ(res_reg.clone(), lhs_reg, rhs_reg)),
                    comment: String::new(),
                    owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
                });
            }
        }
        self.reg_map.insert(*instr_val, res_reg);
    }

    fn compile_conditional_branch(
        &mut self,
        cond_value: &Value,
        true_block: &BranchToWithArgs,
        false_block: &BranchToWithArgs,
    ) -> Result<(), CompileError> {
        if true_block.block == false_block.block && true_block.block.num_args(self.context) > 0 {
            return Err(CompileError::Internal(
                "Cannot compile CBR with both branches going to same dest block",
                self.md_mgr
                    .val_to_span(self.context, *cond_value)
                    .unwrap_or_else(Self::empty_span),
            ));
        }
        self.compile_branch_to_phi_value(true_block);
        self.compile_branch_to_phi_value(false_block);

        let cond_reg = self.value_to_register(cond_value);

        let true_label = self.block_to_label(&true_block.block);
        self.cur_bytecode
            .push(Op::jump_if_not_zero(cond_reg, true_label));

        let false_label = self.block_to_label(&false_block.block);
        self.cur_bytecode.push(Op::jump_to_label(false_label));

        Ok(())
    }

    fn compile_branch_to_phi_value(&mut self, to_block: &BranchToWithArgs) {
        for (i, param) in to_block.args.iter().enumerate() {
            // We only need a MOVE here if param is actually assigned to a register
            if let Some(local_reg) = self.opt_value_to_register(param) {
                let phi_reg =
                    self.value_to_register(&to_block.block.get_arg(self.context, i).unwrap());
                self.cur_bytecode.push(Op::register_move(
                    phi_reg,
                    local_reg,
                    "parameter from branch to block argument",
                    None,
                ));
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_contract_call(
        &mut self,
        instr_val: &Value,
        params: &Value,
        coins: &Value,
        asset_id: &Value,
        gas: &Value,
    ) {
        let ra_pointer = self.value_to_register(params);
        let coins_register = self.value_to_register(coins);
        let asset_id_register = self.value_to_register(asset_id);
        let gas_register = self.value_to_register(gas);

        self.cur_bytecode.push(Op {
            opcode: Either::Left(VirtualOp::CALL(
                ra_pointer,
                coins_register,
                asset_id_register,
                gas_register,
            )),
            comment: "call external contract".into(),
            owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
        });

        // now, move the return value of the contract call to the return register.
        // TODO validate RETL matches the expected type (this is a comment from the old codegen)
        let instr_reg = self.reg_seqr.next();
        self.cur_bytecode.push(Op::register_move(
            instr_reg.clone(),
            VirtualRegister::Constant(ConstantRegister::ReturnValue),
            "save call result",
            None,
        ));
        self.reg_map.insert(*instr_val, instr_reg);
    }

    fn compile_get_storage_key(&mut self, instr_val: &Value) -> Result<(), CompileError> {
        let state_idx = self.md_mgr.val_to_storage_key(self.context, *instr_val);
        let instr_span = self.md_mgr.val_to_span(self.context, *instr_val);

        let storage_slot_to_hash = match state_idx {
            Some(state_idx) => {
                format!(
                    "{}{}",
                    sway_utils::constants::STORAGE_DOMAIN_SEPARATOR,
                    state_idx
                )
            }
            None => {
                return Err(CompileError::Internal(
                    "State index for __get_storage_key is not available as a metadata",
                    instr_span.unwrap_or_else(Self::empty_span),
                ));
            }
        };

        let hashed_storage_slot = Hasher::hash(storage_slot_to_hash);

        let data_id = self.data_section.insert_data_value(Entry::new_byte_array(
            (*hashed_storage_slot).to_vec(),
            None,
            None,
        ));

        // Allocate a register for it, and a load instruction.
        let reg = self.reg_seqr.next();

        self.cur_bytecode.push(Op {
            opcode: either::Either::Left(VirtualOp::LWDataId(reg.clone(), data_id)),
            comment: "literal instantiation".into(),
            owning_span: instr_span,
        });
        self.reg_map.insert(*instr_val, reg);

        Ok(())
    }

    fn compile_get_elem_ptr(
        &mut self,
        instr_val: &Value,
        base_val: &Value,
        _elem_ty: &Type,
        indices: &[Value],
    ) -> Result<(), CompileError> {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        let base_type = base_val
            .get_type(self.context)
            .and_then(|ty| ty.get_inner_type(self.context))
            .ok_or_else(|| {
                CompileError::Internal(
                    "Failed to get type of base value for GEP.",
                    owning_span
                        .as_ref()
                        .cloned()
                        .unwrap_or_else(|| Span::dummy()),
                )
            })?;

        // A utility lambda to unwrap Values which must be constant uints.
        let unwrap_constant_uint = |idx_val: &Value| {
            idx_val
                .get_constant(self.context)
                .and_then(|idx_const| {
                    if let ConstantValue::Uint(idx) = idx_const.value {
                        Some(idx as usize)
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    CompileError::Internal(
                        "Failed to convert struct index from constant to integer.",
                        sway_types::span::Span::dummy(),
                    )
                })
        };

        // The indices for a GEP are Values.  For structs and unions they are always constant
        // uints.  For arrays they may be any value expression.  So we need to take all the
        // individual offsets and add them up.
        //
        // Ideally, most of the time, there will only be a single constant struct index.  And often
        // they will be zero, making the GEP a no-op.  But if not we need to add the non-constant
        // values together.
        //
        // Eventually this can be optimised with an ASM opt pass which can combine constant
        // ADD/ADDIs together.  Then we could just emit an ADD for every index at this stage.  But
        // until then we can keep track of the constant values and add them once.

        let base_reg = self.value_to_register(base_val);
        let (base_reg, const_offs, _) =
            indices
                .iter()
                .fold(Ok((base_reg, 0, base_type)), |acc, idx_val| {
                    // So we're folding to a Result, as unwrapping the constants can fail.
                    acc.and_then(|(reg, offs, elem_ty)| {
                        // If we find a constant index then we add its offset to `offs`.  Otherwise we grab
                        // its value, which should be compiled already, and add it to reg.
                        if elem_ty.is_struct(self.context) {
                            // For structs the index must be a const uint.
                            unwrap_constant_uint(idx_val).map(|idx| {
                                let field_types = elem_ty.get_field_types(self.context);
                                let field_type = field_types[idx];
                                let field_offs_in_bytes = field_types
                                    .iter()
                                    .take(idx)
                                    .map(|field_ty| ir_type_size_in_bytes(self.context, field_ty))
                                    .sum::<u64>();
                                (reg, offs + field_offs_in_bytes, field_type)
                            })
                        } else if elem_ty.is_union(self.context) {
                            // For unions the index must also be a const uint.
                            unwrap_constant_uint(idx_val).map(|idx| {
                                let field_type = elem_ty.get_field_types(self.context)[idx];
                                let union_size_in_bytes =
                                    ir_type_size_in_bytes(self.context, &elem_ty);
                                let field_size_in_bytes =
                                    ir_type_size_in_bytes(self.context, &field_type);

                                // The union fields are at offset (union_size - variant_size) due to left padding.
                                (
                                    reg,
                                    offs + union_size_in_bytes - field_size_in_bytes,
                                    field_type,
                                )
                            })
                        } else if elem_ty.is_array(self.context) {
                            // For arrays the index is a value.  We need to fetch it and add it to
                            // the base.
                            let index_reg =
                                self.opt_value_to_register(idx_val).ok_or_else(|| {
                                    CompileError::Internal(
                                        "Array index has not been compiled before use.",
                                        sway_types::span::Span::dummy(),
                                    )
                                })?;
                            let offset_reg = self.reg_seqr.next();

                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::ADD(
                                    offset_reg.clone(),
                                    reg,
                                    index_reg,
                                )),
                                comment: "get offset to element".into(),
                                owning_span: owning_span.clone(),
                            });
                            let member_type =
                                elem_ty.get_array_elem_type(self.context).ok_or_else(|| {
                                    CompileError::Internal(
                                        "Can't get array elem type for GEP.",
                                        sway_types::span::Span::dummy(),
                                    )
                                })?;

                            Ok((offset_reg, offs, member_type))
                        } else {
                            Err(CompileError::Internal(
                                "Cannot get element offset in non-aggregate.",
                                sway_types::span::Span::dummy(),
                            ))
                        }
                    })
                })?;

        if const_offs == 0 {
            // No need to add anything.
            self.reg_map.insert(*instr_val, base_reg);
        } else {
            let instr_reg = self.reg_seqr.next();
            if const_offs > compiler_constants::TWELVE_BITS {
                let offset_data_id = self
                    .data_section
                    .insert_data_value(Entry::new_word(const_offs, None, None));
                let offset_reg = self.reg_seqr.next();
                self.cur_bytecode.push(Op {
                    opcode: Either::Left(VirtualOp::LWDataId(offset_reg.clone(), offset_data_id)),
                    owning_span: owning_span.clone(),
                    comment: "loading offset for GEP".into(),
                });
                self.cur_bytecode.push(Op {
                    opcode: Either::Left(VirtualOp::ADD(instr_reg.clone(), base_reg, offset_reg)),
                    comment: "get offset to element".into(),
                    owning_span,
                });
            } else {
                self.cur_bytecode.push(Op {
                    opcode: Either::Left(VirtualOp::ADDI(
                        instr_reg.clone(),
                        base_reg,
                        VirtualImmediate12 {
                            value: const_offs as u16,
                        },
                    )),
                    comment: "get offset to element".into(),
                    owning_span,
                });
            }

            self.reg_map.insert(*instr_val, instr_reg);
        }

        Ok(())
    }

    fn compile_get_local(&mut self, instr_val: &Value, local_var: &LocalVar) {
        // `get_local` is like a `load` except the value isn't dereferenced.
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        match self.ptr_map.get(local_var) {
            None => unimplemented!("BUG? Uninitialised pointer."),
            Some(storage) => match storage.clone() {
                Storage::Data(_data_id) => {
                    // Not sure if we'll ever need this.
                    unimplemented!("TODO get_ptr() into the data section.");
                }
                Storage::Stack(word_offs) => {
                    let offset_in_bytes = word_offs * 8;
                    let instr_reg = self.reg_seqr.next();
                    if offset_in_bytes > compiler_constants::TWELVE_BITS {
                        self.number_to_reg(offset_in_bytes, &instr_reg, owning_span.clone());
                        self.cur_bytecode.push(Op {
                            opcode: either::Either::Left(VirtualOp::ADD(
                                instr_reg.clone(),
                                self.locals_base_reg().clone(),
                                instr_reg.clone(),
                            )),
                            comment: "get offset reg for get_ptr".into(),
                            owning_span,
                        });
                    } else {
                        self.cur_bytecode.push(Op {
                            opcode: either::Either::Left(VirtualOp::ADDI(
                                instr_reg.clone(),
                                self.locals_base_reg().clone(),
                                VirtualImmediate12 {
                                    value: (offset_in_bytes) as u16,
                                },
                            )),
                            comment: "get offset reg for get_ptr".into(),
                            owning_span,
                        });
                    }
                    self.reg_map.insert(*instr_val, instr_reg);
                }
            },
        }
    }

    fn compile_gtf(&mut self, instr_val: &Value, index: &Value, tx_field_id: u64) {
        let instr_reg = self.reg_seqr.next();
        let index_reg = self.value_to_register(index);
        self.cur_bytecode.push(Op {
            opcode: either::Either::Left(VirtualOp::GTF(
                instr_reg.clone(),
                index_reg,
                VirtualImmediate12 {
                    value: tx_field_id as u16,
                },
            )),
            comment: "get transaction field".into(),
            owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
        });
        self.reg_map.insert(*instr_val, instr_reg);
    }

    fn compile_int_to_ptr(&mut self, instr_val: &Value, int_to_ptr_val: &Value) {
        let val_reg = self.value_to_register(int_to_ptr_val);
        self.reg_map.insert(*instr_val, val_reg);
    }

    fn compile_load(&mut self, instr_val: &Value, src_val: &Value) -> Result<(), CompileError> {
        // XXX GEPs now have byte offsets so we need to change this.
        let local_var = self.resolve_ptr(src_val)?;
        let instr_reg = self.reg_seqr.next();
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        match self.ptr_map.get(&local_var) {
            None => unimplemented!("BUG? Uninitialised pointer."),
            Some(storage) => match storage.clone() {
                Storage::Data(data_id) => {
                    self.cur_bytecode.push(Op {
                        opcode: Either::Left(VirtualOp::LWDataId(instr_reg.clone(), data_id)),
                        comment: "load constant".into(),
                        owning_span,
                    });
                }
                Storage::Stack(word_offs) => {
                    let base_reg = self.locals_base_reg().clone();
                    if self.is_copy_type(&local_var.get_type(self.context)) {
                        // Value can fit in a register, so we load the value.
                        if word_offs > compiler_constants::TWELVE_BITS {
                            let offs_reg = self.reg_seqr.next();
                            self.number_to_reg(
                                word_offs * 8, // Base reg for LW is in bytes
                                &offs_reg,
                                owning_span.clone(),
                            );
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::ADD(
                                    offs_reg.clone(),
                                    base_reg,
                                    offs_reg.clone(),
                                )),
                                comment: "absolute offset for load".into(),
                                owning_span: owning_span.clone(),
                            });
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::LW(
                                    instr_reg.clone(),
                                    offs_reg.clone(),
                                    VirtualImmediate12 { value: 0 },
                                )),
                                comment: "load value".into(),
                                owning_span,
                            });
                        } else {
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::LW(
                                    instr_reg.clone(),
                                    base_reg,
                                    VirtualImmediate12 {
                                        value: word_offs as u16,
                                    },
                                )),
                                comment: "load value".into(),
                                owning_span,
                            });
                        }
                    } else {
                        // Value too big for a register, so we return the memory offset.  This is
                        // what LW to the data section does, via LWDataId.
                        let word_offs = word_offs * 8;
                        if word_offs > compiler_constants::TWELVE_BITS {
                            let offs_reg = self.reg_seqr.next();
                            self.number_to_reg(word_offs, &offs_reg, owning_span.clone());
                            self.cur_bytecode.push(Op {
                                opcode: either::Either::Left(VirtualOp::ADD(
                                    instr_reg.clone(),
                                    base_reg,
                                    offs_reg,
                                )),
                                comment: "load address".into(),
                                owning_span,
                            });
                        } else {
                            self.cur_bytecode.push(Op {
                                opcode: either::Either::Left(VirtualOp::ADDI(
                                    instr_reg.clone(),
                                    base_reg,
                                    VirtualImmediate12 {
                                        value: word_offs as u16,
                                    },
                                )),
                                comment: "load address".into(),
                                owning_span,
                            });
                        }
                    }
                }
            },
        }
        self.reg_map.insert(*instr_val, instr_reg);
        Ok(())
    }

    fn compile_mem_copy_bytes(
        &mut self,
        instr_val: &Value,
        dst_val_ptr: &Value,
        src_val_ptr: &Value,
        byte_len: u64,
    ) {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);

        let dst_reg = self.value_to_register(dst_val_ptr);
        let src_reg = self.value_to_register(src_val_ptr);

        let len_reg = self.reg_seqr.next();
        self.cur_bytecode.push(Op {
            opcode: Either::Left(VirtualOp::MOVI(
                len_reg.clone(),
                VirtualImmediate18 {
                    value: byte_len as u32,
                },
            )),
            comment: "get length for mcp".into(),
            owning_span: owning_span.clone(),
        });

        self.cur_bytecode.push(Op {
            opcode: Either::Left(VirtualOp::MCP(dst_reg, src_reg, len_reg)),
            comment: "copy memory with mem_copy".into(),
            owning_span,
        });
    }

    fn compile_mem_copy_val(
        &mut self,
        instr_val: &Value,
        dst_val_ptr: &Value,
        src_val_ptr: &Value,
    ) {
        let dst_ty = dst_val_ptr
            .get_type(self.context)
            .and_then(|ptr_ty| ptr_ty.get_inner_type(self.context))
            .expect("mem_copy dst type must be known and a pointer.");
        let byte_len = ir_type_size_in_bytes(self.context, &dst_ty);
        self.compile_mem_copy_bytes(instr_val, dst_val_ptr, src_val_ptr, byte_len)
    }

    fn compile_log(&mut self, instr_val: &Value, log_val: &Value, log_ty: &Type, log_id: &Value) {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        let log_val_reg = self.value_to_register(log_val);
        let log_id_reg = self.value_to_register(log_id);

        if self.is_copy_type(log_ty) {
            self.cur_bytecode.push(Op {
                owning_span,
                opcode: Either::Left(VirtualOp::LOG(
                    log_val_reg,
                    log_id_reg,
                    VirtualRegister::Constant(ConstantRegister::Zero),
                    VirtualRegister::Constant(ConstantRegister::Zero),
                )),
                comment: "".into(),
            });
        } else {
            // If the type not a reference type then we use LOGD to log the data. First put the
            // size into the data section, then add a LW to get it, then add a LOGD which uses
            // it.
            let size_reg = self.reg_seqr.next();
            let size_in_bytes = ir_type_size_in_bytes(self.context, log_ty);
            let size_data_id =
                self.data_section
                    .insert_data_value(Entry::new_word(size_in_bytes, None, None));

            self.cur_bytecode.push(Op {
                opcode: Either::Left(VirtualOp::LWDataId(size_reg.clone(), size_data_id)),
                owning_span: owning_span.clone(),
                comment: "loading size for LOGD".into(),
            });
            self.cur_bytecode.push(Op {
                owning_span,
                opcode: Either::Left(VirtualOp::LOGD(
                    VirtualRegister::Constant(ConstantRegister::Zero),
                    log_id_reg,
                    log_val_reg,
                    size_reg,
                )),
                comment: "".into(),
            });
        }
    }

    fn compile_ptr_to_int(&mut self, instr_val: &Value, ptr_val: &Value, _int_ty: &Type) {
        // The int_ty should be validated/verified already by IR.
        let reg = self.value_to_register(ptr_val);
        self.reg_map.insert(*instr_val, reg);
    }

    fn compile_read_register(&mut self, instr_val: &Value, reg: &sway_ir::Register) {
        let instr_reg = self.reg_seqr.next();
        self.cur_bytecode.push(Op {
            opcode: Either::Left(VirtualOp::MOVE(
                instr_reg.clone(),
                VirtualRegister::Constant(match reg {
                    sway_ir::Register::Of => ConstantRegister::Overflow,
                    sway_ir::Register::Pc => ConstantRegister::ProgramCounter,
                    sway_ir::Register::Ssp => ConstantRegister::StackStartPointer,
                    sway_ir::Register::Sp => ConstantRegister::StackPointer,
                    sway_ir::Register::Fp => ConstantRegister::FramePointer,
                    sway_ir::Register::Hp => ConstantRegister::HeapPointer,
                    sway_ir::Register::Error => ConstantRegister::Error,
                    sway_ir::Register::Ggas => ConstantRegister::GlobalGas,
                    sway_ir::Register::Cgas => ConstantRegister::ContextGas,
                    sway_ir::Register::Bal => ConstantRegister::Balance,
                    sway_ir::Register::Is => ConstantRegister::InstructionStart,
                    sway_ir::Register::Ret => ConstantRegister::ReturnValue,
                    sway_ir::Register::Retl => ConstantRegister::ReturnLength,
                    sway_ir::Register::Flag => ConstantRegister::Flags,
                }),
            )),
            comment: "move register into abi function".to_owned(),
            owning_span: self.md_mgr.val_to_span(self.context, *instr_val),
        });

        self.reg_map.insert(*instr_val, instr_reg);
    }

    fn compile_ret_from_entry(&mut self, instr_val: &Value, ret_val: &Value, ret_type: &Type) {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        if ret_type.is_unit(self.context) {
            // Unit returns should always be zero, although because they can be omitted from
            // functions, the register is sometimes uninitialized. Manually return zero in this
            // case.
            self.cur_bytecode.push(Op {
                opcode: Either::Left(VirtualOp::RET(VirtualRegister::Constant(
                    ConstantRegister::Zero,
                ))),
                owning_span,
                comment: "returning unit as zero".into(),
            });
        } else {
            let ret_reg = self.value_to_register(ret_val);

            if self.is_copy_type(ret_type) {
                self.cur_bytecode.push(Op {
                    owning_span,
                    opcode: Either::Left(VirtualOp::RET(ret_reg)),
                    comment: "".into(),
                });
            } else {
                // If the type is not a copy type then we use RETD to return data.
                let size_reg = self.reg_seqr.next();
                if ret_type.is_slice(self.context) {
                    // If this is a slice then return what it points to.
                    self.cur_bytecode.push(Op {
                        opcode: Either::Left(VirtualOp::LW(
                            size_reg.clone(),
                            ret_reg.clone(),
                            VirtualImmediate12 { value: 1 },
                        )),
                        owning_span: owning_span.clone(),
                        comment: "load size of returned slice".into(),
                    });
                    self.cur_bytecode.push(Op {
                        opcode: Either::Left(VirtualOp::LW(
                            ret_reg.clone(),
                            ret_reg.clone(),
                            VirtualImmediate12 { value: 0 },
                        )),
                        owning_span: owning_span.clone(),
                        comment: "load ptr of returned slice".into(),
                    });
                } else {
                    // First put the size into the data section, then add a LW to get it,
                    // then add a RETD which uses it.
                    let size_in_bytes = ir_type_size_in_bytes(self.context, ret_type);
                    let size_data_id = self.data_section.insert_data_value(Entry::new_word(
                        size_in_bytes,
                        None,
                        None,
                    ));

                    self.cur_bytecode.push(Op {
                        opcode: Either::Left(VirtualOp::LWDataId(size_reg.clone(), size_data_id)),
                        owning_span: owning_span.clone(),
                        comment: "load size of returned ref".into(),
                    });
                }
                self.cur_bytecode.push(Op {
                    owning_span,
                    opcode: Either::Left(VirtualOp::RETD(ret_reg, size_reg)),
                    comment: "".into(),
                });
            }
        }
    }

    fn compile_revert(&mut self, instr_val: &Value, revert_val: &Value) {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        let revert_reg = self.value_to_register(revert_val);

        self.cur_bytecode.push(Op {
            owning_span,
            opcode: Either::Left(VirtualOp::RVRT(revert_reg)),
            comment: "".into(),
        });
    }

    fn compile_smo(
        &mut self,
        instr_val: &Value,
        recipient_and_message: &Value,
        message_size: &Value,
        output_index: &Value,
        coins: &Value,
    ) {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        let recipient_and_message_reg = self.value_to_register(recipient_and_message);
        let message_size_reg = self.value_to_register(message_size);
        let output_index_reg = self.value_to_register(output_index);
        let coins_reg = self.value_to_register(coins);

        self.cur_bytecode.push(Op {
            owning_span,
            opcode: Either::Left(VirtualOp::SMO(
                recipient_and_message_reg,
                message_size_reg,
                output_index_reg,
                coins_reg,
            )),
            comment: "".into(),
        });
    }

    fn offset_reg(
        &mut self,
        base_reg: &VirtualRegister,
        offset_in_bytes: u64,
        span: Option<Span>,
    ) -> VirtualRegister {
        let offset_reg = self.reg_seqr.next();
        if offset_in_bytes > compiler_constants::TWELVE_BITS {
            let offs_reg = self.reg_seqr.next();
            self.number_to_reg(offset_in_bytes, &offs_reg, span.clone());
            self.cur_bytecode.push(Op {
                opcode: either::Either::Left(VirtualOp::ADD(
                    offset_reg.clone(),
                    base_reg.clone(),
                    offs_reg,
                )),
                comment: "get offset".into(),
                owning_span: span,
            });
        } else {
            self.cur_bytecode.push(Op {
                opcode: either::Either::Left(VirtualOp::ADDI(
                    offset_reg.clone(),
                    base_reg.clone(),
                    VirtualImmediate12 {
                        value: offset_in_bytes as u16,
                    },
                )),
                comment: "get offset".into(),
                owning_span: span,
            });
        }

        offset_reg
    }

    fn compile_state_clear(
        &mut self,
        instr_val: &Value,
        key: &Value,
        number_of_slots: &Value,
    ) -> Result<(), CompileError> {
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        let key_var = self.resolve_ptr(key)?;
        let key_reg = match self.ptr_map.get(&key_var) {
            Some(Storage::Stack(key_offset)) => {
                let base_reg = self.locals_base_reg().clone();
                let key_offset_in_bytes = key_offset * 8;
                self.offset_reg(&base_reg, key_offset_in_bytes, owning_span.clone())
            }
            _ => unreachable!("Unexpected storage locations for key and val"),
        };

        // Capture the status of whether the slot was set before calling this instruction.
        let was_slot_set_reg = self.reg_seqr.next();

        // Number of slots to be cleared
        let number_of_slots_reg = self.value_to_register(number_of_slots);

        self.cur_bytecode.push(Op {
            opcode: Either::Left(VirtualOp::SCWQ(
                key_reg,
                was_slot_set_reg.clone(),
                number_of_slots_reg,
            )),
            comment: "clear a sequence of storage slots".into(),
            owning_span,
        });

        self.reg_map.insert(*instr_val, was_slot_set_reg);

        Ok(())
    }

    fn compile_state_access_quad_word(
        &mut self,
        instr_val: &Value,
        val: &Value,
        key: &Value,
        number_of_slots: &Value,
        access_type: StateAccessType,
    ) -> Result<(), CompileError> {
        // Make sure that both val and key are pointers to B256.
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        let key_var = self.resolve_ptr(key)?;
        let val_reg = if matches!(
            val.get_instruction(self.context),
            Some(Instruction::IntToPtr(..))
        ) {
            match self.reg_map.get(val) {
                Some(vreg) => vreg.clone(),
                None => unreachable!("int_to_ptr instruction doesn't have vreg mapped"),
            }
        } else {
            let local_val = self.resolve_ptr(val)?;
            match self.ptr_map.get(&local_val) {
                Some(Storage::Stack(val_offset)) => {
                    let base_reg = self.locals_base_reg().clone();
                    let val_offset_in_bytes = val_offset * 8;
                    self.offset_reg(&base_reg, val_offset_in_bytes, owning_span.clone())
                }
                _ => unreachable!("Unexpected storage locations for key and val"),
            }
        };

        let key_reg = match self.ptr_map.get(&key_var) {
            Some(Storage::Stack(key_offset)) => {
                let base_reg = self.locals_base_reg().clone();
                let key_offset_in_bytes = key_offset * 8;
                self.offset_reg(&base_reg, key_offset_in_bytes, owning_span.clone())
            }
            _ => unreachable!("Unexpected storage locations for key and val"),
        };

        // capture the status of whether the slot was set before calling this instruction
        let was_slot_set_reg = self.reg_seqr.next();

        // Number of slots to be read or written
        let number_of_slots_reg = self.value_to_register(number_of_slots);

        self.cur_bytecode.push(Op {
            opcode: Either::Left(match access_type {
                StateAccessType::Read => VirtualOp::SRWQ(
                    val_reg,
                    was_slot_set_reg.clone(),
                    key_reg,
                    number_of_slots_reg,
                ),
                StateAccessType::Write => VirtualOp::SWWQ(
                    key_reg,
                    was_slot_set_reg.clone(),
                    val_reg,
                    number_of_slots_reg,
                ),
            }),
            comment: "access a sequence of storage slots".into(),
            owning_span,
        });

        self.reg_map.insert(*instr_val, was_slot_set_reg);

        Ok(())
    }

    fn compile_state_load_word(
        &mut self,
        instr_val: &Value,
        key: &Value,
    ) -> Result<(), CompileError> {
        let key_var = self.resolve_ptr(key)?;
        let load_reg = self.reg_seqr.next();
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);

        // capture the status of whether the slot was set before calling this instruction
        let was_slot_set_reg = self.reg_seqr.next();

        match self.ptr_map.get(&key_var) {
            Some(Storage::Stack(key_offset)) => {
                let base_reg = self.locals_base_reg().clone();
                let key_offset_in_bytes = key_offset * 8;

                let key_reg = self.offset_reg(&base_reg, key_offset_in_bytes, owning_span.clone());

                self.cur_bytecode.push(Op {
                    opcode: Either::Left(VirtualOp::SRW(
                        load_reg.clone(),
                        was_slot_set_reg,
                        key_reg,
                    )),
                    comment: "single word state access".into(),
                    owning_span,
                });
            }
            _ => unreachable!("Unexpected storage location for key"),
        }

        self.reg_map.insert(*instr_val, load_reg);

        Ok(())
    }

    fn compile_state_store_word(
        &mut self,
        instr_val: &Value,
        store_val: &Value,
        key: &Value,
    ) -> Result<(), CompileError> {
        let store_reg = self.value_to_register(store_val);
        let key_var = self.resolve_ptr(key)?;

        // Capture the status of whether the slot was set before calling this instruction.
        let was_slot_set_reg = self.reg_seqr.next();

        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        match self.ptr_map.get(&key_var) {
            Some(Storage::Stack(key_offset)) => {
                let base_reg = self.locals_base_reg().clone();
                let key_offset_in_bytes = key_offset * 8;

                let key_reg = self.offset_reg(&base_reg, key_offset_in_bytes, owning_span.clone());

                self.cur_bytecode.push(Op {
                    opcode: Either::Left(VirtualOp::SWW(
                        key_reg,
                        was_slot_set_reg.clone(),
                        store_reg,
                    )),
                    comment: "single word state access".into(),
                    owning_span,
                });
            }
            _ => unreachable!("Unexpected storage locations for key and store_val"),
        }

        self.reg_map.insert(*instr_val, was_slot_set_reg);

        Ok(())
    }

    fn compile_store(
        &mut self,
        instr_val: &Value,
        dst_val: &Value,
        stored_val: &Value,
    ) -> Result<(), CompileError> {
        // XXX GEPs now have byte offsets so we need to change this.
        let local_var = self.resolve_ptr(dst_val)?;
        let stored_reg = self.value_to_register(stored_val);
        let owning_span = self.md_mgr.val_to_span(self.context, *instr_val);
        match self.ptr_map.get(&local_var) {
            None => unreachable!("Bug! Trying to store to an unknown pointer."),
            Some(storage) => match storage {
                Storage::Data(_) => unreachable!("BUG! Trying to store to the data section."),
                Storage::Stack(word_offs) => {
                    let word_offs = *word_offs;
                    let store_type = local_var.get_type(self.context);
                    let store_size_in_words =
                        size_bytes_in_words!(ir_type_size_in_bytes(self.context, &store_type));
                    if self.is_copy_type(&store_type) {
                        let base_reg = self.locals_base_reg().clone();

                        // A single word can be stored with SW.
                        let local_var_ty = local_var.get_type(self.context);
                        let is_aggregate_var = local_var_ty.is_array(self.context)
                            || local_var_ty.is_struct(self.context)
                            || local_var_ty.is_union(self.context);

                        let stored_reg = if !is_aggregate_var {
                            // stored_reg is a value.
                            stored_reg
                        } else {
                            // stored_reg is a pointer, even though size is 1.  We need to load it.
                            let tmp_reg = self.reg_seqr.next();
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::LW(
                                    tmp_reg.clone(),
                                    stored_reg,
                                    VirtualImmediate12 { value: 0 },
                                )),
                                comment: "load for store".into(),
                                owning_span: owning_span.clone(),
                            });
                            tmp_reg
                        };
                        if word_offs > compiler_constants::TWELVE_BITS {
                            let offs_reg = self.reg_seqr.next();
                            self.number_to_reg(
                                word_offs * 8, // Base reg for SW is in bytes
                                &offs_reg,
                                owning_span.clone(),
                            );
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::ADD(
                                    offs_reg.clone(),
                                    base_reg,
                                    offs_reg.clone(),
                                )),
                                comment: "store absolute offset".into(),
                                owning_span: owning_span.clone(),
                            });
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::SW(
                                    offs_reg,
                                    stored_reg,
                                    VirtualImmediate12 { value: 0 },
                                )),
                                comment: "store value".into(),
                                owning_span,
                            });
                        } else {
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::SW(
                                    base_reg,
                                    stored_reg,
                                    VirtualImmediate12 {
                                        value: word_offs as u16,
                                    },
                                )),
                                comment: "store value".into(),
                                owning_span,
                            });
                        }
                    } else {
                        let base_reg = self.locals_base_reg().clone();

                        // Bigger than 1 word needs a MCPI.  XXX Or MCP if it's huge.
                        let dest_offs_reg = self.reg_seqr.next();
                        if word_offs * 8 > compiler_constants::TWELVE_BITS {
                            self.number_to_reg(word_offs * 8, &dest_offs_reg, owning_span.clone());
                            self.cur_bytecode.push(Op {
                                opcode: either::Either::Left(VirtualOp::ADD(
                                    dest_offs_reg.clone(),
                                    base_reg,
                                    dest_offs_reg.clone(),
                                )),
                                comment: "get store offset".into(),
                                owning_span: owning_span.clone(),
                            });
                        } else {
                            self.cur_bytecode.push(Op {
                                opcode: either::Either::Left(VirtualOp::ADDI(
                                    dest_offs_reg.clone(),
                                    base_reg,
                                    VirtualImmediate12 {
                                        value: (word_offs * 8) as u16,
                                    },
                                )),
                                comment: "get store offset".into(),
                                owning_span: owning_span.clone(),
                            });
                        }

                        if store_size_in_words * 8 > compiler_constants::TWELVE_BITS {
                            let size_reg = self.reg_seqr.next();
                            self.number_to_reg(
                                store_size_in_words * 8,
                                &size_reg,
                                owning_span.clone(),
                            );
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::MCP(
                                    dest_offs_reg,
                                    stored_reg,
                                    size_reg,
                                )),
                                comment: "store value".into(),
                                owning_span,
                            });
                        } else {
                            self.cur_bytecode.push(Op {
                                opcode: Either::Left(VirtualOp::MCPI(
                                    dest_offs_reg,
                                    stored_reg,
                                    VirtualImmediate12 {
                                        value: (store_size_in_words * 8) as u16,
                                    },
                                )),
                                comment: "store value".into(),
                                owning_span,
                            });
                        }
                    }
                }
            },
        };

        Ok(())
    }

    pub(crate) fn is_copy_type(&self, ty: &Type) -> bool {
        ty.is_unit(self.context) || ty.is_bool(self.context) | ty.is_uint(self.context)
    }

    // This sucks a bit -- we shouldn't be necessarily getting the local var for a pointer.  The
    // pointer is an address, we should just read from it.  The hard bit is knowing whether it's
    // aligned or not, and needs to be aligned.
    fn resolve_ptr(&mut self, ptr_val: &Value) -> Result<LocalVar, CompileError> {
        match ptr_val.get_instruction(self.context) {
            Some(Instruction::GetLocal(local_var)) => Ok(*local_var),
            Some(Instruction::GetElemPtr {
                ..
                //base,
                //elem_ptr_ty,
                //indices,
            }) => todo!(),
            Some(Instruction::CastPtr(local_val, _ty)) => self.resolve_ptr(local_val),

            _otherwise => Err(CompileError::Internal(
                "Destination ptr arg for load/store is not valid.",
                self.md_mgr
                    .val_to_span(self.context, *ptr_val)
                    .unwrap_or_else(Self::empty_span),
            )),
        }
    }

    fn initialise_constant(
        &mut self,
        constant: &Constant,
        config_name: Option<String>,
        span: Option<Span>,
    ) -> (VirtualRegister, Option<DataId>) {
        match &constant.value {
            // Use cheaper $zero or $one registers if possible.
            ConstantValue::Unit | ConstantValue::Bool(false) | ConstantValue::Uint(0)
                if config_name.is_none() =>
            {
                (VirtualRegister::Constant(ConstantRegister::Zero), None)
            }

            ConstantValue::Bool(true) | ConstantValue::Uint(1) if config_name.is_none() => {
                (VirtualRegister::Constant(ConstantRegister::One), None)
            }

            _otherwise => {
                // Get the constant into the namespace.
                let entry = Entry::from_constant(self.context, constant, config_name);
                let data_id = self.data_section.insert_data_value(entry);

                // Allocate a register for it, and a load instruction.
                let reg = self.reg_seqr.next();
                self.cur_bytecode.push(Op {
                    opcode: either::Either::Left(VirtualOp::LWDataId(reg.clone(), data_id.clone())),
                    comment: "literal instantiation".into(),
                    owning_span: span,
                });
                (reg, Some(data_id))
            }
        }

        // Insert the value into the map.
        //self.reg_map.insert(*value, reg.clone());
        //
        // Actually, no, don't.  It's possible for constant values to be
        // reused in the IR, especially with transforms which copy blocks
        // around, like inlining.  The `LW`/`LWDataId` instruction above
        // initialises that constant value but it may be in a conditional
        // block and not actually get evaluated for every possible
        // execution. So using the register later on by pulling it from
        // `self.reg_map` will have a potentially uninitialised register.
        //
        // By not putting it in the map we recreate the `LW` each time it's
        // used, which also isn't ideal.  A better solution is to put this
        // initialisation into the IR itself, and allow for analysis there
        // to determine when it may be initialised and/or reused.
    }

    // Get the reg corresponding to `value`. Returns None if the value is not in reg_map or is not
    // a constant.
    fn opt_value_to_register(&mut self, value: &Value) -> Option<VirtualRegister> {
        self.reg_map
            .get(value)
            .cloned()
            .or_else(|| {
                value.get_constant(self.context).map(|constant| {
                    let span = self.md_mgr.val_to_span(self.context, *value);
                    self.initialise_constant(constant, None, span).0
                })
            })
            .or_else(|| {
                value.get_configurable(self.context).map(|constant| {
                    let span = self.md_mgr.val_to_span(self.context, *value);
                    let config_name = self
                        .md_mgr
                        .md_to_config_const_name(self.context, value.get_metadata(self.context))
                        .unwrap()
                        .to_string();

                    let initialized =
                        self.initialise_constant(constant, Some(config_name.clone()), span);
                    if let Some(data_id) = initialized.1 {
                        self.data_section.config_map.insert(config_name, data_id.0);
                    }
                    initialized.0
                })
            })
    }

    // Same as `opt_value_to_register` but returns a new register if no register is found or if
    // `value` is not a constant.
    pub(super) fn value_to_register(&mut self, value: &Value) -> VirtualRegister {
        match self.opt_value_to_register(value) {
            Some(reg) => reg,
            None => {
                // Just make a new register for this value.
                let reg = self.reg_seqr.next();
                self.reg_map.insert(*value, reg.clone());
                reg
            }
        }
    }

    pub(super) fn number_to_reg(
        &mut self,
        offset: u64,
        offset_reg: &VirtualRegister,
        span: Option<Span>,
    ) {
        if offset > compiler_constants::TWENTY_FOUR_BITS {
            todo!("Absolutely giant arrays.");
        }

        // Use bitwise ORs and SHIFTs to crate a 24 bit value in a register.
        self.cur_bytecode.push(Op {
            opcode: either::Either::Left(VirtualOp::ORI(
                offset_reg.clone(),
                VirtualRegister::Constant(ConstantRegister::Zero),
                VirtualImmediate12 {
                    value: (offset >> 12) as u16,
                },
            )),
            comment: "get extract offset high bits".into(),
            owning_span: span.clone(),
        });
        self.cur_bytecode.push(Op {
            opcode: either::Either::Left(VirtualOp::SLLI(
                offset_reg.clone(),
                offset_reg.clone(),
                VirtualImmediate12 { value: 12 },
            )),
            comment: "shift extract offset high bits".into(),
            owning_span: span.clone(),
        });
        self.cur_bytecode.push(Op {
            opcode: either::Either::Left(VirtualOp::ORI(
                offset_reg.clone(),
                offset_reg.clone(),
                VirtualImmediate12 {
                    value: (offset & 0xfff) as u16,
                },
            )),
            comment: "get extract offset low bits".into(),
            owning_span: span,
        });
    }

    pub(super) fn func_to_labels(&mut self, func: &Function) -> (Label, Label) {
        self.func_label_map.get(func).cloned().unwrap_or_else(|| {
            let labels = (self.reg_seqr.get_label(), self.reg_seqr.get_label());
            self.func_label_map.insert(*func, labels);
            labels
        })
    }

    fn block_to_label(&mut self, block: &Block) -> Label {
        self.block_label_map.get(block).cloned().unwrap_or_else(|| {
            let label = self.reg_seqr.get_label();
            self.block_label_map.insert(*block, label);
            label
        })
    }
}
