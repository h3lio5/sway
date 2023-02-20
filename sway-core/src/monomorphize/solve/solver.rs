use sway_error::handler::{ErrorEmitted, Handler};

use crate::{decl_engine::*, engine_threading::*, monomorphize::priv_prelude::*, type_system::*};

pub(crate) struct Solver<'a> {
    /// The type engine storing types.
    type_engine: &'a TypeEngine,

    /// The declaration engine holds declarations.
    decl_engine: &'a DeclEngine,

    /// The instructions returned by the [Solver].
    instructions: Vec<Instruction>,
}

impl<'a> Solver<'a> {
    /// Creates a new [Solver].
    pub(crate) fn new(engines: Engines<'a>) -> Solver<'a> {
        let (type_engine, decl_engine) = engines.unwrap();
        Solver {
            type_engine,
            decl_engine,
            instructions: vec![],
        }
    }

    /// Takes a [Solver] and returns the list of [Instruction]s from that
    /// [Solver].
    pub(crate) fn into_instructions(self) -> Vec<Instruction> {
        self.instructions
    }

    /// Solves a set of constraints with a given [Solver].
    pub(crate) fn solve<T>(&mut self, handler: &Handler, constraints: T) -> Result<(), ErrorEmitted>
    where
        T: IntoIterator<Item = Constraint>,
    {
        let mut constraints: ConstraintPQ = constraints
            .into_iter()
            .map(|c| self.wrap_constraint(c))
            .collect();

        // for constraint in constraints.iter() {
        //     println!("{:#?}", constraint);
        // }

        let mut iterations = 0;
        let mut instructions = vec![];

        while iterations < 100 && !constraints.is_empty() {
            let report = self.helper(
                handler,
                constraints
                    .into_sorted_vec()
                    .into_iter()
                    .map(|c| c.thing)
                    .collect(),
            )?;

            constraints = report.new_constraints;
            instructions.extend(report.instructions);

            iterations += 1;
        }

        self.instructions.extend(instructions);

        Ok(())
    }

    fn helper(
        &self,
        handler: &Handler,
        constraints: Vec<Constraint>,
    ) -> Result<IterationReport, ErrorEmitted> {
        let mut new_constraints = ConstraintPQ::new();
        let mut instructions = vec![];

        for constraint in constraints.into_iter() {
            let instruction_res = match &constraint {
                // these might be necessary (?) but right now they are not used
                Constraint::Ty(_) => InstructionResult::NoInstruction,
                Constraint::Decl { .. } => InstructionResult::NoInstruction,
                Constraint::FnCall {
                    decl_id,
                    subst_list,
                    ..
                } => self.helper_fn_call(handler, *decl_id, subst_list.clone())?,
            };
            match instruction_res {
                InstructionResult::NewInstructions(instruction_res) => {
                    instructions.extend(instruction_res);
                }
                InstructionResult::NoInstruction => {}
                InstructionResult::RedoConstraint => {
                    new_constraints.push(self.wrap_constraint(constraint));
                }
            }
        }

        let report = IterationReport {
            new_constraints,
            instructions,
        };

        Ok(report)
    }

    fn helper_ty_use(
        &self,
        handler: &Handler,
        type_id: TypeId,
    ) -> Result<InstructionResult, ErrorEmitted> {
        let mut instructions = vec![];

        match self.type_engine.get(type_id) {
            TypeInfo::Unknown => todo!(),
            TypeInfo::UnknownGeneric { .. } => todo!(),
            TypeInfo::Placeholder(_) => todo!(),
            TypeInfo::TypeParam(_) => todo!(),
            TypeInfo::Enum { .. } => todo!(),
            TypeInfo::Struct { .. } => todo!(),
            TypeInfo::Tuple(elems) => {
                let res: InstructionResult = elems
                    .into_iter()
                    .map(|type_arg| self.helper_ty_use(handler, type_arg.type_id))
                    .collect::<Result<_, _>>()?;
                match res {
                    InstructionResult::NewInstructions(new_instructions) => {
                        instructions.extend(new_instructions);
                    }
                    InstructionResult::NoInstruction => {}
                    InstructionResult::RedoConstraint => {
                        return Ok(InstructionResult::RedoConstraint);
                    }
                }
            }
            TypeInfo::ContractCaller { .. } => todo!(),
            TypeInfo::Custom { .. } => todo!(),
            TypeInfo::SelfType => todo!(),
            TypeInfo::Numeric => todo!(),
            TypeInfo::ErrorRecovery => todo!(),
            TypeInfo::Array(_, _) => todo!(),
            TypeInfo::Storage { .. } => todo!(),
            TypeInfo::Str(_)
            | TypeInfo::UnsignedInteger(_)
            | TypeInfo::Boolean
            | TypeInfo::B256
            | TypeInfo::Contract
            | TypeInfo::RawUntypedPtr
            | TypeInfo::RawUntypedSlice => {}
        }

        Ok(InstructionResult::from_instructions(instructions))
    }

    fn helper_fn_call(
        &self,
        handler: &Handler,
        decl_id: DeclId,
        subst_list: TypeSubstList,
    ) -> Result<InstructionResult, ErrorEmitted> {
        let mut instructions = vec![];

        // let engines = Engines::new(self.type_engine, self.decl_engine);
        // let mut decl = self.decl_engine.get(&decl_id);
        // match decl {
        //     DeclWrapper::Function(ref mut fn_decl) => {
        //         fn_decl.subst(engines, &subst_list);
        //     }
        //     DeclWrapper::Trait(_) => todo!(),
        //     DeclWrapper::ImplTrait(_) => todo!(),
        //     DeclWrapper::Struct(_) => todo!(),
        //     DeclWrapper::Enum(_) => todo!(),
        //     // None of these would generate a [TypeSubstList] anyway.
        //     DeclWrapper::Unknown
        //     | DeclWrapper::Storage(_)
        //     | DeclWrapper::Abi(_)
        //     | DeclWrapper::Constant(_)
        //     | DeclWrapper::TraitFn(_) => {}
        // }
        // self.decl_engine
        //     .insert_into_mono_cache(self.type_engine, decl_id, subst_list, decl);

        let res: InstructionResult = subst_list
            .iter()
            .map(|type_param| self.helper_ty_use(handler, type_param.type_id))
            .collect::<Result<_, _>>()?;
        match res {
            InstructionResult::NewInstructions(new_instructions) => {
                instructions.extend(new_instructions);
            }
            InstructionResult::NoInstruction => {}
            InstructionResult::RedoConstraint => {
                return Ok(InstructionResult::RedoConstraint);
            }
        }

        instructions.push(Instruction::MonomorphizeDecl(decl_id, subst_list));

        Ok(InstructionResult::from_instructions(instructions))
    }

    fn wrap_constraint(&self, constraint: Constraint) -> ConstraintWrapper {
        WithEngines {
            thing: constraint,
            engines: Engines::new(self.type_engine, self.decl_engine),
        }
    }
}
