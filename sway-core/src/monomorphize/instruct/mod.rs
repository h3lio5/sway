pub(crate) mod code_block;
pub(crate) mod context;
pub(crate) mod declaration;
pub(crate) mod expression;
pub(crate) mod module;
pub(crate) mod node;

use std::sync::RwLock;

use hashbrown::HashMap;
use sway_error::handler::{ErrorEmitted, Handler};

use crate::{decl_engine::DeclId, language::ty, monomorphize::priv_prelude::*, Engines};

pub(crate) fn apply_instructions(
    engines: Engines<'_>,
    handler: &Handler,
    instructions: Vec<Instruction>,
    module: &mut ty::TyModule,
) -> Result<(), ErrorEmitted> {
    let (decl_map, other_instructions) = preprocess_instructions(instructions);
    let ctx = InstructContext::from_root(engines, &decl_map, &other_instructions);

    instruct_root(ctx, handler, module)?;

    Ok(())
}

fn preprocess_instructions(
    instructions: Vec<Instruction>,
) -> (RwLock<HashMap<DeclId, Vec<Instruction>>>, Vec<Instruction>) {
    let mut decl_map: HashMap<DeclId, Vec<Instruction>> = HashMap::new();
    let mut other_instructions = vec![];
    for instruction in instructions.into_iter() {
        match &instruction {
            Instruction::MonomorphizeDecl(decl_id, _) => {
                let v = decl_map.entry(decl_id.clone()).or_default();
                v.push(instruction);
            }
            _ => {
                other_instructions.push(instruction);
            }
        }
    }
    (RwLock::new(decl_map), other_instructions)
}
