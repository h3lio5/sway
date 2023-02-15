use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use crate::{decl_engine::DeclId, engine_threading::*, type_system::*};

#[derive(Debug)]
pub(crate) enum Instruction {
    MonomorphizeType(TypeId, TypeSubstList),
    MonomorphizeDecl(DeclId, TypeSubstList),
}

impl Instruction {
    fn discriminant_value(&self) -> u8 {
        match self {
            Instruction::MonomorphizeType(_, _) => 0,
            Instruction::MonomorphizeDecl(_, _) => 1,
        }
    }
}

impl EqWithEngines for Instruction {}
impl PartialEqWithEngines for Instruction {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        match (self, other) {
            (Instruction::MonomorphizeType(l1, l2), Instruction::MonomorphizeType(r1, r2)) => {
                type_engine.get(*l1).eq(&type_engine.get(*r1), type_engine)
                    && l2.eq(r2, type_engine)
            }
            (Instruction::MonomorphizeDecl(l1, l2), Instruction::MonomorphizeDecl(r1, r2)) => {
                l1 == r1 && l2.eq(r2, type_engine)
            }
            (l, r) => l.discriminant_value() == r.discriminant_value(),
        }
    }
}

impl HashWithEngines for Instruction {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        match self {
            Instruction::MonomorphizeType(type_id, subst_list) => {
                state.write_u8(self.discriminant_value());
                type_engine.get(*type_id).hash(state, type_engine);
                subst_list.hash(state, type_engine);
            }
            Instruction::MonomorphizeDecl(decl_id, subst_list) => {
                state.write_u8(self.discriminant_value());
                decl_id.hash(state);
                subst_list.hash(state, type_engine);
            }
        }
    }
}

impl OrdWithEngines for Instruction {
    fn cmp(&self, other: &Self, type_engine: &TypeEngine) -> Ordering {
        match (self, other) {
            (Instruction::MonomorphizeType(l1, l2), Instruction::MonomorphizeType(r1, r2)) => {
                type_engine
                    .get(*l1)
                    .cmp(&type_engine.get(*r1), type_engine)
                    .then_with(|| l2.cmp(r2, type_engine))
            }
            (Instruction::MonomorphizeDecl(l1, l2), Instruction::MonomorphizeDecl(r1, r2)) => {
                l1.cmp(r1).then_with(|| l2.cmp(r2, type_engine))
            }
            (l, r) => l.discriminant_value().cmp(&r.discriminant_value()),
        }
    }
}
