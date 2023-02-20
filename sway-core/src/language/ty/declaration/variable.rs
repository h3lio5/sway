use std::hash::{Hash, Hasher};

use sway_types::Ident;

use crate::{engine_threading::*, language::ty::*, type_system::*};

#[derive(Clone, Debug)]
pub struct TyVariableDeclaration {
    pub name: Ident,
    pub body: TyExpression,
    pub mutability: VariableMutability,
    pub return_type: TypeId,
    pub type_ascription: TypeArgument,
}

impl EqWithEngines for TyVariableDeclaration {}
impl PartialEqWithEngines for TyVariableDeclaration {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.name == other.name
            && self.body.eq(&other.body, type_engine)
            && self.mutability == other.mutability
            && type_engine
                .get(self.return_type)
                .eq(&type_engine.get(other.return_type), type_engine)
            && self.type_ascription.eq(&other.type_ascription, type_engine)
    }
}

impl HashWithEngines for TyVariableDeclaration {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyVariableDeclaration {
            name,
            body,
            mutability,
            return_type,
            type_ascription,
        } = self;
        name.hash(state);
        body.hash(state, type_engine);
        type_engine.get(*return_type).hash(state, type_engine);
        type_ascription.hash(state, type_engine);
        mutability.hash(state);
    }
}

impl SubstTypes for TyVariableDeclaration {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        self.return_type.subst(engines, subst_list);
        self.type_ascription.subst(engines, subst_list);
        self.body.subst(engines, subst_list)
    }
}
