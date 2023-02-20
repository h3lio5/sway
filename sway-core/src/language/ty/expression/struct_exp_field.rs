use std::hash::{Hash, Hasher};

use sway_types::Ident;

use crate::{engine_threading::*, language::ty::*, type_system::*};

#[derive(Clone, Debug)]
pub struct TyStructExpressionField {
    pub name: Ident,
    pub value: TyExpression,
}

impl EqWithEngines for TyStructExpressionField {}
impl PartialEqWithEngines for TyStructExpressionField {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.name == other.name && self.value.eq(&other.value, type_engine)
    }
}

impl HashWithEngines for TyStructExpressionField {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyStructExpressionField { name, value } = self;
        name.hash(state);
        value.hash(state, type_engine);
    }
}

impl SubstTypes for TyStructExpressionField {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        self.value.subst(engines, subst_list);
    }
}
