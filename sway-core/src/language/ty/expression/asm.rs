use std::hash::{Hash, Hasher};

use sway_types::Ident;

use crate::{engine_threading::*, language::ty::*, type_system::*};

#[derive(Clone, Debug)]
pub struct TyAsmRegisterDeclaration {
    pub(crate) initializer: Option<TyExpression>,
    pub(crate) name: Ident,
}

impl PartialEqWithEngines for TyAsmRegisterDeclaration {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.name == other.name
            && if let (Some(l), Some(r)) = (&self.initializer, &other.initializer) {
                l.eq(r, type_engine)
            } else {
                true
            }
    }
}

impl HashWithEngines for TyAsmRegisterDeclaration {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyAsmRegisterDeclaration { initializer, name } = self;
        name.hash(state);
        if let Some(x) = initializer.as_ref() {
            x.hash(state, type_engine)
        }
    }
}

impl SubstTypes for TyAsmRegisterDeclaration {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        if let Some(ref mut initializer) = self.initializer {
            initializer.subst(engines, subst_list)
        }
    }
}
