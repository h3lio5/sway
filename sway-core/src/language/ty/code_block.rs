use std::hash::Hasher;

use crate::{
    decl_engine::*, engine_threading::*, language::ty::*, type_system::*,
    types::DeterministicallyAborts,
};

#[derive(Clone, Debug)]
pub struct TyCodeBlock {
    pub contents: Vec<TyAstNode>,
}

impl EqWithEngines for TyCodeBlock {}
impl PartialEqWithEngines for TyCodeBlock {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.contents.eq(&other.contents, type_engine)
    }
}

impl HashWithEngines for TyCodeBlock {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyCodeBlock { contents } = self;
        contents.hash(state, type_engine);
    }
}

impl SubstTypes for TyCodeBlock {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        self.contents
            .iter_mut()
            .for_each(|x| x.subst(engines, subst_list));
    }
}

impl DeterministicallyAborts for TyCodeBlock {
    fn deterministically_aborts(&self, decl_engine: &DeclEngine, check_call_body: bool) -> bool {
        self.contents
            .iter()
            .any(|x| x.deterministically_aborts(decl_engine, check_call_body))
    }
}
