use std::hash::{Hash, Hasher};

use sway_types::Span;

use crate::{decl_engine::DeclRef, engine_threading::*, language::CallPath, type_system::*};

#[derive(Clone, Debug)]
pub struct TyImplTrait {
    pub impl_type_parameters: Vec<TypeParameter>,
    pub trait_name: CallPath,
    pub trait_type_arguments: Vec<TypeArgument>,
    pub methods: Vec<DeclRef>,
    pub implementing_for_type_id: TypeId,
    pub trait_decl_ref: Option<DeclRef>,
    pub type_implementing_for_span: Span,
    pub span: Span,
}

impl EqWithEngines for TyImplTrait {}
impl PartialEqWithEngines for TyImplTrait {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.impl_type_parameters
            .eq(&other.impl_type_parameters, type_engine)
            && self.trait_name == other.trait_name
            && self
                .trait_type_arguments
                .eq(&other.trait_type_arguments, type_engine)
            && self.methods.eq(&other.methods, type_engine)
            && type_engine.get(self.implementing_for_type_id).eq(
                &type_engine.get(other.implementing_for_type_id),
                type_engine,
            )
            && self.trait_decl_ref.eq(&other.trait_decl_ref, type_engine)
    }
}

impl HashWithEngines for TyImplTrait {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyImplTrait {
            impl_type_parameters,
            trait_name,
            trait_type_arguments,
            methods,
            implementing_for_type_id,
            trait_decl_ref,
            // these fields are not hashed because they aren't relevant/a
            // reliable source of obj v. obj distinction
            type_implementing_for_span: _,
            span: _,
        } = self;
        trait_name.hash(state);
        impl_type_parameters.hash(state, type_engine);
        trait_type_arguments.hash(state, type_engine);
        methods.hash(state, type_engine);
        type_engine
            .get(*implementing_for_type_id)
            .hash(state, type_engine);
        trait_decl_ref.hash(state, type_engine);
    }
}

impl SubstTypes for TyImplTrait {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        self.impl_type_parameters
            .iter_mut()
            .for_each(|x| x.subst(engines, subst_list));
        self.implementing_for_type_id.subst(engines, subst_list);
        self.methods
            .iter_mut()
            .for_each(|x| x.subst(engines, subst_list));
    }
}
