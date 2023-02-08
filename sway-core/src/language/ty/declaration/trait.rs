use std::hash::{Hash, Hasher};

use sway_types::{Ident, Span};

use crate::{
    decl_engine::DeclRef,
    engine_threading::*,
    language::{parsed, Visibility},
    transform,
    type_system::*,
};

#[derive(Clone, Debug)]
pub struct TyTraitDeclaration {
    pub name: Ident,
    pub type_parameters: Vec<TypeParameter>,
    pub interface_surface: Vec<DeclRef>,
    pub methods: Vec<DeclRef>,
    pub supertraits: Vec<parsed::Supertrait>,
    pub visibility: Visibility,
    pub attributes: transform::AttributesMap,
    pub span: Span,
}

impl EqWithEngines for TyTraitDeclaration {}
impl PartialEqWithEngines for TyTraitDeclaration {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.name == other.name
            && self.type_parameters.eq(&other.type_parameters, type_engine)
            && self.interface_surface == other.interface_surface
            && self.methods == other.methods
            && self.supertraits == other.supertraits
            && self.visibility == other.visibility
    }
}

impl HashWithEngines for TyTraitDeclaration {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyTraitDeclaration {
            name,
            type_parameters,
            interface_surface,
            methods,
            supertraits,
            visibility,
            // these fields are not hashed because they aren't relevant/a
            // reliable source of obj v. obj distinction
            attributes: _,
            span: _,
        } = self;
        name.hash(state);
        type_parameters.hash(state, type_engine);
        interface_surface.hash(state);
        methods.hash(state);
        supertraits.hash(state);
        visibility.hash(state);
    }
}

impl SubstTypes for TyTraitDeclaration {
    fn subst_inner(&mut self, type_mapping: &TypeSubstMap, engines: Engines<'_>) {
        self.type_parameters
            .iter_mut()
            .for_each(|x| x.subst(type_mapping, engines));
        self.interface_surface
            .iter_mut()
            .for_each(|function_decl_ref| {
                let new_decl_ref = function_decl_ref
                    .clone()
                    .subst_types_and_insert_new(type_mapping, engines);
                function_decl_ref.replace_id((&new_decl_ref).into());
            });
        // we don't have to type check the methods because it hasn't been type checked yet
    }
}

impl MonomorphizeHelper for TyTraitDeclaration {
    fn name(&self) -> &Ident {
        &self.name
    }

    fn type_parameters(&self) -> &[TypeParameter] {
        &self.type_parameters
    }
}
