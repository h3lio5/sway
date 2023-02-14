use std::hash::{Hash, Hasher};

use sway_types::{ident::Ident, span::Span, Spanned};

use crate::{
    decl_engine::*,
    engine_threading::*,
    language::{parsed::*, *},
    transform,
    type_system::*,
};

#[derive(Debug, Clone)]
pub struct TraitDeclaration {
    pub name: Ident,
    pub(crate) type_parameters: Vec<TypeParameter>,
    pub attributes: transform::AttributesMap,
    pub interface_surface: Vec<TraitFn>,
    pub methods: Vec<FunctionDeclaration>,
    pub supertraits: Vec<Supertrait>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Supertrait {
    pub name: CallPath,
    pub decl_ref: Option<DeclRef>,
}

impl Spanned for Supertrait {
    fn span(&self) -> Span {
        self.name.span()
    }
}

impl EqWithEngines for Supertrait {}
impl PartialEqWithEngines for Supertrait {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        let Supertrait {
            name: ln,
            decl_ref: ldr,
        } = self;
        let Supertrait {
            name: rn,
            decl_ref: rdr,
        } = other;
        ln == rn && ldr.eq(rdr, type_engine)
    }
}

impl HashWithEngines for Supertrait {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let Supertrait { name, decl_ref } = self;
        name.hash(state);
        decl_ref.hash(state, type_engine);
    }
}

#[derive(Debug, Clone)]
pub struct TraitFn {
    pub name: Ident,
    pub attributes: transform::AttributesMap,
    pub purity: Purity,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: TypeInfo,
    pub return_type_span: Span,
}
