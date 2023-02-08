use std::hash::Hash;

use super::{FunctionDeclaration, FunctionParameter};

use crate::{decl_engine::DeclRef, language::*, transform, type_system::*};
use sway_types::{ident::Ident, span::Span, Spanned};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Supertrait {
    pub name: CallPath,
    pub decl_ref: Option<DeclRef>,
}

impl Spanned for Supertrait {
    fn span(&self) -> Span {
        self.name.span()
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
