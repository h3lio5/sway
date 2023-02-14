use std::fmt;

use sway_error::error::CompileError;
use sway_types::{Ident, Span};

use crate::{
    concurrent_slab::{ConcurrentSlab, ListDisplay},
    decl_engine::*,
    language::ty,
};

/// Used inside of type inference to store declarations.
#[derive(Debug, Default)]
pub struct DeclEngine {
    slab: ConcurrentSlab<DeclWrapper>,
}

impl fmt::Display for DeclEngine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.slab.with_slice(|elems| {
            let list = ListDisplay { list: elems.iter() };
            write!(f, "DeclarationEngine {{\n{list}\n}}")
        })
    }
}

impl DeclEngine {
    pub(crate) fn get<'a, T>(&self, index: &'a T) -> DeclWrapper
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index))
    }

    pub(super) fn replace<'a, T>(&self, index: &'a T, wrapper: DeclWrapper)
    where
        DeclId: From<&'a T>,
    {
        self.slab.replace(DeclId::from(index), wrapper);
    }

    pub(crate) fn insert<T>(&self, decl: T) -> DeclRef
    where
        T: Into<(Ident, DeclWrapper, Span)>,
    {
        let (ident, decl_wrapper, span) = decl.into();
        DeclRef::new(ident, self.slab.insert(decl_wrapper), span)
    }

    pub(crate) fn insert_wrapper(
        &self,
        ident: Ident,
        decl_wrapper: DeclWrapper,
        span: Span,
    ) -> DeclRef {
        DeclRef::new(ident, self.slab.insert(decl_wrapper), span)
    }

    pub fn get_function<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyFunctionDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_function(span)
    }

    pub fn get_trait<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyTraitDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_trait(span)
    }

    pub fn get_trait_fn<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyTraitFn, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_trait_fn(span)
    }

    pub fn get_impl_trait<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyImplTrait, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_impl_trait(span)
    }

    pub fn get_struct<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyStructDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_struct(span)
    }

    pub fn get_storage<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyStorageDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_storage(span)
    }

    pub fn get_abi<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyAbiDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_abi(span)
    }

    pub fn get_constant<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyConstantDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_constant(span)
    }

    pub fn get_enum<'a, T>(
        &self,
        index: &'a T,
        span: &Span,
    ) -> Result<ty::TyEnumDeclaration, CompileError>
    where
        DeclId: From<&'a T>,
    {
        self.slab.get(*DeclId::from(index)).expect_enum(span)
    }
}
