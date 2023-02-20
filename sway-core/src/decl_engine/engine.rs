use hashbrown::{hash_map::RawEntryMut, HashMap};
use std::{fmt, sync::RwLock};

use sway_error::error::CompileError;
use sway_types::Span;

use crate::{
    concurrent_slab::{ConcurrentSlab, ListDisplay},
    decl_engine::*,
    engine_threading::*,
    language::ty,
    TypeEngine,
};

/// Used inside of type inference to store declarations.
#[derive(Debug, Default)]
pub struct DeclEngine {
    /// Holds all of the declarations.
    slab: ConcurrentSlab<DeclWrapper>,

    /// Map from declaration -> id so that id's are unique.
    id_map: RwLock<HashMap<DeclWrapper, DeclId>>,
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

    pub(crate) fn insert<T>(&self, type_engine: &TypeEngine, decl: T) -> DeclId
    where
        T: Into<(Ident, DeclWrapper, Span)>,
    {
        let (_, decl_wrapper, _) = decl.into();
        self.insert_wrapper(type_engine, decl_wrapper)
    }

    pub(crate) fn insert_wrapper(
        &self,
        type_engine: &TypeEngine,
        decl_wrapper: DeclWrapper,
    ) -> DeclId {
        let mut id_map = self.id_map.write().unwrap();

        let hash_builder = id_map.hasher().clone();
        let decl_hash = make_hasher(&hash_builder, type_engine)(&decl_wrapper);

        match id_map
            .raw_entry_mut()
            .from_hash(decl_hash, |x| x.eq(&decl_wrapper, type_engine))
        {
            RawEntryMut::Occupied(o) => *o.get(),
            RawEntryMut::Vacant(v) => {
                let decl_id = DeclId::new(self.slab.insert(decl_wrapper.clone()));
                v.insert_with_hasher(
                    decl_hash,
                    decl_wrapper,
                    decl_id,
                    make_hasher(&hash_builder, type_engine),
                );
                decl_id
            }
        }
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
