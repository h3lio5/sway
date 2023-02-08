use std::hash::{Hash, Hasher};

use sway_types::{state::StateIndex, Ident, Span, Spanned};

use crate::{engine_threading::*, type_system::TypeId, TypeEngine};

/// Describes the full storage access including all the subfields
#[derive(Clone, Debug)]
pub struct TyStorageAccess {
    pub fields: Vec<TyStorageAccessDescriptor>,
    pub(crate) ix: StateIndex,
}

impl EqWithEngines for TyStorageAccess {}
impl PartialEqWithEngines for TyStorageAccess {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.ix == other.ix
            && self.fields.len() == other.fields.len()
            && self.fields.eq(&other.fields, type_engine)
    }
}

impl HashWithEngines for TyStorageAccess {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyStorageAccess { fields, ix } = self;
        fields.hash(state, type_engine);
        ix.hash(state);
    }
}

impl Spanned for TyStorageAccess {
    fn span(&self) -> Span {
        self.fields
            .iter()
            .fold(self.fields[0].span.clone(), |acc, field| {
                Span::join(acc, field.span.clone())
            })
    }
}

impl TyStorageAccess {
    pub fn storage_field_name(&self) -> Ident {
        self.fields[0].name.clone()
    }
}

/// Describes a single subfield access in the sequence when accessing a subfield within storage.
#[derive(Clone, Debug)]
pub struct TyStorageAccessDescriptor {
    pub name: Ident,
    pub(crate) type_id: TypeId,
    pub(crate) span: Span,
}

impl EqWithEngines for TyStorageAccessDescriptor {}
impl PartialEqWithEngines for TyStorageAccessDescriptor {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        self.name == other.name
            && type_engine
                .get(self.type_id)
                .eq(&type_engine.get(other.type_id), type_engine)
    }
}

impl HashWithEngines for TyStorageAccessDescriptor {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TyStorageAccessDescriptor {
            name,
            type_id,
            // these fields are not hashed because they aren't relevant/a
            // reliable source of obj v. obj distinction
            span: _,
        } = self;
        name.hash(state);
        type_engine.get(*type_id).hash(state, type_engine);
    }
}
