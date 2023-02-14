use std::hash::{Hash, Hasher};

use sway_types::{Ident, Span, Spanned};

use crate::{decl_engine::*, engine_threading::*, type_system::*};

/// A reference to the use of a declaration. A smart-wrapper around a [DeclId],
/// containing additional information about a declaration.
#[derive(Debug, Clone)]
pub struct DeclRef {
    /// The name of the declaration.
    // NOTE: In the case of storage, the name is "storage".
    pub name: Ident,

    /// The index into the [DeclEngine].
    pub id: DeclId,

    /// The [Span] of the entire declaration.
    pub decl_span: Span,
}

impl DeclRef {
    pub(crate) fn new(name: Ident, id: usize, decl_span: Span) -> DeclRef {
        DeclRef {
            name,
            id: DeclId::new(id),
            decl_span,
        }
    }

    pub(crate) fn replace_id(&mut self, index: DeclId) {
        self.id.replace_id(index);
    }

    pub(crate) fn subst_types_and_insert_new(
        &self,
        type_mapping: &TypeSubstMap,
        engines: Engines<'_>,
    ) -> DeclRef {
        let decl_engine = engines.de();
        let mut decl = decl_engine.get(self);
        decl.subst(type_mapping, engines);
        decl_engine.insert_wrapper(self.name.clone(), decl, self.decl_span.clone())
    }
}

impl PartialEq for DeclRef {
    fn eq(&self, other: &Self) -> bool {
        let DeclRef {
            name: ln,
            id: lid,
            // these fields are not used in comparison because they aren't
            // relevant/a reliable source of obj v. obj distinction
            decl_span: _,
        } = self;
        let DeclRef {
            name: rn,
            id: rid,
            // these fields are not used in comparison because they aren't
            // relevant/a reliable source of obj v. obj distinction
            decl_span: _,
        } = other;
        ln == rn && lid == rid
    }
}

impl Eq for DeclRef {}

impl Hash for DeclRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let DeclRef {
            name,
            id,
            // these fields are not hashed because they aren't relevant/a
            // reliable source of obj v. obj distinction
            decl_span: _,
        } = self;
        name.hash(state);
        id.hash(state);
    }
}

impl Spanned for DeclRef {
    fn span(&self) -> Span {
        self.decl_span.clone()
    }
}

impl SubstTypes for DeclRef {
    fn subst_inner(&mut self, type_mapping: &TypeSubstMap, engines: Engines<'_>) {
        let decl_engine = engines.de();
        let mut decl = decl_engine.get(self);
        decl.subst(type_mapping, engines);
        decl_engine.replace(self, decl);
    }
}
