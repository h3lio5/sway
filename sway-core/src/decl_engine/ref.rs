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

    /// The type substitution list to apply to the `id` field for type
    /// monomorphization.
    pub(crate) subst_list: TypeSubstList,

    /// The [Span] of the entire declaration.
    pub decl_span: Span,
}

impl DeclRef {
    pub(crate) fn new(
        name: Ident,
        id: usize,
        subst_list: TypeSubstList,
        decl_span: Span,
    ) -> DeclRef {
        DeclRef {
            name,
            id: DeclId::new(id),
            subst_list,
            decl_span,
        }
    }

    pub(crate) fn replace_id(&mut self, index: DeclId) {
        self.id.replace_id(index);
    }

    pub(crate) fn subst_types_and_insert_new(
        &self,
        engines: Engines<'_>,
        subst_list: &TypeSubstList,
    ) -> DeclRef {
        let type_engine = engines.te();
        let decl_engine = engines.de();
        let mut decl = decl_engine.get(self);
        decl.subst(engines, subst_list);
        DeclRef {
            name: todo!(),
            id: decl_engine.insert_wrapper(type_engine, decl),
            subst_list: todo!(),
            decl_span: todo!(),
        }
    }
}

impl EqWithEngines for DeclRef {}
impl PartialEqWithEngines for DeclRef {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        let DeclRef {
            name: ln,
            id: lid,
            subst_list: lsl,
            // these fields are not used in comparison because they aren't
            // relevant/a reliable source of obj v. obj distinction
            decl_span: _,
        } = self;
        let DeclRef {
            name: rn,
            id: rid,
            subst_list: rsl,
            // these fields are not used in comparison because they aren't
            // relevant/a reliable source of obj v. obj distinction
            decl_span: _,
        } = other;
        ln == rn && lid == rid && lsl.eq(rsl, type_engine)
    }
}

impl HashWithEngines for DeclRef {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let DeclRef {
            name,
            id,
            subst_list,
            // these fields are not hashed because they aren't relevant/a
            // reliable source of obj v. obj distinction
            decl_span: _,
        } = self;
        name.hash(state);
        id.hash(state);
        subst_list.hash(state, type_engine);
    }
}

impl Spanned for DeclRef {
    fn span(&self) -> Span {
        self.decl_span.clone()
    }
}

impl SubstTypes for DeclRef {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        let decl_engine = engines.de();
        let mut decl = decl_engine.get(self);
        decl.subst(engines, subst_list);
        decl_engine.replace(self, decl);
    }
}
