use std::sync::RwLock;

use hashbrown::HashMap;

use crate::{decl_engine::*, monomorphize::priv_prelude::*, Engines, TypeEngine};

/// Contextual state tracked and accumulated throughout applying the
/// monomorphization instructions.
pub(crate) struct InstructContext<'a> {
    /// The type engine storing types.
    pub(crate) type_engine: &'a TypeEngine,

    /// The declaration engine holds declarations.
    pub(crate) decl_engine: &'a DeclEngine,

    /// A shortcut map of [DeclId]s to be monomorphized.
    decl_map: &'a RwLock<HashMap<DeclId, Vec<Instruction>>>,

    /// The list of instructions not included in the `decl_map`.
    instructions: &'a [Instruction],
}

impl<'a> InstructContext<'a> {
    /// Initialize a context at the top-level of a module with its namespace.
    pub(crate) fn from_root(
        engines: Engines<'a>,
        decl_map: &'a RwLock<HashMap<DeclId, Vec<Instruction>>>,
        instructions: &'a [Instruction],
    ) -> Self {
        Self::from_module_namespace(engines, decl_map, instructions)
    }

    fn from_module_namespace(
        engines: Engines<'a>,
        decl_map: &'a RwLock<HashMap<DeclId, Vec<Instruction>>>,
        instructions: &'a [Instruction],
    ) -> Self {
        let (type_engine, decl_engine) = engines.unwrap();
        Self {
            type_engine,
            decl_engine,
            decl_map,
            instructions,
        }
    }

    /// Create a new context that mutably borrows the inner [Namespace] with a
    /// lifetime bound by `self`.
    pub(crate) fn by_ref(&mut self) -> InstructContext<'_> {
        InstructContext {
            type_engine: self.type_engine,
            decl_engine: self.decl_engine,
            decl_map: self.decl_map,
            instructions: self.instructions,
        }
    }

    /// Scope the [InstructContext] with the given [Namespace].
    pub(crate) fn scoped(self) -> InstructContext<'a> {
        InstructContext {
            type_engine: self.type_engine,
            decl_engine: self.decl_engine,
            decl_map: self.decl_map,
            instructions: self.instructions,
        }
    }
}
