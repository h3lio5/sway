use crate::Engines;

/// An object that is a template for copies from the template.
///
/// This is predominantly used with
/// [TypeSubstList](crate::type_system::TypeSubstList) inside
/// [TyDeclaration](crate::language::ty::TyDeclaration). The various
/// variants of [TyDeclaration](crate::language::ty::TyDeclaration) contain
/// fields `type_subst_list: Template<TypeSubstList>`. This type indicates that
/// the [TypeSubstList](crate::type_system::TypeSubstList) contained in this
/// field is simply a template for usages of the
/// declaration declared in that particular
/// [TyDeclaration](crate::language::ty::TyDeclaration) node.
#[derive(Clone, Debug)]
pub struct Template<T>(T)
where
    T: Templated + Clone;

impl<T> Template<T>
where
    T: Templated + Clone,
{
    pub(crate) fn new(value: T) -> Template<T> {
        Template(value)
    }

    pub(crate) fn inner(&self) -> &T {
        &self.0
    }

    pub(crate) fn inner_mut(&mut self) -> &mut T {
        &mut self.0
    }

    pub(crate) fn into_inner(self) -> T {
        self.0
    }

    pub(crate) fn fresh_value(&self, engines: Engines<'_>) -> T {
        self.0.fresh_copy(engines)
    }
}

pub trait Templated {
    fn fresh_copy(&self, engines: Engines<'_>) -> Self;
}
