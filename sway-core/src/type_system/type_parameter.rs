use crate::{
    decl_engine::*,
    engine_threading::*,
    error::*,
    language::{ty, CallPath},
    semantic_analysis::*,
    type_system::*,
};

use sway_error::error::CompileError;
use sway_types::{ident::Ident, span::Span, Spanned};

use std::{
    cmp::Ordering,
    collections::BTreeMap,
    fmt,
    hash::{Hash, Hasher},
};

#[derive(Clone)]
pub struct TypeParameter {
    pub type_id: TypeId,
    pub(crate) initial_type_id: TypeId,
    pub name_ident: Ident,
    pub(crate) trait_constraints: Vec<TraitConstraint>,
    pub(crate) trait_constraints_span: Span,
}

impl HashWithEngines for TypeParameter {
    fn hash<H: Hasher>(&self, state: &mut H, type_engine: &TypeEngine) {
        let TypeParameter {
            type_id,
            name_ident,
            trait_constraints,
            // these fields are not hashed because they aren't relevant/a
            // reliable source of obj v. obj distinction
            trait_constraints_span: _,
            initial_type_id: _,
        } = self;
        type_engine.get(*type_id).hash(state, type_engine);
        name_ident.hash(state);
        trait_constraints.hash(state, type_engine);
    }
}

impl EqWithEngines for TypeParameter {}
impl PartialEqWithEngines for TypeParameter {
    fn eq(&self, other: &Self, type_engine: &TypeEngine) -> bool {
        type_engine
            .get(self.type_id)
            .eq(&type_engine.get(other.type_id), type_engine)
            && self.name_ident == other.name_ident
            && self
                .trait_constraints
                .eq(&other.trait_constraints, type_engine)
    }
}

impl OrdWithEngines for TypeParameter {
    fn cmp(&self, other: &Self, type_engine: &TypeEngine) -> Ordering {
        let TypeParameter {
            type_id: lti,
            name_ident: ln,
            trait_constraints: ltc,
            // these fields are not compared because they aren't relevant/a
            // reliable source of obj v. obj distinction
            trait_constraints_span: _,
            initial_type_id: _,
        } = self;
        let TypeParameter {
            type_id: rti,
            name_ident: rn,
            trait_constraints: rtc,
            // these fields are not compared because they aren't relevant/a
            // reliable source of obj v. obj distinction
            trait_constraints_span: _,
            initial_type_id: _,
        } = other;
        ln.cmp(rn)
            .then_with(|| {
                type_engine
                    .get(*lti)
                    .cmp(&type_engine.get(*rti), type_engine)
            })
            .then_with(|| ltc.cmp(rtc, type_engine))
    }
}

impl SubstTypes for TypeParameter {
    fn subst_inner(&mut self, engines: Engines<'_>, subst_list: &TypeSubstList) {
        self.type_id.subst(engines, subst_list);
        self.trait_constraints
            .iter_mut()
            .for_each(|x| x.subst(engines, subst_list));
    }
}

impl Spanned for TypeParameter {
    fn span(&self) -> Span {
        self.name_ident.span()
    }
}

impl DisplayWithEngines for TypeParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, engines: Engines<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name_ident, engines.help_out(self.type_id))
    }
}

impl fmt::Debug for TypeParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.name_ident, self.type_id)
    }
}

impl TypeParameter {
    /// Type check a list of [TypeParameter] and return a new list of
    /// [TypeParameter] and a [TypeSubstList]. This will also insert the list of
    /// [TypeParameter]s and the [TypeSubstList] into the current namespace.
    pub(crate) fn type_check_type_params(
        mut ctx: TypeCheckContext,
        type_params: Vec<TypeParameter>,
        disallow_trait_constraints: bool,
    ) -> CompileResult<(Vec<TypeParameter>, Template<TypeSubstList>)> {
        let mut warnings = vec![];
        let mut errors = vec![];

        let engines = ctx.engines();

        let mut new_type_params: Vec<TypeParameter> = vec![];
        let mut subst_list: TypeSubstList = ctx
            .namespace
            .get_template_subst_list_stack()
            .pop()
            .map(|t| t.fresh_value(engines))
            .unwrap_or_default();

        for type_param in type_params.into_iter() {
            if disallow_trait_constraints && !type_param.trait_constraints.is_empty() {
                let errors = vec![CompileError::WhereClauseNotYetSupported {
                    span: type_param.trait_constraints_span,
                }];
                return err(vec![], errors);
            }
            let (body_type_param, subst_type_param) = check!(
                TypeParameter::type_check(ctx.by_ref(), type_param, subst_list.len()),
                continue,
                warnings,
                errors
            );
            new_type_params.push(body_type_param);
            subst_list.push(subst_type_param);
        }

        let subst_list: Template<TypeSubstList> =
            Template::new(subst_list.set_num_from_the_current_scope(new_type_params.len()));

        // Push the type subst list onto the stack.
        ctx.namespace
            .get_template_subst_list_stack()
            .push(subst_list.clone());

        if errors.is_empty() {
            ok((new_type_params, subst_list), warnings, errors)
        } else {
            err(warnings, errors)
        }
    }

    /// Type checks a [TypeParameter] (including its [TraitConstraint]s) and
    /// inserts into into the current namespace.
    ///
    /// This function returns two [TypeParameter]s. The first is based on the
    /// value of `n` and is designated to go in the body of the AST object for
    /// which this [TypeParameter] was created. The second is the "oracle"
    /// [TypeParameter] and is designated to go in the parent [TypeSubstList].
    fn type_check(
        mut ctx: TypeCheckContext,
        type_parameter: TypeParameter,
        n: usize,
    ) -> CompileResult<(TypeParameter, TypeParameter)> {
        let mut warnings = vec![];
        let mut errors = vec![];

        let type_engine = ctx.type_engine;

        let TypeParameter {
            initial_type_id,
            name_ident,
            mut trait_constraints,
            trait_constraints_span,
            ..
        } = type_parameter;

        // Type check the trait constraints.
        for trait_constraint in trait_constraints.iter_mut() {
            check!(
                trait_constraint.type_check(ctx.by_ref()),
                return err(warnings, errors),
                warnings,
                errors
            );
        }

        // TODO: add check here to see if the type parameter has a valid name and does not have type parameters

        // Create the "body type param".
        let body_type_id = type_engine.insert(TypeInfo::TypeParam(n));
        let body_type_param = TypeParameter {
            type_id: body_type_id,
            initial_type_id,
            name_ident: name_ident.clone(),
            trait_constraints: trait_constraints.clone(),
            trait_constraints_span: trait_constraints_span.clone(),
        };

        // Insert the trait constraints into the namespace using the "body type
        // param".
        for trait_constraint in trait_constraints.iter() {
            check!(
                TraitConstraint::insert_into_namespace(
                    ctx.by_ref(),
                    body_type_id,
                    trait_constraint
                ),
                return err(warnings, errors),
                warnings,
                errors
            );
        }

        // Insert the "body type param" into the namespace as a dummy
        // declaration.
        ctx.namespace
            .insert_symbol(
                name_ident.clone(),
                ty::TyDeclaration::GenericTypeForFunctionScope {
                    name: name_ident.clone(),
                    type_id: body_type_id,
                },
            )
            .ok(&mut warnings, &mut errors);

        // Create the "subst type param".
        let type_id = type_engine.insert(TypeInfo::UnknownGeneric {
            name: name_ident.clone(),
            trait_constraints: VecSet(trait_constraints.clone()),
        });
        let subst_type_param = TypeParameter {
            type_id,
            initial_type_id,
            name_ident,
            trait_constraints,
            trait_constraints_span,
        };

        ok((body_type_param, subst_type_param), warnings, errors)
    }
}

fn handle_trait(
    mut ctx: TypeCheckContext,
    type_id: TypeId,
    trait_name: &CallPath,
    type_arguments: &[TypeArgument],
) -> CompileResult<(MethodMap, MethodMap)> {
    let mut warnings = vec![];
    let mut errors = vec![];

    let decl_engine = ctx.decl_engine;

    let mut original_method_refs: MethodMap = BTreeMap::new();
    let mut impld_method_refs: MethodMap = BTreeMap::new();

    match ctx
        .namespace
        .resolve_call_path(trait_name)
        .ok(&mut warnings, &mut errors)
        .cloned()
    {
        Some(ty::TyDeclaration::TraitDeclaration { decl_id, .. }) => {
            let trait_decl = check!(
                CompileResult::from(decl_engine.get_trait(&decl_id, &trait_name.suffix.span())),
                return err(warnings, errors),
                warnings,
                errors
            );

            let (trait_original_method_refs, trait_method_refs, trait_impld_method_refs) = check!(
                trait_decl.retrieve_interface_surface_and_methods_and_implemented_methods_for_type(
                    ctx.by_ref(),
                    type_id,
                    trait_name,
                    type_arguments
                ),
                return err(warnings, errors),
                warnings,
                errors
            );
            original_method_refs.extend(trait_original_method_refs);
            original_method_refs.extend(trait_method_refs);
            impld_method_refs.extend(trait_impld_method_refs);

            for supertrait in trait_decl.supertraits.iter() {
                let (supertrait_original_method_refs, supertrait_impld_method_refs) = check!(
                    handle_trait(ctx.by_ref(), type_id, &supertrait.name, &[]),
                    continue,
                    warnings,
                    errors
                );
                original_method_refs.extend(supertrait_original_method_refs);
                impld_method_refs.extend(supertrait_impld_method_refs);
            }
        }
        _ => errors.push(CompileError::TraitNotFound {
            name: trait_name.to_string(),
            span: trait_name.span(),
        }),
    }

    if errors.is_empty() {
        ok((original_method_refs, impld_method_refs), warnings, errors)
    } else {
        err(warnings, errors)
    }
}
