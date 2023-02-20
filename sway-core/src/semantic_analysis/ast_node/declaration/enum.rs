use crate::{
    decl_engine::Template,
    error::*,
    language::{parsed::*, ty, CallPath},
    semantic_analysis::*,
    type_system::*,
};

impl ty::TyEnumDeclaration {
    pub fn type_check(
        ctx: TypeCheckContext,
        decl: EnumDeclaration,
    ) -> CompileResult<(ty::TyEnumDeclaration, Template<TypeSubstList>)> {
        let mut errors = vec![];
        let mut warnings = vec![];

        let EnumDeclaration {
            name,
            type_parameters,
            variants,
            span,
            attributes,
            visibility,
            ..
        } = decl;

        // create a namespace for the decl, used to create a scope for generics
        let mut decl_namespace = ctx.namespace.clone();
        let mut ctx = ctx.scoped(&mut decl_namespace);

        // Type check the type parameters. This will also insert them into the
        // current namespace.
        let (new_type_parameters, type_subst_list) = check!(
            TypeParameter::type_check_type_params(ctx.by_ref(), type_parameters, true),
            return err(warnings, errors),
            warnings,
            errors
        );

        // type check the variants
        let mut variants_buf = vec![];
        for variant in variants {
            variants_buf.push(check!(
                ty::TyEnumVariant::type_check(ctx.by_ref(), variant.clone()),
                continue,
                warnings,
                errors
            ));
        }

        let mut call_path: CallPath = name.into();
        call_path = call_path.to_fullpath(ctx.namespace);

        // create the enum decl
        let enum_decl = ty::TyEnumDeclaration {
            call_path,
            type_parameters: new_type_parameters,
            variants: variants_buf,
            span,
            attributes,
            visibility,
        };

        ok((enum_decl, type_subst_list), warnings, errors)
    }
}

impl ty::TyEnumVariant {
    pub(crate) fn type_check(
        mut ctx: TypeCheckContext,
        variant: EnumVariant,
    ) -> CompileResult<Self> {
        let mut warnings = vec![];
        let mut errors = vec![];
        let type_engine = ctx.type_engine;
        let mut type_argument = variant.type_argument;
        type_argument.type_id = check!(
            ctx.resolve_type(
                type_argument.type_id,
                &type_argument.span,
                EnforceTypeArguments::Yes,
                None
            ),
            type_engine.insert(TypeInfo::ErrorRecovery),
            warnings,
            errors,
        );
        ok(
            ty::TyEnumVariant {
                name: variant.name.clone(),
                type_argument,
                tag: variant.tag,
                span: variant.span,
                attributes: variant.attributes,
            },
            vec![],
            errors,
        )
    }
}
