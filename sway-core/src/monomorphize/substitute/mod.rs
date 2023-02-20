use sway_error::{
    error::CompileError,
    handler::{ErrorEmitted, Handler},
};
use sway_types::Span;

use crate::{decl_engine::*, engine_threading::*, language::ty, type_system::*};

pub(crate) fn subst(
    engines: Engines<'_>,
    handler: &Handler,
    decl_id: DeclId,
    subst_list: TypeSubstList,
    decl_span: &Span,
) -> Result<DeclId, ErrorEmitted> {
    let type_engine = engines.te();
    let decl_engine = engines.de();

    let mut decl = decl_engine.get(&decl_id);
    subst_decl(engines, handler, &mut decl, subst_list, decl_span)?;
    Ok(decl_engine.insert_wrapper(type_engine, decl))
}

fn subst_decl(
    engines: Engines<'_>,
    handler: &Handler,
    decl: &mut DeclWrapper,
    subst_list: TypeSubstList,
    decl_span: &Span,
) -> Result<(), ErrorEmitted> {
    match decl {
        DeclWrapper::Function(fn_decl) => {
            subst_fn_decl(engines, handler, fn_decl, subst_list, decl_span)?;
        }
        DeclWrapper::Trait(_) => todo!(),
        DeclWrapper::ImplTrait(_) => todo!(),
        DeclWrapper::Struct(_) => todo!(),
        DeclWrapper::Enum(_) => todo!(),
        // None of these would generate a [TypeSubstList] anyway.
        DeclWrapper::Unknown
        | DeclWrapper::Storage(_)
        | DeclWrapper::Abi(_)
        | DeclWrapper::Constant(_)
        | DeclWrapper::TraitFn(_) => {}
    }

    Ok(())
}

fn subst_fn_decl(
    engines: Engines<'_>,
    handler: &Handler,
    fn_decl: &mut ty::TyFunctionDeclaration,
    subst_list: TypeSubstList,
    decl_span: &Span,
) -> Result<(), ErrorEmitted> {
    let ty::TyFunctionDeclaration {
        body,
        parameters,
        implementing_type,
        type_parameters,
        return_type,
        // these fields are not used because they either do not contain
        // declarations/types or they are used for debugging/error messages
        name: _,
        visibility: _,
        is_contract_call: _,
        purity: _,
        span: _,
        attributes: _,
    } = fn_decl;

    subst_type_arg(engines, handler, return_type, subst_list, decl_span)?;

    Ok(())
}

fn subst_type_arg(
    engines: Engines<'_>,
    handler: &Handler,
    type_arg: &mut TypeArgument,
    subst_list: TypeSubstList,
    decl_span: &Span,
) -> Result<(), ErrorEmitted> {
    let TypeArgument {
        type_id,
        // these fields are not used because they either do not contain
        // declarations/types or they are used for debugging/error messages
        initial_type_id: _,
        span: _,
        call_path_tree: _,
    } = type_arg;

    todo!()
}

fn subst_type_id(
    engines: Engines<'_>,
    handler: &Handler,
    type_id: &mut TypeId,
    subst_list: &TypeSubstList,
    decl_span: &Span,
) -> Result<(), ErrorEmitted> {
    let type_engine = engines.te();

    if let TypeInfo::TypeParam(n) = type_engine.get(*type_id) {
        let elem = subst_list.get(n).ok_or_else(|| {
            handler.emit_err(CompileError::Internal(
                "tried to index outside of the type list",
                decl_span.clone(),
            ))
        })?;
        *type_id = elem.type_id;
        // TODO: recurse here
    }

    Ok(())
}
