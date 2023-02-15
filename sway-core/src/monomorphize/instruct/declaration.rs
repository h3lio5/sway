use sway_error::handler::{ErrorEmitted, Handler};
use sway_types::Span;

use crate::{decl_engine::DeclId, language::ty, monomorphize::priv_prelude::*, TypeSubstList};

pub(crate) fn instruct_decl(
    ctx: InstructContext,
    handler: &Handler,
    decl: &ty::TyDeclaration,
) -> Result<(), ErrorEmitted> {
    match decl {
        ty::TyDeclaration::VariableDeclaration(decl) => {
            instruct_exp(ctx, handler, &decl.body)?;
        }
        ty::TyDeclaration::ConstantDeclaration { .. } => todo!(),
        ty::TyDeclaration::FunctionDeclaration {
            name,
            decl_id,
            type_subst_list,
            decl_span,
        } => {
            instruct_fn_decl(ctx, handler, decl_id, type_subst_list.inner(), decl_span)?;
        }
        ty::TyDeclaration::TraitDeclaration { .. } => todo!(),
        ty::TyDeclaration::StructDeclaration { .. } => todo!(),
        ty::TyDeclaration::EnumDeclaration { .. } => todo!(),
        ty::TyDeclaration::ImplTrait { .. } => todo!(),
        ty::TyDeclaration::AbiDeclaration { .. } => todo!(),
        ty::TyDeclaration::GenericTypeForFunctionScope { .. } => todo!(),
        ty::TyDeclaration::StorageDeclaration { .. } => todo!(),
        ty::TyDeclaration::ErrorRecovery(_) => {}
    }

    Ok(())
}

fn instruct_fn_decl(
    mut ctx: InstructContext,
    handler: &Handler,
    decl_id: &DeclId,
    type_subst_list: &TypeSubstList,
    decl_span: &Span,
) -> Result<(), ErrorEmitted> {
    let decl = ctx
        .decl_engine
        .get_function(decl_id, decl_span)
        .map_err(|err| handler.emit_err(err))?;

    if !type_subst_list.is_empty() {
        unimplemented!("{}", decl.name);
    }

    let ty::TyFunctionDeclaration {
        body,
        parameters,
        return_type,
        ..
    } = decl;

    // NOTE: todo here
    instruct_code_block(ctx.by_ref(), handler, &body)?;

    Ok(())
}
