use sway_error::handler::{ErrorEmitted, Handler};
use sway_types::Span;

use crate::{decl_engine::DeclId, language::ty, monomorphize::priv_prelude::*};

pub(crate) fn gather_from_decl(
    ctx: GatherContext,
    handler: &Handler,
    decl: &ty::TyDeclaration,
) -> Result<(), ErrorEmitted> {
    match decl {
        ty::TyDeclaration::VariableDeclaration(decl) => {
            gather_from_exp(ctx, handler, &decl.body)?;
        }
        ty::TyDeclaration::ConstantDeclaration { .. } => todo!(),
        ty::TyDeclaration::FunctionDeclaration {
            decl_id, decl_span, ..
        } => {
            ctx.add_constraint(decl.into());
            gather_from_fn_decl(ctx, handler, decl_id, decl_span)?;
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

fn gather_from_fn_decl(
    mut ctx: GatherContext,
    handler: &Handler,
    decl_id: &DeclId,
    decl_span: &Span,
) -> Result<(), ErrorEmitted> {
    let decl = ctx
        .decl_engine
        .get_function(decl_id, decl_span)
        .map_err(|err| handler.emit_err(err))?;

    let ty::TyFunctionDeclaration {
        body,
        parameters,
        return_type,
        ..
    } = decl;

    parameters.iter().for_each(|param| {
        ctx.add_constraint(param.type_argument.type_id.into());
    });
    ctx.add_constraint(return_type.type_id.into());
    gather_from_code_block(ctx.by_ref(), handler, &body)?;

    Ok(())
}
