use crate::{
    core::{
        session::Session,
        token::{get_range_from_span, SymbolKind},
    },
    error::{LanguageServerError, RenameError},
};
use std::collections::HashMap;
use std::sync::Arc;
use sway_types::{Spanned, Span, Ident};
use tower_lsp::lsp_types::{Position, PrepareRenameResponse, TextEdit, Url, WorkspaceEdit};

const RAW_MODIFIER: &str = "r#";

pub fn rename(
    session: Arc<Session>,
    new_name: String,
    url: Url,
    position: Position,
) -> Option<WorkspaceEdit> {
    let (_, token) = session.token_map().token_at_position(&url, position)?;
    let mut edits = Vec::new();

    // todo: currently only supports single file rename
    for (ident, _) in session
        .token_map()
        .all_references_of_token(&token, &session.type_engine.read())
    {
        let span = if ident.is_raw_ident() {
            span_from_raw(&ident.span())?
        } else {
            ident.span()
        };
        let range = get_range_from_span(&span);
        edits.push(TextEdit::new(range, new_name.clone()));
    }

    let mut map_of_changes = HashMap::new();
    session.sync.to_workspace_url(url).map(|url| {
        map_of_changes.insert(url, edits);
        WorkspaceEdit::new(map_of_changes)
    })
}

// TODO add comment
fn span_from_raw(s: &Span) -> Option<Span> {
    Span::new(
        Arc::from(format!("{}{}", RAW_MODIFIER, s.src())),
        s.start()-2,
        s.end(),
        s.path().cloned()
    )
}

pub fn prepare_rename(
    session: Arc<Session>,
    url: Url,
    position: Position,
) -> Result<PrepareRenameResponse, LanguageServerError> {
    let temp_path = &session.sync.temp_dir()?;
    let (ident, token) = session
        .token_map()
        .token_at_position(&url, position)
        .ok_or_else(|| RenameError::TokenNotFound)?;

    // Only let through tokens that are in the users workspace.
    // tokens that are external to the users workspace cannot be renamed.
    let decl_ident = token
        .declared_token_ident(&session.type_engine.read())
        .ok_or_else(|| RenameError::TokenNotFound)?;

    // Check the span of the tokens defintions to determine if it's in the users workspace.
    if let Some(path) = decl_ident.span().path() {
        if !path.starts_with(temp_path) {
            return Err(LanguageServerError::RenameError(
                RenameError::TokenNotPartOfWorkspace,
            ));
        }
    }

    // Make sure we don't allow renaming of tokens that
    // are keywords or intrinsics.
    if matches!(token.kind, SymbolKind::Keyword | SymbolKind::Intrinsic) {
        return Err(LanguageServerError::RenameError(
            RenameError::UnableToRenameKeyword,
        ));
    }

    Ok(PrepareRenameResponse::RangeWithPlaceholder {
        range: get_range_from_span(&ident.span()),
        placeholder: ident.as_str().to_string(),
    })
}
