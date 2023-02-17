use crate::{
    core::{
        session::Session,
        token::{get_range_from_span, AstToken, SymbolKind},
    },
    error::LanguageServerError,
};
use std::collections::HashMap;
use std::sync::Arc;
use sway_types::Spanned;
use tower_lsp::lsp_types::{Position, PrepareRenameResponse, TextEdit, Url, WorkspaceEdit};

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
        let range = get_range_from_span(&ident.span());
        edits.push(TextEdit::new(range, new_name.clone()));
    }

    let mut map_of_changes = HashMap::new();
    session.sync.to_workspace_url(url).map(|url| {
        map_of_changes.insert(url, edits);
        WorkspaceEdit::new(map_of_changes)
    })
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
        .ok_or_else(|| LanguageServerError::TokenNotFound)?;

    for item in session.token_map().iter() {
        let ((.., span), t) = item.pair();
        // only let through tokens that are in the users workspace.

        // This isn't entirely correct, we should be checking the definition of the token
        // and then using the definitions span to check if it is in the users workspace.
        if let Some(path) = span.path() {
            if !path.starts_with(temp_path) {
                return Err(LanguageServerError::TokenNotPartOfWorkspace);
            }
        }
    }
    // Make sure we don't allow renaming of tokens that
    // are keywords, intrinsics, or tokens that are external
    // to the users workspace.
    session
        .token_map()
        .iter()
        .filter(|item| {
            let t = item.value();
            let res = match t.parsed {
                AstToken::Intrinsic(_) => true,
                _ => false,
            };
            t.kind == SymbolKind::Keyword || res
        })
        .for_each(|item| {
            let (.., span) = item.key();
            if ident.as_str().contains(span.as_str()) {
                println!(
                    "WE GOT PROBELMS {} contains {}",
                    ident.as_str(),
                    span.as_str()
                );
                //TODO return an error here.
            }
        });
    Ok(PrepareRenameResponse::RangeWithPlaceholder {
        range: get_range_from_span(&ident.span()),
        placeholder: ident.as_str().to_string(),
    })
}
