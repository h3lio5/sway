use insta::*;

use crate::common::parse;

mod common;

#[test]
fn noop_script() {
    assert_ron_snapshot!(parse(r#"
      script;
      
      fn main() {
        ()
      }
    "#,), @r###"
    Some(Annotated(
      attribute_list: [],
      value: Module(
        kind: Script(
          script_token: ScriptToken(
            span: Span(
              start: 7,
              end: 13,
            ),
          ),
        ),
        semicolon_token: SemicolonToken(
          span: Span(
            start: 13,
            end: 14,
          ),
        ),
        items: [
          Annotated(
            attribute_list: [],
            value: Fn(ItemFn(
              fn_signature: FnSignature(
                visibility: None,
                fn_token: FnToken(
                  span: Span(
                    start: 28,
                    end: 30,
                  ),
                ),
                name: BaseIdent(
                  name_override_opt: None,
                  span: Span(
                    start: 31,
                    end: 35,
                  ),
                  is_raw_ident: false,
                ),
                generics: None,
                arguments: Parens(
                  inner: Static(Punctuated(
                    value_separator_pairs: [],
                    final_value_opt: None,
                  )),
                  span: Span(
                    start: 35,
                    end: 37,
                  ),
                ),
                return_type_opt: None,
                where_clause_opt: None,
              ),
              body: Braces(
                inner: CodeBlockContents(
                  statements: [],
                  final_expr_opt: Some(Tuple(Parens(
                    inner: Nil,
                    span: Span(
                      start: 48,
                      end: 50,
                    ),
                  ))),
                ),
                span: Span(
                  start: 38,
                  end: 58,
                ),
              ),
            )),
          ),
        ],
      ),
    ))
    "###);
}
