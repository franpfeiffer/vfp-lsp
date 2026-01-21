//! Tests for the VFP lexer.

use crate::{LiteralKind, Token, TokenKind, tokenize};

fn lex(source: &str) -> Vec<Token> {
    tokenize(source).collect()
}

fn lex_kinds(source: &str) -> Vec<TokenKind> {
    tokenize(source).map(|t| t.kind).collect()
}

fn lex_with_text(source: &str) -> Vec<(TokenKind, &str)> {
    let mut result = Vec::new();
    let mut pos = 0;
    for token in tokenize(source) {
        let text = &source[pos..pos + token.len as usize];
        result.push((token.kind, text));
        pos += token.len as usize;
    }
    result
}

#[test]
fn test_whitespace() {
    let tokens = lex_with_text("   \t  ");
    assert_eq!(tokens, vec![(TokenKind::Whitespace, "   \t  ")]);
}

#[test]
fn test_newlines() {
    let tokens = lex_kinds("a\nb\r\nc");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Ident,
            TokenKind::Newline,
            TokenKind::Ident,
            TokenKind::Newline,
            TokenKind::Ident,
        ]
    );
}

#[test]
fn test_line_continuation() {
    let tokens = lex_kinds("SELECT ;\n  FROM");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Ident,
            TokenKind::Whitespace,
            TokenKind::LineContinuation,
            TokenKind::Whitespace,
            TokenKind::Ident,
        ]
    );
}

#[test]
fn test_line_continuation_with_spaces() {
    let tokens = lex_kinds("a ;  \nb");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Ident,
            TokenKind::Whitespace,
            TokenKind::LineContinuation,
            TokenKind::Ident,
        ]
    );
}

#[test]
fn test_star_comment() {
    let tokens = lex_with_text("* this is a comment\ncode");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::CommentStar, "* this is a comment"),
            (TokenKind::Newline, "\n"),
            (TokenKind::Ident, "code"),
        ]
    );
}

#[test]
fn test_star_not_at_line_start() {
    let tokens = lex_kinds("a * b");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Ident,
            TokenKind::Whitespace,
            TokenKind::Star,
            TokenKind::Whitespace,
            TokenKind::Ident,
        ]
    );
}

#[test]
fn test_ampamp_comment() {
    let tokens = lex_with_text("code && this is a comment\nmore");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Ident, "code"),
            (TokenKind::Whitespace, " "),
            (TokenKind::CommentAmpAmp, "&& this is a comment"),
            (TokenKind::Newline, "\n"),
            (TokenKind::Ident, "more"),
        ]
    );
}

#[test]
fn test_note_comment() {
    let tokens = lex_with_text("NOTE this is a note comment\ncode");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::CommentNote, "NOTE this is a note comment"),
            (TokenKind::Newline, "\n"),
            (TokenKind::Ident, "code"),
        ]
    );
}

#[test]
fn test_note_as_identifier() {
    let tokens = lex_kinds("NOTEPAD");
    assert_eq!(tokens, vec![TokenKind::Ident]);
}

#[test]
fn test_boolean_literals() {
    let tokens = lex_with_text(".T. .F. .TRUE. .FALSE. .NULL.");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::True, ".T."),
            (TokenKind::Whitespace, " "),
            (TokenKind::False, ".F."),
            (TokenKind::Whitespace, " "),
            (TokenKind::True, ".TRUE."),
            (TokenKind::Whitespace, " "),
            (TokenKind::False, ".FALSE."),
            (TokenKind::Whitespace, " "),
            (TokenKind::Null, ".NULL."),
        ]
    );
}

#[test]
fn test_boolean_case_insensitive() {
    let tokens = lex_kinds(".t. .f. .True. .false.");
    assert_eq!(
        tokens,
        vec![
            TokenKind::True,
            TokenKind::Whitespace,
            TokenKind::False,
            TokenKind::Whitespace,
            TokenKind::True,
            TokenKind::Whitespace,
            TokenKind::False,
        ]
    );
}

#[test]
fn test_dot_operators() {
    let tokens = lex_with_text(".AND. .OR. .NOT.");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::DotAnd, ".AND."),
            (TokenKind::Whitespace, " "),
            (TokenKind::DotOr, ".OR."),
            (TokenKind::Whitespace, " "),
            (TokenKind::DotNot, ".NOT."),
        ]
    );
}

#[test]
fn test_preprocessor() {
    let tokens = lex_with_text("#DEFINE #IF #ELSE #ENDIF #INCLUDE");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::PreDefine, "#DEFINE"),
            (TokenKind::Whitespace, " "),
            (TokenKind::PreIf, "#IF"),
            (TokenKind::Whitespace, " "),
            (TokenKind::PreElse, "#ELSE"),
            (TokenKind::Whitespace, " "),
            (TokenKind::PreEndIf, "#ENDIF"),
            (TokenKind::Whitespace, " "),
            (TokenKind::PreInclude, "#INCLUDE"),
        ]
    );
}

#[test]
fn test_preprocessor_ifdef_ifndef() {
    let tokens = lex_kinds("#IFDEF #IFNDEF #ELIF #UNDEF");
    assert_eq!(
        tokens,
        vec![
            TokenKind::PreIfDef,
            TokenKind::Whitespace,
            TokenKind::PreIfNDef,
            TokenKind::Whitespace,
            TokenKind::PreElif,
            TokenKind::Whitespace,
            TokenKind::PreUndef,
        ]
    );
}

#[test]
fn test_hash_not_preprocessor() {
    let tokens = lex_kinds("# 1");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Hash,
            TokenKind::Whitespace,
            TokenKind::Literal {
                kind: LiteralKind::Int
            }
        ]
    );
}

#[test]
fn test_comparison_operators() {
    let tokens = lex_with_text("= == != <> < <= > >=");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Eq, "="),
            (TokenKind::Whitespace, " "),
            (TokenKind::EqEq, "=="),
            (TokenKind::Whitespace, " "),
            (TokenKind::BangEq, "!="),
            (TokenKind::Whitespace, " "),
            (TokenKind::BangEq, "<>"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Lt, "<"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LtEq, "<="),
            (TokenKind::Whitespace, " "),
            (TokenKind::Gt, ">"),
            (TokenKind::Whitespace, " "),
            (TokenKind::GtEq, ">="),
        ]
    );
}

#[test]
fn test_arithmetic_operators() {
    let tokens = lex_with_text("+ - * / % ^ **");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Plus, "+"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Minus, "-"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Star, "*"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Slash, "/"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Percent, "%"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Caret, "^"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Caret, "**"),
        ]
    );
}

#[test]
fn test_special_operators() {
    let tokens = lex_with_text("$ @ ! -> << >>");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Dollar, "$"),
            (TokenKind::Whitespace, " "),
            (TokenKind::At, "@"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Bang, "!"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Arrow, "->"),
            (TokenKind::Whitespace, " "),
            (TokenKind::TextMergeOpen, "<<"),
            (TokenKind::Whitespace, " "),
            (TokenKind::TextMergeClose, ">>"),
        ]
    );
}

#[test]
fn test_delimiters() {
    let tokens = lex_with_text("( ) [ ] { } , . : ::");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::LParen, "("),
            (TokenKind::Whitespace, " "),
            (TokenKind::RParen, ")"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LBracket, "["),
            (TokenKind::Whitespace, " "),
            (TokenKind::RBracket, "]"),
            (TokenKind::Whitespace, " "),
            (TokenKind::LBrace, "{"),
            (TokenKind::Whitespace, " "),
            (TokenKind::RBrace, "}"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Comma, ","),
            (TokenKind::Whitespace, " "),
            (TokenKind::Dot, "."),
            (TokenKind::Whitespace, " "),
            (TokenKind::Colon, ":"),
            (TokenKind::Whitespace, " "),
            (TokenKind::ColonColon, "::"),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Numbers
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_integers() {
    let tokens = lex_with_text("123 0 999");
    assert_eq!(
        tokens,
        vec![
            (
                TokenKind::Literal {
                    kind: LiteralKind::Int
                },
                "123"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Int
                },
                "0"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Int
                },
                "999"
            ),
        ]
    );
}

#[test]
fn test_floats() {
    let tokens = lex_with_text("1.5 3.14159 0.001");
    assert_eq!(
        tokens,
        vec![
            (
                TokenKind::Literal {
                    kind: LiteralKind::Float
                },
                "1.5"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Float
                },
                "3.14159"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Float
                },
                "0.001"
            ),
        ]
    );
}

#[test]
fn test_scientific_notation() {
    let tokens = lex_with_text("1e10 1.5e-3 2E+5");
    assert_eq!(
        tokens,
        vec![
            (
                TokenKind::Literal {
                    kind: LiteralKind::Float
                },
                "1e10"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Float
                },
                "1.5e-3"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Float
                },
                "2E+5"
            ),
        ]
    );
}

#[test]
fn test_hex() {
    let tokens = lex_with_text("0xFF 0x1A2B 0hABCD");
    assert_eq!(
        tokens,
        vec![
            (
                TokenKind::Literal {
                    kind: LiteralKind::Hex
                },
                "0xFF"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Hex
                },
                "0x1A2B"
            ),
            (TokenKind::Whitespace, " "),
            (
                TokenKind::Literal {
                    kind: LiteralKind::Hex
                },
                "0hABCD"
            ),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Strings
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_double_quoted_string() {
    let tokens = lex_with_text(r#""hello world""#);
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::StringDouble { terminated: true }
            },
            r#""hello world""#
        )]
    );
}

#[test]
fn test_single_quoted_string() {
    let tokens = lex_with_text("'hello world'");
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::StringSingle { terminated: true }
            },
            "'hello world'"
        )]
    );
}

#[test]
fn test_string_with_backslashes() {
    // VFP doesn't use backslash escapes - backslashes are literal characters
    // This is important for file paths like "C:\Data\"
    let tokens = lex_with_text(r#""C:\Data\file.txt""#);
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::StringDouble { terminated: true }
            },
            r#""C:\Data\file.txt""#
        )]
    );

    // Test trailing backslash (common in path strings)
    let tokens = lex_with_text(r#""C:\Data\""#);
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::StringDouble { terminated: true }
            },
            r#""C:\Data\""#
        )]
    );
}

#[test]
fn test_unterminated_string() {
    let tokens = lex_with_text("\"hello\nworld");
    assert_eq!(
        tokens,
        vec![
            (
                TokenKind::Literal {
                    kind: LiteralKind::StringDouble { terminated: false }
                },
                "\"hello"
            ),
            (TokenKind::Newline, "\n"),
            (TokenKind::Ident, "world"),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Date Literals
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_date_literal() {
    let tokens = lex_with_text("{^2024-01-15}");
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::Date
            },
            "{^2024-01-15}"
        )]
    );
}

#[test]
fn test_datetime_literal() {
    let tokens = lex_with_text("{^2024-01-15 10:30:00}");
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::DateTime
            },
            "{^2024-01-15 10:30:00}"
        )]
    );
}

#[test]
fn test_datetime_literal_with_t() {
    let tokens = lex_with_text("{^2024-01-15T10:30:00}");
    assert_eq!(
        tokens,
        vec![(
            TokenKind::Literal {
                kind: LiteralKind::DateTime
            },
            "{^2024-01-15T10:30:00}"
        )]
    );
}

#[test]
fn test_brace_not_date() {
    // Just a brace without ^
    let tokens = lex_kinds("{1, 2, 3}");
    assert_eq!(
        tokens,
        vec![
            TokenKind::LBrace,
            TokenKind::Literal {
                kind: LiteralKind::Int
            },
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::Literal {
                kind: LiteralKind::Int
            },
            TokenKind::Comma,
            TokenKind::Whitespace,
            TokenKind::Literal {
                kind: LiteralKind::Int
            },
            TokenKind::RBrace,
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Identifiers
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_identifiers() {
    let tokens = lex_with_text("myVar _private MyClass123 m_nCount");
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Ident, "myVar"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Ident, "_private"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Ident, "MyClass123"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Ident, "m_nCount"),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Real VFP Code
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_function_definition() {
    let source = r#"FUNCTION GetName(nId)
    LOCAL cName
    cName = ""
    RETURN cName
ENDFUNC"#;

    let tokens = lex(source);
    assert!(!tokens.is_empty());

    // Check that we get the expected structure
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert!(kinds.contains(&TokenKind::Ident)); // FUNCTION, GetName, etc.
    assert!(kinds.contains(&TokenKind::LParen));
    assert!(kinds.contains(&TokenKind::RParen));
    assert!(kinds.contains(&TokenKind::Eq));
}

#[test]
fn test_sql_select() {
    let source = r#"SELECT name, id ;
    FROM customers ;
    WHERE active = .T. ;
    INTO CURSOR csrResult"#;

    let tokens = lex(source);
    assert!(!tokens.is_empty());

    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
    assert!(kinds.contains(&TokenKind::True)); // .T.
    assert!(kinds.contains(&TokenKind::LineContinuation));
}

#[test]
fn test_class_definition() {
    let source = r#"DEFINE CLASS MyButton AS CommandButton
    Caption = "Click"

    PROCEDURE Click
        MESSAGEBOX("Clicked!")
    ENDPROC
ENDDEFINE"#;

    let tokens = lex(source);
    assert!(!tokens.is_empty());
}

#[test]
fn test_preprocessor_in_context() {
    let source = r#"#DEFINE MAX_ITEMS 100
#IF VERSION(5) >= 900
    * VFP 9 code
#ELSE
    * Older version
#ENDIF"#;

    let tokens = lex(source);
    let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();

    assert!(kinds.contains(&TokenKind::PreDefine));
    assert!(kinds.contains(&TokenKind::PreIf));
    assert!(kinds.contains(&TokenKind::PreElse));
    assert!(kinds.contains(&TokenKind::PreEndIf));
}

#[test]
fn test_member_access() {
    let source = "this.Name = thisform.Caption";
    let tokens = lex_with_text(source);

    // Should tokenize as: this . Name = thisform . Caption
    assert_eq!(
        tokens,
        vec![
            (TokenKind::Ident, "this"),
            (TokenKind::Dot, "."),
            (TokenKind::Ident, "Name"),
            (TokenKind::Whitespace, " "),
            (TokenKind::Eq, "="),
            (TokenKind::Whitespace, " "),
            (TokenKind::Ident, "thisform"),
            (TokenKind::Dot, "."),
            (TokenKind::Ident, "Caption"),
        ]
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// Edge Cases
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn test_empty_input() {
    let tokens = lex("");
    assert!(tokens.is_empty());
}

#[test]
fn test_unknown_character() {
    // α is a single character (2 UTF-8 bytes), lexer consumes it as one unknown token
    let tokens = lex_kinds("α");
    assert_eq!(tokens, vec![TokenKind::Unknown]);
}

#[test]
fn test_dot_followed_by_number() {
    // .5 should be Dot + number, not a float
    let tokens = lex_kinds(".5");
    assert_eq!(
        tokens,
        vec![
            TokenKind::Dot,
            TokenKind::Literal {
                kind: LiteralKind::Int
            }
        ]
    );
}
