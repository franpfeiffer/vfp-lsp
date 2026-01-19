#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Whitespace,
    Newline,
    LineContinuation,
    CommentStar,
    CommentAmpAmp,
    CommentNote,

    Ident,
    Literal { kind: LiteralKind },

    True,
    False,
    Null,

    DotAnd,
    DotOr,
    DotNot,

    PreDefine,
    PreUndef,
    PreIf,
    PreIfDef,
    PreIfNDef,
    PreElse,
    PreElif,
    PreEndIf,
    PreInclude,

    Eq,
    EqEq,
    BangEq,
    Hash,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Dollar,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Bang,
    At,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Colon,
    ColonColon,
    Semi,
    Arrow,
    Pipe,
    Amp,

    TextMergeOpen,
    TextMergeClose,

    Eof,
    Unknown,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LineContinuation
                | TokenKind::CommentStar
                | TokenKind::CommentAmpAmp
                | TokenKind::CommentNote
        )
    }

    pub fn is_comment(self) -> bool {
        matches!(
            self,
            TokenKind::CommentStar | TokenKind::CommentAmpAmp | TokenKind::CommentNote
        )
    }

    pub fn is_preprocessor(self) -> bool {
        matches!(
            self,
            TokenKind::PreDefine
                | TokenKind::PreUndef
                | TokenKind::PreIf
                | TokenKind::PreIfDef
                | TokenKind::PreIfNDef
                | TokenKind::PreElse
                | TokenKind::PreElif
                | TokenKind::PreEndIf
                | TokenKind::PreInclude
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Int,
    Float,
    Hex,
    StringDouble { terminated: bool },
    StringSingle { terminated: bool },
    StringBracket { terminated: bool },
    Date,
    DateTime,
}
