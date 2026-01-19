use vfp_lexer::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    WHITESPACE,
    NEWLINE,
    LINE_CONTINUATION,
    COMMENT_STAR,
    COMMENT_AMPAMP,
    COMMENT_NOTE,

    IDENT,
    INT_NUMBER,
    FLOAT_NUMBER,
    HEX_NUMBER,
    STRING,
    DATE_LITERAL,
    DATETIME_LITERAL,

    TRUE_KW,
    FALSE_KW,
    NULL_KW,

    DOT_AND,
    DOT_OR,
    DOT_NOT,

    PRE_DEFINE,
    PRE_UNDEF,
    PRE_IF,
    PRE_IFDEF,
    PRE_IFNDEF,
    PRE_ELSE,
    PRE_ELIF,
    PRE_ENDIF,
    PRE_INCLUDE,

    IF_KW,
    ELSE_KW,
    ELSEIF_KW,
    ENDIF_KW,
    DO_KW,
    WHILE_KW,
    ENDDO_KW,
    FOR_KW,
    EACH_KW,
    ENDFOR_KW,
    NEXT_KW,
    TO_KW,
    STEP_KW,
    IN_KW,
    CASE_KW,
    OTHERWISE_KW,
    ENDCASE_KW,
    SCAN_KW,
    ENDSCAN_KW,
    EXIT_KW,
    LOOP_KW,
    RETURN_KW,
    TRY_KW,
    CATCH_KW,
    FINALLY_KW,
    ENDTRY_KW,
    THROW_KW,
    FUNCTION_KW,
    ENDFUNC_KW,
    PROCEDURE_KW,
    ENDPROC_KW,
    LPARAMETERS_KW,
    PARAMETERS_KW,
    CLASS_KW,
    DEFINE_KW,
    ENDDEFINE_KW,
    AS_KW,
    OF_KW,
    OLEPUBLIC_KW,
    THIS_KW,
    THISFORM_KW,
    THISFORMSET_KW,
    DODEFAULT_KW,
    NODEFAULT_KW,
    PROTECTED_KW,
    HIDDEN_KW,
    LOCAL_KW,
    PRIVATE_KW,
    PUBLIC_KW,
    DIMENSION_KW,
    DECLARE_KW,
    EXTERNAL_KW,
    WITH_KW,
    ENDWITH_KW,
    TEXT_KW,
    ENDTEXT_KW,
    THEN_KW,

    SELECT_KW,
    FROM_KW,
    WHERE_KW,
    ORDER_KW,
    BY_KW,
    GROUP_KW,
    HAVING_KW,
    INTO_KW,
    CURSOR_KW,
    TABLE_KW,
    ARRAY_KW,
    INSERT_KW,
    UPDATE_KW,
    DELETE_KW,
    SET_KW,
    VALUES_KW,
    JOIN_KW,
    INNER_KW,
    LEFT_KW,
    RIGHT_KW,
    OUTER_KW,
    ON_KW,
    DISTINCT_KW,
    TOP_KW,
    UNION_KW,
    ALL_KW,
    AND_KW,
    OR_KW,
    NOT_KW,
    LIKE_KW,
    BETWEEN_KW,
    IS_KW,

    USE_KW,
    ALIAS_KW,
    INDEX_KW,
    TAG_KW,

    EQ,
    EQEQ,
    BANGEQ,
    HASH,
    LT,
    LTEQ,
    GT,
    GTEQ,
    DOLLAR,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    CARET,
    BANG,
    AT,
    PIPE,
    AMP,

    L_PAREN,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    L_BRACE,
    R_BRACE,
    COMMA,
    DOT,
    COLON,
    COLONCOLON,
    SEMI,
    ARROW,
    TEXTMERGE_OPEN,
    TEXTMERGE_CLOSE,

    EOF,
    ERROR,

    SOURCE_FILE,
    STMT,
    EXPR,
    BIN_EXPR,
    UNARY_EXPR,
    PAREN_EXPR,
    CALL_EXPR,
    INDEX_EXPR,
    MEMBER_EXPR,
    NAME_REF,
    LITERAL,
    ARG_LIST,
    PARAM_LIST,
    PARAM,
    FUNCTION_DEF,
    PROCEDURE_DEF,
    CLASS_DEF,
    IF_STMT,
    DO_WHILE_STMT,
    FOR_STMT,
    FOR_EACH_STMT,
    SCAN_STMT,
    DO_CASE_STMT,
    CASE_CLAUSE,
    OTHERWISE_CLAUSE,
    TRY_STMT,
    CATCH_CLAUSE,
    FINALLY_CLAUSE,
    WITH_STMT,
    TEXT_STMT,
    VAR_DECL,
    ASSIGN_STMT,
    RETURN_STMT,
    SQL_SELECT,
    SQL_INSERT,
    SQL_UPDATE,
    SQL_DELETE,
    PREPROCESSOR,
    BLOCK,
}

impl SyntaxKind {
    pub fn from_token(token: TokenKind) -> Self {
        use vfp_lexer::LiteralKind;

        match token {
            TokenKind::Whitespace => SyntaxKind::WHITESPACE,
            TokenKind::Newline => SyntaxKind::NEWLINE,
            TokenKind::LineContinuation => SyntaxKind::LINE_CONTINUATION,
            TokenKind::CommentStar => SyntaxKind::COMMENT_STAR,
            TokenKind::CommentAmpAmp => SyntaxKind::COMMENT_AMPAMP,
            TokenKind::CommentNote => SyntaxKind::COMMENT_NOTE,

            TokenKind::Ident => SyntaxKind::IDENT,
            TokenKind::Literal { kind } => match kind {
                LiteralKind::Int => SyntaxKind::INT_NUMBER,
                LiteralKind::Float => SyntaxKind::FLOAT_NUMBER,
                LiteralKind::Hex => SyntaxKind::HEX_NUMBER,
                LiteralKind::StringDouble { .. }
                | LiteralKind::StringSingle { .. }
                | LiteralKind::StringBracket { .. } => SyntaxKind::STRING,
                LiteralKind::Date => SyntaxKind::DATE_LITERAL,
                LiteralKind::DateTime => SyntaxKind::DATETIME_LITERAL,
            },

            TokenKind::True => SyntaxKind::TRUE_KW,
            TokenKind::False => SyntaxKind::FALSE_KW,
            TokenKind::Null => SyntaxKind::NULL_KW,

            TokenKind::DotAnd => SyntaxKind::DOT_AND,
            TokenKind::DotOr => SyntaxKind::DOT_OR,
            TokenKind::DotNot => SyntaxKind::DOT_NOT,

            TokenKind::PreDefine => SyntaxKind::PRE_DEFINE,
            TokenKind::PreUndef => SyntaxKind::PRE_UNDEF,
            TokenKind::PreIf => SyntaxKind::PRE_IF,
            TokenKind::PreIfDef => SyntaxKind::PRE_IFDEF,
            TokenKind::PreIfNDef => SyntaxKind::PRE_IFNDEF,
            TokenKind::PreElse => SyntaxKind::PRE_ELSE,
            TokenKind::PreElif => SyntaxKind::PRE_ELIF,
            TokenKind::PreEndIf => SyntaxKind::PRE_ENDIF,
            TokenKind::PreInclude => SyntaxKind::PRE_INCLUDE,

            TokenKind::Eq => SyntaxKind::EQ,
            TokenKind::EqEq => SyntaxKind::EQEQ,
            TokenKind::BangEq => SyntaxKind::BANGEQ,
            TokenKind::Hash => SyntaxKind::HASH,
            TokenKind::Lt => SyntaxKind::LT,
            TokenKind::LtEq => SyntaxKind::LTEQ,
            TokenKind::Gt => SyntaxKind::GT,
            TokenKind::GtEq => SyntaxKind::GTEQ,
            TokenKind::Dollar => SyntaxKind::DOLLAR,
            TokenKind::Plus => SyntaxKind::PLUS,
            TokenKind::Minus => SyntaxKind::MINUS,
            TokenKind::Star => SyntaxKind::STAR,
            TokenKind::Slash => SyntaxKind::SLASH,
            TokenKind::Percent => SyntaxKind::PERCENT,
            TokenKind::Caret => SyntaxKind::CARET,
            TokenKind::Bang => SyntaxKind::BANG,
            TokenKind::At => SyntaxKind::AT,
            TokenKind::Pipe => SyntaxKind::PIPE,
            TokenKind::Amp => SyntaxKind::AMP,

            TokenKind::LParen => SyntaxKind::L_PAREN,
            TokenKind::RParen => SyntaxKind::R_PAREN,
            TokenKind::LBracket => SyntaxKind::L_BRACKET,
            TokenKind::RBracket => SyntaxKind::R_BRACKET,
            TokenKind::LBrace => SyntaxKind::L_BRACE,
            TokenKind::RBrace => SyntaxKind::R_BRACE,
            TokenKind::Comma => SyntaxKind::COMMA,
            TokenKind::Dot => SyntaxKind::DOT,
            TokenKind::Colon => SyntaxKind::COLON,
            TokenKind::ColonColon => SyntaxKind::COLONCOLON,
            TokenKind::Semi => SyntaxKind::SEMI,
            TokenKind::Arrow => SyntaxKind::ARROW,
            TokenKind::TextMergeOpen => SyntaxKind::TEXTMERGE_OPEN,
            TokenKind::TextMergeClose => SyntaxKind::TEXTMERGE_CLOSE,

            TokenKind::Eof => SyntaxKind::EOF,
            TokenKind::Unknown => SyntaxKind::ERROR,
        }
    }

    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::WHITESPACE
                | SyntaxKind::NEWLINE
                | SyntaxKind::LINE_CONTINUATION
                | SyntaxKind::COMMENT_STAR
                | SyntaxKind::COMMENT_AMPAMP
                | SyntaxKind::COMMENT_NOTE
        )
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        rowan::SyntaxKind(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VfpLanguage {}

impl rowan::Language for VfpLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 < SyntaxKind::BLOCK as u16 + 1);
        unsafe { std::mem::transmute(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<VfpLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<VfpLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<VfpLanguage>;
