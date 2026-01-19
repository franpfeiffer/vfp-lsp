mod cursor;
mod token;

#[cfg(test)]
mod tests;

pub use token::{LiteralKind, TokenKind};

use cursor::Cursor;

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    pub fn new(kind: TokenKind, len: u32) -> Self {
        Self { kind, len }
    }
}

pub fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

pub fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

pub fn is_keyword(s: &str) -> bool {
    let upper = s.to_ascii_uppercase();
    matches!(
        upper.as_str(),
        "IF" | "ELSE"
            | "ELSEIF"
            | "ENDIF"
            | "DO"
            | "WHILE"
            | "ENDDO"
            | "FOR"
            | "EACH"
            | "ENDFOR"
            | "NEXT"
            | "TO"
            | "STEP"
            | "IN"
            | "CASE"
            | "OTHERWISE"
            | "ENDCASE"
            | "SCAN"
            | "ENDSCAN"
            | "EXIT"
            | "LOOP"
            | "RETURN"
            | "TRY"
            | "CATCH"
            | "FINALLY"
            | "ENDTRY"
            | "THROW"
            | "FUNCTION"
            | "ENDFUNC"
            | "PROCEDURE"
            | "ENDPROC"
            | "LPARAMETERS"
            | "PARAMETERS"
            | "CLASS"
            | "ENDDEFINE"
            | "DEFINE"
            | "AS"
            | "OF"
            | "OLEPUBLIC"
            | "THIS"
            | "THISFORM"
            | "THISFORMSET"
            | "DODEFAULT"
            | "NODEFAULT"
            | "PROTECTED"
            | "HIDDEN"
            | "LOCAL"
            | "PRIVATE"
            | "PUBLIC"
            | "DIMENSION"
            | "DECLARE"
            | "EXTERNAL"
            | "WITH"
            | "ENDWITH"
            | "TEXT"
            | "ENDTEXT"
            | "NULL"
            | "USE"
            | "ALIAS"
            | "EXCLUSIVE"
            | "SHARED"
            | "NOUPDATE"
            | "INDEX"
            | "TAG"
            | "DESCENDING"
            | "ASCENDING"
            | "DATABASE"
            | "DATABASES"
            | "TABLES"
            | "INDEXES"
            | "OPEN"
            | "CLOSE"
            | "CLEAR"
            | "STORE"
            | "RELEASE"
            | "NOVALIDATE"
            | "THEN"
    )
}

pub fn is_sql_keyword(s: &str) -> bool {
    let upper = s.to_ascii_uppercase();
    matches!(
        upper.as_str(),
        "SELECT"
            | "FROM"
            | "WHERE"
            | "ORDER"
            | "BY"
            | "GROUP"
            | "HAVING"
            | "INTO"
            | "CURSOR"
            | "TABLE"
            | "ARRAY"
            | "INSERT"
            | "UPDATE"
            | "DELETE"
            | "SET"
            | "VALUES"
            | "JOIN"
            | "INNER"
            | "LEFT"
            | "RIGHT"
            | "OUTER"
            | "FULL"
            | "CROSS"
            | "ON"
            | "DISTINCT"
            | "TOP"
            | "UNION"
            | "ALL"
            | "LIKE"
            | "BETWEEN"
            | "IS"
            | "CREATE"
            | "ALTER"
            | "DROP"
            | "ADD"
            | "COLUMN"
            | "AND"
            | "OR"
            | "NOT"
            | "NULL"
            | "ASC"
            | "DESC"
            | "COUNT"
            | "SUM"
            | "AVG"
            | "MIN"
            | "MAX"
            | "NOFILTER"
            | "READWRITE"
    )
}
