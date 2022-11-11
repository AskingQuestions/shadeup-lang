// use core::fmt::Display;
// use logos::Logos;
// use std::fmt::Formatter;

// #[derive(Debug, PartialEq, Copy, Clone, Eq, Logos, Hash)]
// #[logos(subpattern decimal = r"[0-9][_0-9]*")]
// #[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
// #[logos(subpattern octal = r"[0-7][_0-7]*")]
// #[logos(subpattern binary = r"[0-1][_0-1]*")]
// #[logos(subpattern exp = r"[eE][+-]?[0-9][_0-9]*")]
// pub enum Token {
//     // Tokens can be literal strings, of any length.
//     #[regex(r"//[^\n]*", priority = 2)]
//     #[regex(r"//[^\n]*\n?")]
//     #[regex(r"/\*([^*]|\*+[^*/])*\*+/")]
//     Comment,

//     #[token("import")]
//     Import,

//     #[token("as")]
//     As,

//     #[token("from")]
//     From,

//     #[token("shader")]
//     Shader,

//     #[token("struct")]
//     Struct,

//     #[token("fn")]
//     Fn,

//     #[token("impl")]
//     Impl,

//     #[token("pub")]
//     Pub,

//     #[token("let")]
//     Let,

//     #[token("main")]
//     Main,

//     #[token("return")]
//     Return,

//     #[token("if")]
//     If,

//     #[token("else")]
//     Else,

//     #[token("for")]
//     For,

//     #[token("while")]
//     While,

//     #[token("break")]
//     Break,

//     #[token("continue")]
//     Continue,

//     #[token("true")]
//     True,

//     #[token("false")]
//     False,

//     #[token("null")]
//     Null,

//     #[token("in")]
//     In,

//     #[token("{")]
//     OpenScope,

//     #[token("}")]
//     CloseScope,

//     #[token("[")]
//     OpenSq,

//     #[token("]")]
//     CloseSq,

//     #[token("(")]
//     OpenPar,

//     #[token(")")]
//     ClosePar,

//     #[regex(r#"["]([^"\\\n]|\\(.|\n))*["]"#)]
//     #[regex(r#"[']([^'\\\n]|\\(.|\n))*[']"#)]
//     String,

//     #[regex("(?&decimal)")]
//     Integer,

//     #[regex("0[xX](?&hex)")]
//     HexInteger,

//     #[regex("0[oO](?&octal)")]
//     OctalInteger,

//     #[regex("0[bB](?&binary)")]
//     BinaryInteger,

//     #[regex(r#"[+-]?(((?&decimal)\.(?&decimal)?(?&exp)?[fFdD]?)|(\.(?&decimal)(?&exp)?[fFdD]?)|((?&decimal)(?&exp)[fFdD]?)|((?&decimal)(?&exp)?[fFdD]))"#)]
//     Float,

//     #[regex(r"0[xX](((?&hex))|((?&hex)\.)|((?&hex)?\.(?&hex)))[pP][+-]?(?&decimal)[fFdD]?")]
//     HexFloat,

//     #[token(".")]
//     Dot,

//     #[token(",")]
//     Comma,

//     #[token(":")]
//     Colon,

//     #[token(";")]
//     Semicolon,

//     #[token("=")]
//     Equal,

//     #[token("==", priority = 2)]
//     EqualEqual,

//     #[token("!=")]
//     NotEqual,

//     #[token("+")]
//     Plus,

//     #[token("++")]
//     PlusPlus,

//     #[token("+=")]
//     PlusEqual,

//     #[token("-")]
//     Minus,

//     #[token("--")]
//     MinusMinus,

//     #[token("-=")]
//     MinusEqual,

//     #[token("*")]
//     Star,

//     #[token("*=")]
//     StarEqual,

//     #[token("/")]
//     Slash,

//     #[token("/=")]
//     SlashEqual,

//     #[token("%")]
//     Percent,

//     #[token("%=")]
//     PercentEqual,

//     #[token("<<")]
//     ShiftLeft,

//     #[token("<<=")]
//     ShiftLeftEqual,

//     #[token(">>")]
//     ShiftRight,

//     #[token(">>=")]
//     ShiftRightEqual,

//     #[token("&")]
//     Ampersand,

//     #[token("&&")]
//     AmpersandAmpersand,

//     #[token("&=")]
//     AmpersandEqual,

//     #[token("|")]
//     Pipe,

//     #[token("||")]
//     PipePipe,

//     #[token("|=")]
//     PipeEqual,

//     #[token("^")]
//     Caret,

//     #[token("^=")]
//     CaretEqual,

//     #[token("!")]
//     Bang,

//     #[token("~")]
//     Tilde,

//     #[token("<")]
//     Less,

//     #[token("<=")]
//     LessEqual,

//     #[token(">")]
//     Greater,

//     #[token(">=")]
//     GreaterEqual,

//     #[token("->")]
//     Arrow,

//     #[token("=>")]
//     FatArrow,

//     #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
//     Identifier,

//     // Logos requires one token variant to handle errors,
//     // it can be named anything you wish.
//     #[error]
//     // We can also use this variant to define whitespace,
//     // or any other matches we wish to skip.
//     #[regex(r"[ \t\n\f]+", logos::skip)]
//     Error,
// }

// impl Display for Token {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self)
//     }
// }

// pub type LexerResult<'source> = logos::Lexer<'source, Token>;

// pub fn lex<'source>(input: &'source str) -> logos::Lexer<'source, Token> {
//     Token::lexer(input)
// }
