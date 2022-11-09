#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::{ast, ast::Location, ast::USizeTuple, lexer, lexer::Token, printer};
use colored::Colorize;
use std::string::ToString;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::{collections::HashMap, env, fmt, fs};

pub type Span = std::ops::Range<usize>;

#[derive(Debug, PartialEq, Eq)]
enum BracketType {
    Curly,
    Square,
    Round,
    None,
}

enum ParseState {
    Program(ast::Program),
    Import(ast::Import),
    Block(ast::Block),
}

impl ToString for BracketType {
    fn to_string(&self) -> String {
        match self {
            BracketType::Curly => "{}".to_string(),
            BracketType::Square => "[]".to_string(),
            BracketType::Round => "()".to_string(),
            BracketType::None => "".to_string(),
        }
    }
}

// type Input<'source> = Tokens<'source, Token>;
type BracketStack = Vec<BracketType>;
type StateStack = Vec<ParseState>;

#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub enum AlertType {
    UndefinedSymbol,
    UnexpectedToken,
    UnexpectedEndOfInput,
    UnexpectedEndOfBlock,
    UnexpectedEndOfStatement,
    UnexpectedEndOfExpression,
    UnexpectedEndOfFunction,
    UnexpectedEndOfFunctionCall,
    UnexpectedEndOfStruct,
    UnexpectedEndOfVariable,
    UnexpectedEndOfImport,
    UnexpectedEndOfImportName,
    UnexpectedEndOfImportPath,
    UnexpectedEndOfImportAlias,
    Unspecified,
}

#[derive(Debug, PartialEq, Eq)]
enum Op {
    Number(i64),
    Addition(Box<(Op, Op)>),
    Subtraction(Box<(Op, Op)>),
}

// data_variant_parser! {
//     fn parse_identifier(input) -> Result<Op>;
//     pattern = Token::Identifier(n) => Op::new(n);
// }

#[derive(Debug, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct ParseAlert {
    pub level: printer::AlertLevel,
    message: String,
    pub location: Location,

    pub start_line_and_column: USizeTuple,
    pub end_line_and_column: USizeTuple,

    pub alert_type: AlertType,
    pretty_printed: String,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl ParseAlert {
    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn pretty_printed(&self) -> String {
        self.pretty_printed.clone()
    }

    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn message(&self) -> String {
        self.message.clone()
    }
}

impl ParseAlert {
    pub fn new(
        level: printer::AlertLevel,
        message: String,
        location: Location,
        alert_type: AlertType,
        source: &str,
    ) -> ParseAlert {
        ParseAlert {
            level,
            message: message.clone(),
            location,
            start_line_and_column: location.get_start_line_and_column(source),
            end_line_and_column: location.get_end_line_and_column(source),
            alert_type,
            pretty_printed: printer::format_pretty_alert(source, location, level, &message),
        }
    }
}

pub struct ParseResult<'source> {
    pub alerts: Vec<ParseAlert>,
    pub source: &'source str,
}

impl<'source> ParseResult<'source> {
    pub fn new(source: &'source str) -> ParseResult<'source> {
        ParseResult {
            source,
            alerts: vec![],
        }
    }

    pub fn add_alert(
        &mut self,
        level: printer::AlertLevel,
        message: String,
        location: Location,
        alert_type: AlertType,
    ) {
        self.alerts.push(ParseAlert::new(
            level,
            message,
            location,
            alert_type,
            self.source,
        ));
    }

    pub fn has_alerts(&self) -> bool {
        self.alerts.len() > 0
    }
}

struct Error {
    span: Span,
    msg: String,
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    List(Vec<Value>),
    Func(String),
}

impl Value {
    fn num(self, span: Span) -> Result<f64, Error> {
        if let Value::Num(x) = self {
            Ok(x)
        } else {
            Err(Error {
                span,
                msg: format!("'{}' is not a number", self),
            })
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Num(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
            Self::List(xs) => write!(
                f,
                "[{}]",
                xs.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Func(name) => write!(f, "<function: {}>", name),
        }
    }
}

#[derive(Clone, Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
}

#[derive(Clone)]
enum Instr {
    Left,
    Right,
    Incr,
    Decr,
    Read,
    Write,
    Loop(Vec<Self>),
}

pub type Spanned<T> = (T, Span);
#[derive(Debug)]
enum Expr {
    Error,
    Value(Value),
    List(Vec<Spanned<Self>>),
    Local(String),
    Let(String, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Print(Box<Spanned<Self>>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token2 {
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    Op(String),
    Ctrl(char),
    Ident(String),
    Fn,
    As,
    From,
    Import,
    Shader,
    Struct,
    Main,
    Pub,
    Let,
    Print,
    If,
    Else,
}

impl fmt::Display for Token2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token2::Null => write!(f, "null"),
            Token2::Bool(x) => write!(f, "{}", x),
            Token2::Num(n) => write!(f, "{}", n),
            Token2::Str(s) => write!(f, "{}", s),
            Token2::Op(s) => write!(f, "{}", s),
            Token2::Ctrl(c) => write!(f, "{}", c),
            Token2::Ident(s) => write!(f, "{}", s),
            Token2::Fn => write!(f, "fn"),
            Token2::Let => write!(f, "let"),
            Token2::Print => write!(f, "print"),
            Token2::If => write!(f, "if"),
            Token2::Else => write!(f, "else"),
            Token2::As => write!(f, "as"),
            Token2::From => write!(f, "from"),
            Token2::Import => write!(f, "import"),
            Token2::Shader => write!(f, "shader"),
            Token2::Struct => write!(f, "struct"),
            Token2::Main => write!(f, "main"),
            Token2::Pub => write!(f, "pub"),
        }
    }
}

// fn parser() -> impl Parser<Token, Vec<Instr>, Error = Simple<Token>> + Clone {
//     recursive(|bf| {
//         choice((
//             just('<').to(Instr::Left),
//             just('>').to(Instr::Right),
//             just('+').to(Instr::Incr),
//             just('-').to(Instr::Decr),
//             just(',').to(Instr::Read),
//             just('.').to(Instr::Write),
//             bf.delimited_by(just('['), just(']')).map(Instr::Loop),
//         ))
//         .repeated()
//     })
// }

// fn funcs_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {}

pub fn parse<'source>(mut tokens: lexer::LexerResult<'source>) -> ParseResult<'source> {
    // Main loop
    let mut bracket_stack: BracketStack = Vec::new();
    let mut state_stack: StateStack = Vec::new();

    let mut result = ParseResult::new(tokens.source());

    fn push_bracket(bracket_stack: &mut BracketStack, bracket_type: BracketType) {
        bracket_stack.push(bracket_type);
    }

    fn pop_bracket(bracket_stack: &mut BracketStack, bracket_type: BracketType) -> bool {
        let last_bracket = bracket_stack.pop();
        return last_bracket == Some(bracket_type);
    }

    fn push_state_to_stack(state_stack: &mut StateStack, state: ParseState) {
        state_stack.push(state);
    }

    fn pop_state_from_stack(state_stack: &mut StateStack) -> ParseState {
        state_stack.pop().unwrap()
    }

    macro_rules! push_state {
        ($state:expr) => {
            push_state_to_stack(&mut state_stack, $state);
        };
    }

    macro_rules! pop_state {
        () => {
            pop_state_from_stack(&mut state_stack)
        };
    }

    let mut program = ast::Program::new();

    push_state!(ParseState::Program(program));

    while let Some(token) = tokens.next() {
        let location = Location {
            file: 0,
            span: USizeTuple(tokens.span().start, tokens.span().end),
        };

        macro_rules! location {
            () => {
                Location {
                    file: 0,
                    span: USizeTuple(tokens.span().start, tokens.span().end),
                }
            };
        }

        macro_rules! add_alert {
            ($alert_level:expr, $alert_type:expr, $($arg:tt)*) => {
                result.add_alert($alert_level, format!($($arg)*), location!(), $alert_type);
            };
        }

        macro_rules! alert_error {
            ($alert_type:expr, $($arg:tt)*) => {
                add_alert!(printer::AlertLevel::Error, $alert_type, $($arg)*);
            };

        }

        macro_rules! alert_warning {
            ($alert_type:expr, $($arg:tt)*) => {
                add_alert!(printer::AlertLevel::Warning, $alert_type, $($arg)*);
            };
        }

        macro_rules! alert_info {
            ($alert_type:expr, $($arg:tt)*) => {
                add_alert!(printer::AlertLevel::Info, $alert_type, $($arg)*);
            };
        }

        macro_rules! seq {
            ($($x:expr),*) => {
                {
                    let peekable = tokens.peekable();
                    let output = vec![$($x),*];
                    $(
                        let tok = peekable.next();
                        if tok != Some($x) {
                            alert_error!(AlertType::UnexpectedToken, "Expected {:?}, got {:?}", $x, tok);
                            break;
                        }
                    )*

                }
            };
        }

        let current_state = state_stack.last().unwrap();

        // match tokens.next() {
        // ParseState::Program(program) => {
        //     match token {
        //         Token::Import => {
        //             let checkpoint = tokens.clone();

        //             let mut import = ast::Import {
        //                 path: ast::StringLiteral {
        //                     value: String::new(),
        //                     location,
        //                 },
        //                 name: Vec::new(),
        //             };

        //             let mut import_state = 0;
        //             let mut import_name: Option<ast::Identifier>;
        //             let mut import_alias: Option<ast::Identifier>;
        //             let mut import_path: Option<ast::StringLiteral>;

        //             while true {
        //                 let tok = tokens.next().unwrap();
        //                 match import_state {
        //                     0 => {
        //                         match tok {
        //                             Token::Identifier => {
        //                                 import_name = Some(ast::Identifier {
        //                                     value: tokens.slice().to_string(),
        //                                     location: location!(),
        //                                 });
        //                                 import_state = 1;
        //                             }
        //                             _ => {
        //                                 alert_error!(AlertType::UnexpectedToken, "Expected identifier, got {:?}, should be: import xyz", tokens.span());
        //                                 alert_error!(AlertType::UnexpectedToken, "Expected identifier, got {:?}, correct syntax: - import Cube from 'geometry'; or\n -import Cube as MyCube from 'geometry'; or\n -import Cube, Pyramid from 'geometry';", tokens.span());
        //                                 break;
        //                             }
        //                         }
        //                     }
        //                     1 => match tok {
        //                         Token::As => {
        //                             import_state = 2;
        //                         }
        //                         Token::Comma => {
        //                             import.name.push(import_name.unwrap());
        //                             import_state = 0;
        //                         }
        //                         Token::From => {
        //                             import.name.push(import_name.unwrap());
        //                             import_state = 3;
        //                         }
        //                         _ => {
        //                             alert_error!(AlertType::UnexpectedToken, "Expected 'as' or 'from', got {:?}, should be: import xyz as abc from 'geometry'; or\n -import xyz from 'geometry'; or\n -import xyz, abc from 'geometry';", tokens.span());
        //                             break;
        //                         }
        //                     },
        //                 }
        //             }

        //             let (import_list, _, import_path) = seq!(
        //                 rep_comma!(
        //                     tok!(Token::Identifier),
        //                     opt!(seq!(tok!(Token::As), tok!(Token::Identifier),),),
        //                 ),
        //                 tok!(Token::From),
        //                 tok!(Token::String),
        //             );
        //             while let Some(peek) = peeks.next() {
        //                 match peek {
        //                     Token::Identifier => {}
        //                     Token::Newline => {
        //                         alert_error!(
        //                             AlertType::UnexpectedEndOfImport,
        //                             "Unexpected end of import"
        //                         );
        //                         break;
        //                     }
        //                     _ => {
        //                         alert_error!(
        //                             AlertType::UnexpectedToken,
        //                             "Unexpected token: {:?}",
        //                             peek
        //                         );
        //                         break;
        //                     }
        //                 }
        //             }
        //             push_state!(ParseState::Import(ast::Import::new()));
        //         }
        // Token::OpenScope => {
        //     push_bracket(&mut bracket_stack, BracketType::Curly);
        // }
        // Token::CloseScope => {
        //     let balanced = pop_bracket(&mut bracket_stack, BracketType::Curly);
        //     if !balanced {
        //         alert_error!(AlertType::Unspecified, "Expected end of block");
        //     }
        // }
        // Token::OpenSq => {
        //     push_bracket(&mut bracket_stack, BracketType::Square);
        // }
        // Token::CloseSq => {
        //     let balanced = pop_bracket(&mut bracket_stack, BracketType::Square);
        //     if !balanced {
        //         alert_error!(AlertType::Unspecified, "Expected end of list");
        //     }
        // }
        // Token::OpenPar => {
        //     push_bracket(&mut bracket_stack, BracketType::Round);
        // }
        // Token::ClosePar => {
        //     let balanced = pop_bracket(&mut bracket_stack, BracketType::Round);
        //     if !balanced {
        //         alert_error!(AlertType::Unspecified, "Expected end of block");
        //     }
        // }
        // Token::Identifier => {
        //     alert_info!(
        //         AlertType::Unspecified,
        //         "Found identifier {}",
        //         tokens.slice(),
        //     );
        // }
        // _ => {}
        // }

        // println!("{:?} {:?}", token, tokens.slice());
    }

    return result;
}
