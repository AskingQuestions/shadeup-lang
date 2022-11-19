#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use std::fmt;
use std::fmt::{Debug};

use std::vec::Vec;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct USizeTuple(pub usize, pub usize);

#[derive(Debug, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Location {
    file: String,
    pub span: USizeTuple,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl Location {
    pub fn new(file: String, span: USizeTuple) -> Location {
        Location { file, span }
    }

    pub fn get_line_and_column(&self, source: &str, loc: usize) -> USizeTuple {
        let mut line = 1;
        let mut line_start = 0;
        for (i, c) in source.char_indices() {
            if i == loc {
                return USizeTuple(line, loc - line_start);
            }
            if c == '\n' {
                line += 1;
                line_start = i + 1;
            }
        }
        USizeTuple(line, loc - line_start)
    }

    pub fn get_start_line_and_column(&self, source: &str) -> USizeTuple {
        self.get_line_and_column(source, self.span.0)
    }

    pub fn get_end_line_and_column(&self, source: &str) -> USizeTuple {
        self.get_line_and_column(source, self.span.1)
    }

    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn file(&self) -> String {
        self.file.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SymbolName {
    pub value: Identifier,

    pub alias: Option<Box<SymbolName>>,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
    pub span: Span,
    // pub location: Location,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
    pub parameters: Vec<(Identifier, Identifier, Option<Expression>)>,
    pub return_type: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub name: Identifier,
    pub body: Vec<Root>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Root {
    Import(Import),
    Function(Function),
    Main(Block),
    Shader(Block),
    Expression(Expression),
    Struct(Struct),
    Impl(Impl),
    If(If),
    Let(Let),
    Return(Return),
    Error,
}

impl Root {
    pub fn get_name(&self) -> Option<String> {
        match self {
            Root::Import(_) => None,
            Root::Function(f) => Some(f.name.name.clone()),
            Root::Main(_) => Some("main".to_string()),
            Root::Shader(_) => Some("shader".to_string()),
            Root::Expression(_) => None,
            Root::Struct(s) => Some(s.name.name.clone()),
            Root::Impl(i) => Some(i.name.name.clone()),
            Root::If(_) => None,
            Root::Let(l) => Some(l.name.name.clone()),
            Root::Return(_) => None,
            Root::Error => None,
        }
    }

    pub fn get_kind(&self) -> &str {
        match self {
            Root::Import(_) => "import",
            Root::Function(_) => "function",
            Root::Main(_) => "main",
            Root::Shader(_) => "shader",
            Root::Expression(_) => "expression",
            Root::Struct(_) => "struct",
            Root::Impl(_) => "impl",
            Root::If(_) => "if",
            Root::Let(_) => "let",
            Root::Return(_) => "return",
            Root::Error => "error",
        }
    }

    pub fn get_span(&self) -> Span {
        match self {
            Root::Import(i) => i.path.span.clone(),
            Root::Function(f) => f.span.clone(),
            Root::Main(b) => b.span.clone(),
            Root::Shader(b) => b.span.clone(),
            Root::Expression(e) => e.get_span(),
            Root::Struct(s) => s.span.clone(),
            Root::Impl(i) => i.span.clone(),
            Root::If(i) => i.span.clone(),
            Root::Let(l) => l.span.clone(),
            Root::Return(r) => r.span.clone(),
            Root::Error => Span { start: 0, end: 0 },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Real(f64),
    Str(String),
    // List(Vec<Value>),
    // Func(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier((Identifier, Span)),
    List((Vec<Expression>, Span)),
    Tuple((Vec<Expression>, Span)),
    Value((Value, Span)),
    Call((Call, Span)),
    Ternary((Box<Expression>, Box<Expression>, Box<Expression>, Span)),
    Op((Box<Expression>, Op, Box<Expression>, Span)),
    InlineBlock((Block, Span)),
    StructInstance((Identifier, Vec<(Identifier, Box<Expression>)>, Span)),
    Error(((), Span)),
}

impl Expression {
    pub fn get_span(&self) -> Span {
        match self {
            Expression::Identifier((_, span)) => span.clone(),
            Expression::List((_, span)) => span.clone(),
            Expression::Value((_, span)) => span.clone(),
            Expression::Call((_, span)) => span.clone(),
            Expression::Op((_, _, _, span)) => span.clone(),
            Expression::Error((_, span)) => span.clone(),
            Expression::InlineBlock((_, span)) => span.clone(),
            Expression::Ternary((_, _, _, span)) => span.clone(),
            Expression::Tuple((_, span)) => span.clone(),
            Expression::StructInstance((_, _, span)) => span.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub expression: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    RFlow,
    EqEq,
    Eq,
    Ellipsis,
    Dot,
    NotEq,
    Not,
    LArrow,
    LessEq,
    Less,
    MoreEq,
    More,
    Join,
    Add,
    RArrow,
    Sub,
    SubSub,
    Mul,
    Div,
    Rem,
    PlusEq,
    MinusEq,
    Question,
    Sq,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::RFlow => write!(f, "->"),
            Op::Eq => write!(f, "="),
            Op::EqEq => write!(f, "=="),
            Op::Ellipsis => write!(f, "..."),
            Op::Dot => write!(f, "."),
            Op::NotEq => write!(f, "!="),
            Op::Not => write!(f, "!"),
            Op::LArrow => write!(f, "<-"),
            Op::LessEq => write!(f, "<="),
            Op::Less => write!(f, "<"),
            Op::MoreEq => write!(f, ">="),
            Op::More => write!(f, ">"),
            Op::Join => write!(f, "&"),
            Op::Add => write!(f, "+"),
            Op::RArrow => write!(f, "->"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Rem => write!(f, "%"),
            Op::PlusEq => write!(f, "+="),
            Op::MinusEq => write!(f, "-="),
            Op::SubSub => write!(f, "--"),
            Op::Sq => write!(f, "**"),
            Op::Question => write!(f, "?"),
        }
    }
}

impl Op {
    pub fn get_code_name(&self) -> String {
        match self {
            Op::RFlow => "right_arrow".to_string(),
            Op::EqEq => "equals".to_string(),
            Op::Eq => "set_equals".to_string(),
            Op::Ellipsis => "ellipsis".to_string(),
            Op::Dot => "dot".to_string(),
            Op::NotEq => "not_equals".to_string(),
            Op::Not => "not".to_string(),
            Op::LArrow => "left_arrow".to_string(),
            Op::LessEq => "less_than_or_equals".to_string(),
            Op::Less => "less_than".to_string(),
            Op::MoreEq => "greater_than_or_equals".to_string(),
            Op::More => "greater_than".to_string(),
            Op::Join => "join".to_string(),
            Op::Add => "plus".to_string(),
            Op::RArrow => "right_arrow".to_string(),
            Op::Sub => "minus".to_string(),
            Op::SubSub => "minus_minus".to_string(),
            Op::Mul => "multiply".to_string(),
            Op::Div => "divide".to_string(),
            Op::Rem => "modulo".to_string(),
            Op::PlusEq => "plus_equals".to_string(),
            Op::MinusEq => "minus_equals".to_string(),
            Op::Sq => "double_multiply".to_string(),
            Op::Question => "question".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub body: Block,
    pub else_ifs: Vec<If>,
    pub else_body: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: Identifier,
    pub value_type: Option<Identifier>,
    pub to: Option<Expression>,

    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Expression>,

    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Identifier,
    pub fields: Vec<(Identifier, Identifier)>,
    pub span: Span,
}

pub struct Program {
    pub imports: Vec<Import>,
    pub blocks: Vec<Block>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            imports: vec![],
            blocks: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportName {
    pub name: Identifier,
    pub alias: Option<Identifier>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: StringLiteral,
    pub name: Vec<ImportName>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    pub roots: Vec<Root>,
}

// #[derive(Debug, Clone)]
// pub enum Block {
//     StructBlock(StructBlock),
//     VariableBlock(VariableBlock),
//     CodeBlock(CodeBlock),
// }

// pub enum Expr {
//     Number(i32),
//     Op(Box<Expr>, Opcode, Box<Expr>),
//     Error,
// }

// pub enum ExprSymbol<'input> {
//     NumSymbol(&'input str),
//     Op(Box<ExprSymbol<'input>>, Opcode, Box<ExprSymbol<'input>>),
//     Error,
// }

// #[derive(Copy, Clone)]
// pub enum Opcode {
//     Mul,
//     Div,
//     Add,
//     Sub,
// }

// impl Debug for Expr {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::Expr::*;
//         match *self {
//             Number(n) => write!(fmt, "{:?}", n),
//             Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
//             Error => write!(fmt, "error"),
//         }
//     }
// }

// impl<'input> Debug for ExprSymbol<'input> {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::ExprSymbol::*;
//         match *self {
//             NumSymbol(n) => write!(fmt, "{:?}", n),
//             Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
//             Error => write!(fmt, "error"),
//         }
//     }
// }

// impl Debug for Opcode {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::Opcode::*;
//         match *self {
//             Mul => write!(fmt, "*"),
//             Div => write!(fmt, "/"),
//             Add => write!(fmt, "+"),
//             Sub => write!(fmt, "-"),
//         }
//     }
// }
