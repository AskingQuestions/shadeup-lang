#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use std::fmt;
use std::fmt::{Debug, Error, Formatter};

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
    If(If),
    Let(Let),
    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Real(f64),
    Str(String),
    List(Vec<Value>),
    Func(String),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier((Identifier, Span)),
    List((Vec<Expression>, Span)),
    Value((Value, Span)),
    Call((Call, Span)),
    Op((Box<Expression>, Op, Box<Expression>, Span)),
    InlineBlock((Block, Span)),
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
pub struct Struct {
    pub name: Identifier,
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
