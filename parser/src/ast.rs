#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use std::fmt;

use std::vec::Vec;

pub type Span = std::ops::Range<usize>;

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct USizeTuple(pub usize, pub usize);

#[derive(Clone, Debug)]
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

    #[allow(dead_code)]
    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn file(&self) -> String {
        self.file.clone()
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct SymbolName {
    pub value: Identifier,

    pub alias: Option<Box<SymbolName>>,
}

#[derive(Clone, Debug)]
pub struct StringLiteral {
    pub value: String,
    pub span: Span,
    // pub location: Location,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
    pub parameters: Vec<(Identifier, Option<Identifier>, Option<Expression>)>,
    pub return_type: Option<Identifier>,
    pub span: Span,
}

pub struct AstWalker<'a> {
    pub root: Option<Box<dyn FnMut(&Root) + 'a>>,
    pub block: Option<Box<dyn FnMut(&Block) + 'a>>,
    pub expression: Option<Box<dyn FnMut(&Block) + 'a>>,
    pub identifier: Option<Box<dyn FnMut(&Identifier) + 'a>>,
    pub function: Option<Box<dyn FnMut(&Function) + 'a>>,
    pub shader: Option<Box<dyn FnMut(&Block) + 'a>>,
}

impl<'a> AstWalker<'a> {
    pub fn new() -> AstWalker<'a> {
        AstWalker {
            root: None,
            block: None,
            expression: None,
            identifier: None,
            function: None,
            shader: None,
        }
    }
}

impl Function {
    pub fn walk(&self, walker: &mut AstWalker) {
        if let Some(ref mut f) = walker.function {
            f(self);
        }
        self.body.walk(walker);
    }
}

#[derive(Clone, Debug)]
pub struct Impl {
    pub name: Identifier,
    pub body: Vec<Root>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Root {
    Import(Import),
    Function(Function),
    Main(Block),
    Shader(Block),
    Expression(Expression),
    Struct(Struct),
    Impl(Impl),
    If(If),
    For(For),
    ForEach(ForEach),
    While(While),
    Let(Let),
    Return(Return),
    Break(Span),
    Continue(Span),
    Error,
}

impl Root {
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
            Root::ForEach(_) => "for_each",
            Root::For(_) => "for",
            Root::While(_) => "while",
            Root::Break(_) => "break",
            Root::Continue(_) => "continue",
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
            Root::ForEach(f) => f.span.clone(),
            Root::For(f) => f.span.clone(),
            Root::While(w) => w.span.clone(),
            Root::Break(s) => s.clone(),
            Root::Continue(s) => s.clone(),
            Root::Error => Span { start: 0, end: 0 },
        }
    }

    pub fn get_def_span(&self) -> Span {
        match self {
            Root::Import(i) => i.path.span.clone(),
            Root::Function(f) => f.name.span.clone(),
            Root::Main(b) => b.span.clone(),
            Root::Shader(b) => b.span.clone(),
            Root::Expression(e) => e.get_span(),
            Root::Struct(s) => s.name.span.clone(),
            Root::Impl(i) => i.name.span.clone(),
            Root::If(i) => i.span.clone(),
            Root::Let(l) => l.span.clone(),
            Root::Return(r) => r.span.clone(),
            Root::ForEach(f) => f.span.clone(),
            Root::For(f) => f.span.clone(),
            Root::While(w) => w.span.clone(),
            Root::Break(s) => s.clone(),
            Root::Continue(s) => s.clone(),
            Root::Error => Span { start: 0, end: 0 },
        }
    }

    pub fn walk(&self, walker: &mut AstWalker) {
        match self {
            Root::Function(f) => f.walk(walker),
            Root::Main(b) => {}
            Root::Shader(b) => {
                if let Some(ref mut f) = walker.shader {
                    f(b);
                }
            }
            Root::Expression(e) => e.walk(walker),
            Root::Struct(s) => {}
            Root::Impl(i) => {
                for r in &i.body {
                    r.walk(walker);
                }
            }
            Root::If(i) => {
                i.condition.walk(walker);
                i.body.walk(walker);
                if let Some(ref else_) = i.else_body {
                    else_.walk(walker);
                }
                for e in &i.else_ifs {
                    e.body.walk(walker);
                    e.condition.walk(walker);
                }
            }
            Root::Let(l) => {
                if let Some(ref e) = l.to {
                    e.walk(walker);
                }
            }
            Root::Return(r) => {
                if let Some(ref e) = r.value {
                    e.walk(walker);
                }
            }
            Root::ForEach(f) => {
                f.body.walk(walker);
                f.iterable.walk(walker);
            }
            Root::For(f) => {
                f.body.walk(walker);
                f.first.walk(walker);
                f.second.walk(walker);
                f.third.walk(walker);
            }
            Root::While(w) => {
                w.body.walk(walker);
                w.condition.walk(walker);
            }
            Root::Break(_) => {}
            Root::Continue(_) => {}
            Root::Error => {}
            _ => {}
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Real(f64),
    Str(String),
    // List(Vec<Value>),
    // Func(String),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
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

    pub fn walk(&self, walker: &mut AstWalker) {
        match self {
            Expression::Identifier(ident) => {
                if let Some(ref mut f) = walker.identifier {
                    f(&ident.0);
                }
            }
            Expression::List((l, _)) => {
                for e in l {
                    e.walk(walker);
                }
            }
            Expression::Value((_, _)) => {}
            Expression::Call((c, _)) => {
                c.expression.walk(walker);
                for e in &c.args {
                    e.walk(walker);
                }
            }
            Expression::Op((l, _, r, _)) => {
                l.walk(walker);
                r.walk(walker);
            }
            Expression::Error((_, _)) => {}
            Expression::InlineBlock((b, _)) => {
                if let Some(ref mut f) = walker.shader {
                    f(b);
                }
                b.walk(walker);
            }
            Expression::Ternary((c, t, f, _)) => {
                c.walk(walker);
                t.walk(walker);
                f.walk(walker);
            }
            Expression::Tuple((l, _)) => {
                for e in l {
                    e.walk(walker);
                }
            }
            Expression::StructInstance((_, l, _)) => {
                for (_, e) in l {
                    e.walk(walker);
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    pub expression: Box<Expression>,
    pub method: Option<Identifier>,
    pub args: Vec<Expression>,
    pub span: Span,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
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
    DoubleColon,
    Sq,
    AndAnd,
    And,
    Bar,
    BarBar,
    Hat,
    DoubleRight,
    DoubleLeft,
    Tilda,
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
            Op::DoubleColon => write!(f, "::"),
            Op::AndAnd => write!(f, "&&"),
            Op::And => write!(f, "&"),
            Op::Bar => write!(f, "|"),
            Op::BarBar => write!(f, "||"),
            Op::Hat => write!(f, "^"),
            Op::DoubleRight => write!(f, ">>"),
            Op::DoubleLeft => write!(f, "<<"),
            Op::Tilda => write!(f, "~"),
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
            Op::DoubleColon => "double_colon".to_string(),
            Op::AndAnd => "and_and".to_string(),
            Op::And => "and".to_string(),
            Op::Bar => "bar".to_string(),
            Op::BarBar => "bar_bar".to_string(),
            Op::Hat => "hat".to_string(),
            Op::DoubleRight => "double_right".to_string(),
            Op::DoubleLeft => "double_left".to_string(),
            Op::Tilda => "tilda".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct If {
    pub condition: Expression,
    pub body: Block,
    pub else_ifs: Vec<If>,
    pub else_body: Option<Block>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct For {
    pub first: Box<Root>,
    pub second: Expression,
    pub third: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ForEach {
    pub variable: Identifier,
    pub counter: Option<Identifier>,

    pub iterable: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct While {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Let {
    pub name: Identifier,
    pub value_type: Option<Identifier>,
    pub to: Option<Expression>,

    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Return {
    pub value: Option<Expression>,

    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Identifier,
    pub fields: Vec<(Identifier, Identifier)>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ImportName {
    pub name: Identifier,
    pub alias: Option<Identifier>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub path: StringLiteral,
    pub name: Vec<ImportName>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub span: Span,
    pub roots: Vec<Root>,
}

impl Block {
    pub fn walk(&self, walker: &mut AstWalker) {
        if let Some(ref mut f) = walker.block {
            f(self);
        }

        for root in &self.roots {
            root.walk(walker);
        }
    }
}
