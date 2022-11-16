use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use chumsky::combinator::Repeated;
use chumsky::{prelude::*, stream::Stream};
use std::io::{BufWriter, Write};
use std::str;
use std::{collections::HashMap, env, fmt, fs};

use crate::ast::{self, Identifier, Op, Value};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Null,
    Bool(bool),
    Real(String),
    Int(i64),
    Str(String),
    Op(Op),
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
    Return,
    If,
    Impl,
    Else,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Null => write!(f, "null"),
            Token::Bool(x) => write!(f, "{}", x),
            Token::Int(n) => write!(f, "{}", n),
            Token::Real(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::As => write!(f, "as"),
            Token::From => write!(f, "from"),
            Token::Import => write!(f, "import"),
            Token::Shader => write!(f, "shader"),
            Token::Struct => write!(f, "struct"),
            Token::Impl => write!(f, "impl"),
            Token::Main => write!(f, "main"),
            Token::Pub => write!(f, "pub"),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map(Token::Real);

    // A parser for numbers
    let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));

    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    // A parser for strings
    let str_full = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
        .labelled("string");

    let str_single = just('\'')
        .ignore_then(filter(|c| *c != '\\' && *c != '\'').or(escape).repeated())
        .then_ignore(just('\''))
        .collect::<String>()
        .map(Token::Str)
        .labelled("string");

    // A parser for operators
    let op = choice((
        // Compare
        just("==").to(Op::EqEq),
        just("!=").to(Op::NotEq),
        just("<=").to(Op::LessEq),
        just('<').to(Op::Less),
        just(">=").to(Op::MoreEq),
        just('>').to(Op::More),
        // Mutate
        just('=').to(Op::Eq),
        just("+=").to(Op::PlusEq),
        just("-=").to(Op::MinusEq),
        just("++").to(Op::Join),
        just("--").to(Op::SubSub),
        just("=>").to(Op::RFlow),
        just("<-").to(Op::LArrow),
        just("->").to(Op::RArrow),
        // Special
        just('!').to(Op::Not),
        just('.').to(Op::Dot),
        just("..").to(Op::Ellipsis),
        // 2nd order
        just('+').to(Op::Add),
        just('-').to(Op::Sub),
        // 1st order
        just("**").to(Op::Sq),
        just('*').to(Op::Mul),
        just('/').to(Op::Div),
        // 0th order
        just('%').to(Op::Rem),
        just('?').to(Op::Question),
    ))
    .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,:").map(|c| Token::Ctrl(c));

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Fn,
        "let" => Token::Let,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,
        "as" => Token::As,
        "from" => Token::From,
        "import" => Token::Import,
        "shader" => Token::Shader,
        "struct" => Token::Struct,
        "main" => Token::Main,
        "pub" => Token::Pub,
        "impl" => Token::Impl,

        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = real
        .or(int)
        .or(str_full)
        .or(str_single)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();
    let multi_comment = just("/*").then(take_until(just("*/"))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded_by(multi_comment.repeated())
        .padded()
        .repeated()
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::Real(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
            // Self::List(xs) => write!(
            //     f,
            //     "[{}]",
            //     xs.iter()
            //         .map(|x| x.to_string())
            //         .collect::<Vec<_>>()
            //         .join(", ")
            // ),
            // Self::Func(name) => write!(f, "<function: {}>", name),
        }
    }
}

struct Error {
    span: Span,
    msg: String,
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

pub type Spanned<T> = (T, Span);

// An expression node in the AST. Children are spanned so we can generate useful runtime errors.
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

// A function node in the AST.
#[derive(Debug)]
struct Func {
    args: Vec<String>,
    body: Spanned<Expr>,
}

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = recursive(|raw_expr| {
            let val = select! {
                Token::Null => Expr::Value(Value::Null),
                Token::Bool(x) => Expr::Value(Value::Bool(x)),
                Token::Int(x) => Expr::Value(Value::Int(x)),
                Token::Real(n) => Expr::Value(Value::Real(n.parse().unwrap())),
                Token::Str(s) => Expr::Value(Value::Str(s)),
            }
            .labelled("value");

            let ident = select! { Token::Ident(ident) => ident.clone() }.labelled("identifier");

            // A list of expressions
            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            // A let expression
            let let_ = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Op(Op::Eq)))
                .then(raw_expr)
                .then_ignore(just(Token::Ctrl(';')))
                .then(expr.clone())
                .map(|((name, val), body)| Expr::Let(name, Box::new(val), Box::new(body)));

            let list = items
                .clone()
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .map(Expr::List);

            // 'Atoms' are expressions that contain no ambiguity
            let atom = val
                .or(ident.map(Expr::Local))
                .or(let_)
                .or(list)
                // // In Nano Rust, `print` is just a keyword, just like Python 2, for simplicity
                // .or(just(Token::Print)
                //     .ignore_then(
                //         expr.clone()
                //             .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                //     )
                //     .map(|expr| Expr::Print(Box::new(expr))))
                .map_with_span(|expr, span| (expr, span))
                // Atoms can also just be normal expressions, but surrounded with parentheses
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ))
                // Attempt to recover anything that looks like a list but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ));

            // Function calls have very high precedence so we prioritise them
            let call = atom
                .then(
                    items
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map_with_span(|args, span: Span| (args, span))
                        .repeated(),
                )
                .foldl(|f, args| {
                    let span = f.1.start..args.1.end;
                    (Expr::Call(Box::new(f), args.0), span)
                });

            // Product ops (multiply and divide) have equal precedence
            let op = just(Token::Op(Op::Mul))
                .to(BinaryOp::Mul)
                .or(just(Token::Op(Op::Div)).to(BinaryOp::Div));
            let product = call
                .clone()
                .then(op.then(call).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Op(Op::Add))
                .to(BinaryOp::Add)
                .or(just(Token::Op(Op::Sub)).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Comparison ops (equal, not-equal) have equal precedence
            let op = just(Token::Op(Op::EqEq))
                .to(BinaryOp::Eq)
                .or(just(Token::Op(Op::NotEq)).to(BinaryOp::NotEq));
            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            compare
        });

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            ));

        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((cond, a), b), span: Span| {
                    (
                        Expr::If(
                            Box::new(cond),
                            Box::new(a),
                            Box::new(match b {
                                Some(b) => b,
                                // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                                None => (Expr::Value(Value::Null), span.clone()),
                            }),
                        ),
                        span,
                    )
                })
        });

        // Both blocks and `if` are 'block expressions' and can appear in the place of statements
        let block_expr = block.or(if_).labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| {
                let span = a.1.start..b.1.end;
                (Expr::Then(Box::new(a), Box::new(b)), span)
            });

        block_chain
            // Expressions, chained by semicolons, are statements
            .or(raw_expr.clone())
            .then(just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated())
            .foldl(|a, b| {
                let span = a.1.clone(); // TODO: Not correct
                (
                    Expr::Then(
                        Box::new(a),
                        Box::new(match b {
                            Some(b) => b,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            })
    })
}

impl Value {
    fn num(self, span: Span) -> Result<f64, Error> {
        if let Value::Real(x) = self {
            Ok(x)
        } else {
            Err(Error {
                span,
                msg: format!("'{}' is not a number", self),
            })
        }
    }
}

fn block_parser() -> impl Parser<Token, Vec<ast::Root>, Error = Simple<Token>> + Clone {
    let ident = filter_map(|span, tok| match tok {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let string_literal = filter_map(|span, tok| match tok {
        Token::Str(s) => Ok(ast::StringLiteral {
            span,
            value: s.clone(),
        }),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    });

    let identifier = ident
        .clone()
        .labelled("identifier")
        .map_with_span(|ident, span| ast::Identifier { name: ident, span });

    let import_as = identifier
        .clone()
        .labelled("import identifier")
        .then_ignore(just(Token::As))
        .then(identifier.labelled("alias identifier"))
        .map_with_span(|(ident1, ident2), span| ast::ImportName {
            name: ident1,
            alias: Some(ident2),
            span,
        })
        .labelled("import as");

    let import_ident = import_as
        .clone()
        .or(identifier
            .clone()
            .labelled("import identifier")
            .map(|ident| ast::ImportName {
                name: ident.clone(),
                alias: None,
                span: ident.span,
            }))
        .separated_by(just(Token::Ctrl(',')))
        .labelled("import identifiers");

    let import = just(Token::Import)
        .ignore_then(import_ident.clone())
        .then_ignore(just(Token::From))
        .then(string_literal.clone().labelled("path"))
        .then_ignore(just(Token::Ctrl(';')))
        .map_with_span(|(names, path), span| ast::Import { name: names, path })
        .labelled("import");

    recursive::<_, Vec<ast::Root>, _, _, _>(|block_recur| {
        let block_shader = filter_map(|span, tok| match tok {
            Token::Shader => Ok(Token::Shader),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        });

        let block_main = filter_map(|span, tok| match tok {
            Token::Main => Ok(Token::Main),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        });

        let block_type = block_main.or(block_shader).labelled("block type");

        let shader_or_main_block = block_type
            .clone()
            .then(block_recur.clone().labelled("block body").delimited_by(
                just(Token::Ctrl('{')),
                just(Token::Ctrl('}')).recover_with(nested_delimiters(
                    Token::Ctrl('{'),
                    Token::Ctrl('}'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('['), Token::Ctrl(']')),
                    ],
                    |span| Token::Fn,
                )),
            ))
            // Attempt to recover anything that looks like a function body but contains errors
            .map_with_span(|(block_type, body), span| match block_type {
                Token::Main => ast::Root::Main(ast::Block {
                    roots: body,
                    span: span,
                }),
                Token::Shader => ast::Root::Shader(ast::Block {
                    roots: body,
                    span: span,
                }),
                _ => ast::Root::Main(ast::Block {
                    roots: vec![],
                    span: span,
                }),
            });

        let expression = recursive(|expr| {
            let val = select! {
                Token::Null => Value::Null,
                Token::Bool(x) => Value::Bool(x),
                Token::Int(x) => Value::Int(x),
                Token::Real(n) => Value::Real(n.parse().unwrap()),
                Token::Str(s) => Value::Str(s),
            }
            .labelled("value");

            // A list of expressions
            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            let list = items
                .clone()
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .map_with_span(|list, span| ast::Expression::List((list, span)));

            let tuple = items
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map_with_span(|tuple, span| ast::Expression::Tuple((tuple, span)));

            let struct_key_val_list = identifier
                .clone()
                .then_ignore(just(Token::Ctrl(':')))
                .then(expr.clone())
                .map(|(key, val)| (key, Box::new(val)))
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            let struct_def = identifier
                .clone()
                .then(
                    struct_key_val_list
                        .clone()
                        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
                )
                .map_with_span(|(name, fields), span| {
                    ast::Expression::StructInstance((name, fields, span))
                })
                .recover_with(nested_delimiters(
                    Token::Ctrl('{'),
                    Token::Ctrl('}'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('['), Token::Ctrl(']')),
                    ],
                    |span| ast::Expression::Error(((), span)),
                ));

            // 'Atoms' are expressions that contain no ambiguity
            let atom = val
                .map_with_span(|val, span| ast::Expression::Value((val, span)))
                .or(struct_def)
                .or(identifier
                    .clone()
                    .map(|id| ast::Expression::Identifier((id.clone(), id.span))))
                .or(list)
                .or(tuple)
                .or(shader_or_main_block.clone().map_with_span(|block, span| {
                    ast::Expression::InlineBlock((
                        match block {
                            ast::Root::Main(block) => block,
                            ast::Root::Shader(block) => block,
                            _ => unreachable!(),
                        },
                        span,
                    ))
                }))
                // Atoms can also just be normal expressions, but surrounded with parentheses
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| ast::Expression::Error(((), span)),
                ))
                // Attempt to recover anything that looks like a list but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| ast::Expression::Error(((), span)),
                ));

            // Function calls have very high precedence so we prioritise them
            let call = atom
                .then(
                    items
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map_with_span(|args, span: Span| (args, span))
                        .repeated(),
                )
                .foldl(|f, args| {
                    let span = f.get_span().start..args.1.end;
                    ast::Expression::Call((
                        ast::Call {
                            args: args.0,
                            expression: Box::new(f),
                            span: span.clone(),
                        },
                        span,
                    ))
                });

            let op = just(Token::Op(Op::Dot)).to(Op::Dot);
            let dot = call
                .clone()
                .then(op.then(call).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.get_span().start..b.get_span().end;
                    ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                });

            // Product ops (multiply and divide) have equal precedence
            let op = just(Token::Op(Op::Mul))
                .to(Op::Mul)
                .or(just(Token::Op(Op::Div)).to(Op::Div))
                .or(just(Token::Op(Op::Sq)).to(Op::Sq));
            let product = dot
                .clone()
                .then(op.then(dot).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.get_span().start..b.get_span().end;
                    ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                });

            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Op(Op::Add))
                .to(Op::Add)
                .or(just(Token::Op(Op::Sub)).to(Op::Sub));
            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.get_span().start..b.get_span().end;
                    ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                });

            // Comparison ops (equal, not-equal) have equal precedence
            let op = just(Token::Op(Op::EqEq))
                .to(Op::EqEq)
                .or(just(Token::Op(Op::NotEq)).to(Op::NotEq))
                .or(just(Token::Op(Op::Less)).to(Op::Less))
                .or(just(Token::Op(Op::LessEq)).to(Op::LessEq))
                .or(just(Token::Op(Op::More)).to(Op::More))
                .or(just(Token::Op(Op::MoreEq)).to(Op::MoreEq));
            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.get_span().start..b.get_span().end;
                    ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                });

            // let op = just(Token::Op(Op::Question)).to(Op::Question);
            // let ternary = compare
            //     .clone()
            //     .then(
            //         op.ignore_then(compare.clone())
            //             .then_ignore(just(Token::Ctrl(':')))
            //             .then(compare.clone())
            //             .repeated(),
            //     )
            //     .foldl(|a, (b, c)| {
            //         let span = a.get_span().start..c.get_span().end;
            //         ast::Expression::Ternary((Box::new(a), Box::new(b), Box::new(c), span.clone()))
            //     });
            // .map_with_span(|(a, tern), span| match tern {
            //     Some((b, c)) => {
            //         ast::Expression::Ternary((Box::new(a), Box::new(b), Box::new(c), span))
            //     }
            //     None => a,
            // });

            // let ternary = compare
            //     .clone()
            //     .then(op.then(compare).repeated())
            //     .foldl(|a, (op, b)| {
            //         let span = a.get_span().start..b.get_span().end;
            //         ast::Expression::Op((Box::new(a), op, Box::new(b), span))
            //     });

            // Ternary operator pushes us just past the stack size
            let ternary = compare;

            let op = just(Token::Op(Op::Eq))
                .to(Op::Eq)
                .or(just(Token::Op(Op::PlusEq)).to(Op::PlusEq))
                .or(just(Token::Op(Op::MinusEq)).to(Op::MinusEq))
                .or(just(Token::Op(Op::Join)).to(Op::Join))
                .or(just(Token::Op(Op::SubSub)).to(Op::SubSub))
                .or(just(Token::Op(Op::RFlow)).to(Op::RFlow))
                .or(just(Token::Op(Op::LArrow)).to(Op::LArrow))
                .or(just(Token::Op(Op::RArrow)).to(Op::RArrow));
            let assign = ternary
                .clone()
                .then(op.then(ternary).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.get_span().start..b.get_span().end;
                    ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                });

            assign
        });

        let args = identifier
            .clone()
            .then_ignore(just(Token::Ctrl(':')))
            .then(identifier.clone())
            .then(
                just(Token::Op(Op::Eq))
                    .ignore_then(expression.clone())
                    .or_not(),
            )
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
            .labelled("function args")
            .map(|args| {
                args.into_iter()
                    .map(|((name, ty), default)| (name, ty, default))
                    .collect()
            });

        let func = just(Token::Fn)
            .ignore_then(identifier.labelled("function name"))
            .then(args)
            .then(just(Token::Op(Op::RArrow)).ignore_then(identifier).or_not())
            .then(
                block_recur
                    .clone()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                    // Attempt to recover anything that looks like a function body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Ctrl('{'),
                        Token::Ctrl('}'),
                        [
                            (Token::Ctrl('('), Token::Ctrl(')')),
                            (Token::Ctrl('['), Token::Ctrl(']')),
                        ],
                        |span: Span| {
                            vec![ast::Root::Function(ast::Function {
                                name: ast::Identifier {
                                    name: "error".to_string(),
                                    span: span.clone(),
                                },
                                body: ast::Block {
                                    roots: vec![],
                                    span: span.clone(),
                                },
                                parameters: vec![],
                                return_type: None,
                                span,
                            })]
                        },
                    )),
            )
            .map_with_span(|(((name, args), return_type), body), span| {
                ast::Root::Function(ast::Function {
                    body: ast::Block {
                        roots: body,
                        span: span.clone(),
                    },
                    parameters: args,
                    return_type,
                    name: name,
                    span,
                })
            })
            .labelled("function");

        let let_start = identifier
            .labelled("let identifier")
            .then_ignore(just(Token::Ctrl(':')))
            .then(identifier.clone())
            .map(|(id, ty)| (id, Some(ty)))
            .or(identifier.labelled("let identifier").map(|id| (id, None)));

        let let_end = just(Token::Op(Op::Eq))
            .then(expression.clone().labelled("let value"))
            .then_ignore(just(Token::Ctrl(';')))
            .map(|(_, expr)| Some(expr))
            .or(just(Token::Ctrl(';')).to(None));

        let _let = just(Token::Let)
            .ignore_then(let_start)
            .then(let_end)
            .map_with_span(|((id, id2), expr), span| {
                ast::Root::Let(ast::Let {
                    name: id,
                    to: expr,
                    value_type: id2,
                    span,
                })
            })
            .labelled("let");

        let _if = just(Token::If)
            .ignore_then(
                expression
                    .clone()
                    .labelled("if condition")
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(
                block_recur
                    .clone()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                    // Attempt to recover anything that looks like an if body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Ctrl('{'),
                        Token::Ctrl('}'),
                        [
                            (Token::Ctrl('('), Token::Ctrl(')')),
                            (Token::Ctrl('['), Token::Ctrl(']')),
                        ],
                        |span: Span| vec![],
                    ))
                    .map_with_span(|roots, span| ast::Block {
                        roots: roots,
                        span: span.clone(),
                    })
                    .then(
                        just(Token::Else).ignore_then(
                            just(Token::If)
                                .ignore_then(expression.clone().labelled("if condition"))
                                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                                .then(
                                    block_recur
                                        .clone()
                                        .delimited_by(
                                            just(Token::Ctrl('{')),
                                            just(Token::Ctrl('}')),
                                        )
                                        // Attempt to recover anything that looks like an else body but contains errors
                                        .recover_with(nested_delimiters(
                                            Token::Ctrl('{'),
                                            Token::Ctrl('}'),
                                            [
                                                (Token::Ctrl('('), Token::Ctrl(')')),
                                                (Token::Ctrl('['), Token::Ctrl(']')),
                                            ],
                                            |span: Span| vec![],
                                        )),
                                )
                                .map_with_span(|(condition, roots), span| ast::If {
                                    condition,
                                    body: ast::Block {
                                        roots: roots,
                                        span: span.clone(),
                                    },
                                    else_ifs: vec![],
                                    else_body: None,
                                    span: span.clone(),
                                })
                                .or(block_recur
                                    .clone()
                                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                                    // Attempt to recover anything that looks like an else body but contains errors
                                    .recover_with(nested_delimiters(
                                        Token::Ctrl('{'),
                                        Token::Ctrl('}'),
                                        [
                                            (Token::Ctrl('('), Token::Ctrl(')')),
                                            (Token::Ctrl('['), Token::Ctrl(']')),
                                        ],
                                        |span: Span| {
                                            vec![ast::Root::If(ast::If {
                                                condition: ast::Expression::Value((
                                                    ast::Value::Bool(true),
                                                    span.clone(),
                                                )),
                                                body: ast::Block {
                                                    roots: vec![],
                                                    span: span.clone(),
                                                },
                                                else_ifs: vec![],
                                                else_body: None,
                                                span: span.clone(),
                                            })]
                                        },
                                    ))
                                    .map_with_span(|roots, span| ast::Block {
                                        roots: roots,
                                        span: span.clone(),
                                    })
                                    .map_with_span(|block, span| ast::If {
                                        condition: ast::Expression::Error(((), 0..0)),
                                        body: ast::Block {
                                            roots: vec![],
                                            span: span.clone(),
                                        },
                                        else_ifs: vec![],
                                        else_body: Some(block),
                                        span: span.clone(),
                                    }))
                                .repeated(),
                        ),
                    ),
            )
            .map_with_span(|(cond, (block, ifs)), span| {
                let mut else_body: Option<ast::Block> = None;
                let mut else_ifs = vec![];
                for if_ in ifs.into_iter().rev() {
                    if let ast::Expression::Error((_, orig_span)) = if_.condition {
                        if orig_span.start == 0 && orig_span.end == 0 {
                            else_body = Some(if_.else_body.unwrap().clone());
                        }
                    } else {
                        else_ifs.push(ast::If {
                            condition: if_.condition,
                            body: if_.body,
                            else_ifs: vec![],
                            else_body: None,
                            span: if_.span.clone(),
                        });
                    }
                }
                ast::Root::If(ast::If {
                    condition: cond,
                    body: block,
                    else_ifs: else_ifs,
                    else_body: else_body,
                    span: span.clone(),
                })
            })
            .labelled("if");

        let _impl = just(Token::Impl)
            .ignore_then(identifier.labelled("impl name"))
            .then(
                func.clone()
                    .repeated()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                    // Attempt to recover anything that looks like a function body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Ctrl('{'),
                        Token::Ctrl('}'),
                        [
                            (Token::Ctrl('('), Token::Ctrl(')')),
                            (Token::Ctrl('['), Token::Ctrl(']')),
                        ],
                        |span: Span| vec![],
                    )),
            )
            .map_with_span(|(name, body), span| ast::Root::Impl(ast::Impl { name, body, span }))
            .labelled("impl");

        let _return = just(Token::Return)
            .ignore_then(expression.clone().labelled("return value").or_not())
            .then_ignore(just(Token::Ctrl(';')))
            .map_with_span(|expr, span| ast::Root::Return(ast::Return { value: expr, span }))
            .labelled("return");

        let _struct_field = identifier
            .clone()
            .then_ignore(just(Token::Ctrl(':')))
            .then(identifier);

        let _struct = just(Token::Struct)
            .ignore_then(identifier.labelled("struct name"))
            .then(
                _struct_field
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
                    // Attempt to recover anything that looks like a function body but contains errors
                    .recover_with(nested_delimiters(
                        Token::Ctrl('{'),
                        Token::Ctrl('}'),
                        [
                            (Token::Ctrl('('), Token::Ctrl(')')),
                            (Token::Ctrl('['), Token::Ctrl(']')),
                        ],
                        |_| vec![],
                    )),
            )
            .map_with_span(|(name, fields), span| {
                ast::Root::Struct(ast::Struct { name, fields, span })
            })
            .labelled("struct");

        // let let_ = just(Token::Let)
        //     .ignore_then(identifier)
        //     .then_ignore(just(Token::Op(Op::Eq)))
        //     .then(expression.clone().labelled("let value"))
        //     .then_ignore(just(Token::Ctrl(';')))
        //     .map_with_span(|(id, expr), span| {
        //         ast::Root::Let(ast::Let {
        //             name: id,
        //             to: Some(expr),
        //             value_type: None,
        //             span,
        //         })
        //     });

        let expression_statement = expression
            .clone()
            .then_ignore(just(Token::Ctrl(';')))
            .map(|expr| ast::Root::Expression(expr));

        shader_or_main_block
            .or(_let)
            .or(_if)
            .or(_return)
            .or(_struct)
            .or(_impl)
            .or(func)
            .or(import.clone().map(|imp| ast::Root::Import(imp)))
            .or(expression_statement)
            .repeated()
    })
}

// fn funcs_parser() -> impl Parser<Token, Vec<ast::Root>, Error = Simple<Token>> + Clone {
//     // let ident = filter_map(|span, tok| match tok {
//     //     Token::Ident(ident) => Ok(ident.clone()),
//     //     _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
//     // });

//     // // Argument lists are just identifiers separated by commas, surrounded by parentheses
//     // let args = ident
//     //     .clone()
//     //     .separated_by(just(Token::Ctrl(',')))
//     //     .allow_trailing()
//     //     .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
//     //     .labelled("function args");

//     // let func = just(Token::Fn)
//     //     .ignore_then(identifier.labelled("function name"))
//     //     .then(args)
//     //     .then(
//     //         expr_parser()
//     //             .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
//     //             // Attempt to recover anything that looks like a function body but contains errors
//     //             .recover_with(nested_delimiters(
//     //                 Token::Ctrl('{'),
//     //                 Token::Ctrl('}'),
//     //                 [
//     //                     (Token::Ctrl('('), Token::Ctrl(')')),
//     //                     (Token::Ctrl('['), Token::Ctrl(']')),
//     //                 ],
//     //                 |span| (Expr::Error, span),
//     //             )),
//     //     )
//     //     .map(|((name, args), body)| (name, Func { args, body }))
//     //     .labelled("function");

//     // func.map(|(name, func)| ast::Root::Function(ast::Function { name }))
//     //     .or(import.map(|import| ast::Root::Import(import)))
//     //     .or(block_parser().map(|block2| block2))
//     //     .repeated()
//     //     .then_ignore(end())
// }

pub struct ParseError {
    pub span: Span,
    pub msg: String,
    pub simple_msg: String,
}

pub fn parse(file_name: String, src: &str) -> (Option<Vec<ast::Root>>, Vec<ParseError>) {
    let (tokens, mut errs) = lexer().parse_recovery(src);

    let (ast, parse_errs) = if let Some(tokens) = tokens {
        //dbg!(tokens);
        let len = src.chars().count();
        let (ast, parse_errs) = block_parser()
            .then_ignore(end())
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        // if let Some(funcs) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
        //     if let Some(main) = funcs.get("main") {
        //         assert_eq!(main.args.len(), 0);
        //         match eval_expr(&main.body, &funcs, &mut Vec::new()) {
        //             Ok(val) => println!("Return value: {}", val),
        //             Err(e) => errs.push(Simple::custom(e.span, e.msg)),
        //         }
        //     } else {
        //         panic!("No main function!");
        //     }
        // }

        (ast, parse_errs)
    } else {
        (None, vec![])
    };

    let mut err = Vec::<ParseError>::new();

    errs.into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .for_each(|e| {
            let s: &str = &file_name;

            let report = Report::build(ReportKind::Error, s, 1);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new((s, span.clone()))
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((s, span.clone()))
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new((s, e.span().clone()))
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new((s, e.span().clone()))
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            let simple_msg = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
                    format!(
                        "Unclosed delimiter {}, must be closed before {}",
                        delimiter,
                        e.found().unwrap_or(&"end of file".to_string())
                    )
                }
                chumsky::error::SimpleReason::Unexpected => format!(
                    "{}, expected {}",
                    if e.found().is_some() {
                        format!("Unexpected token {} in input", e.found().unwrap())
                    } else {
                        format!("Unexpected end of input")
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ),
                chumsky::error::SimpleReason::Custom(msg) => msg.to_string(),
            };

            let mut output = BufWriter::new(Vec::new());
            report
                .finish()
                .write((s, Source::from(&src)), &mut output)
                // .print(Source::from(&src))
                .unwrap();

            let output_str = str::from_utf8(output.by_ref().buffer()).unwrap().to_owned();

            err.push(ParseError {
                span: e.span(),
                msg: output_str,
                simple_msg,
            });
        });

    (ast, err)
}
