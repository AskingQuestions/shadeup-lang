use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

use chumsky::{prelude::*, stream::Stream};
use std::fmt;
use std::io::{BufWriter, Write};
use std::str;

use crate::ast::{self, Op, Value};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, PartialEq, Eq, Hash)]
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
    In,
    For,
    While,
    Break,
    Continue,
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
            Token::In => write!(f, "in"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let real = text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .then(
            just('e')
                .or(just('E'))
                .chain::<char, _, _>(text::digits(10))
                .collect::<String>()
                .or_not(),
        )
        .map(|(num, exp)| {
            let mut s = num;
            if let Some(exp) = exp {
                s.push_str(&exp);
            }
            Token::Real(s)
        });

    // A parser for numbers
    let int = text::int(10).validate(|s: String, span, emit| {
        let par = s.parse::<i64>();
        match par {
            Ok(n) => Token::Int(n),
            Err(err) => {
                emit(Simple::custom(span, format!("Number parse error {}", err)));
                Token::Int(0)
            }
        }
    });

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
        // Bit/Logic
        just("&&").to(Op::AndAnd),
        just('&').to(Op::And),
        just("||").to(Op::BarBar),
        just('|').to(Op::Bar),
        just('^').to(Op::Hat),
        just('~').to(Op::Tilda),
        just("<<").to(Op::DoubleLeft),
        just(">>").to(Op::DoubleRight),
        just("<=").to(Op::LessEq),
        just('<').to(Op::Less),
        just(">=").to(Op::MoreEq),
        just('>').to(Op::More),
    ))
    .or(choice((
        // Compare
        just("==").to(Op::EqEq),
        just("!=").to(Op::NotEq),
        just('!').to(Op::Not),
        // Mutate
        just("+=").to(Op::PlusEq),
        just("-=").to(Op::MinusEq),
        just("++").to(Op::Join),
        just("--").to(Op::SubSub),
        just("=>").to(Op::RFlow),
        just("<-").to(Op::LArrow),
        just("->").to(Op::RArrow),
        just('=').to(Op::Eq),
        // Special
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
        just("::").to(Op::DoubleColon),
    )))
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
        "for" => Token::For,
        "null" => Token::Null,
        "as" => Token::As,
        "from" => Token::From,
        "import" => Token::Import,
        "shader" => Token::Shader,
        "struct" => Token::Struct,
        "pub" => Token::Pub,
        "impl" => Token::Impl,
        "in" => Token::In,
        "while" => Token::While,
        "break" => Token::Break,
        "continue" => Token::Continue,

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

macro_rules! end_with_semicolon {
    ($main:expr) => {
        $main
            .then(
                just(Token::Ctrl(';'))
                    .map(|_| ';')
                    .or(just(Token::Ctrl('}')).rewind().map(|_| '}'))
                    .or(end().rewind().map(|_| 'E')),
            )
            .validate(|(passthrough, terminator), span, emit| {
                if terminator != ';' {
                    emit(Simple::custom(span, format!("Missing ;")));
                }

                passthrough
            })
    };
}

fn assemble_op_list(my: &[(ast::Expression, Option<Op>, Span)]) -> ast::Expression {
    let mut levels = vec![
        vec![Op::Sq],
        vec![Op::Mul, Op::Div, Op::Rem],
        vec![Op::Add, Op::Sub],
        vec![Op::DoubleLeft, Op::DoubleRight],
        vec![Op::Less, Op::LessEq, Op::More, Op::MoreEq],
        vec![Op::EqEq, Op::NotEq],
        vec![Op::And],
        vec![Op::Hat],
        vec![Op::Bar],
        vec![Op::AndAnd],
        vec![Op::BarBar],
        vec![
            Op::Eq,
            Op::PlusEq,
            Op::MinusEq,
            Op::Join,
            Op::SubSub,
            Op::RFlow,
            Op::LArrow,
            Op::RArrow,
            Op::DoubleColon,
        ],
    ];

    levels.reverse();

    if my.len() == 2 {
        if my[0].1.is_some() {
            ast::Expression::Op((
                Box::new(my[0].0.clone()),
                my[0].1.as_ref().unwrap().clone(),
                Box::new(my[1].0.clone()),
                my[0].2.start..my[1].2.end,
            ))
        } else {
            ast::Expression::Op((
                Box::new(my[0].0.clone()),
                Op::Add,
                Box::new(my[1].0.clone()),
                my[0].2.start..my[1].2.end,
            ))
        }
    } else if my.len() == 1 {
        if let Some(ref op) = my[0].1 {
            ast::Expression::Op((
                Box::new(my[0].0.clone()),
                op.clone(),
                Box::new(ast::Expression::Error(((), my[0].2.clone()))),
                my[0].2.clone(),
            ))
        } else {
            my[0].0.clone()
        }
        // match my[0].1 {
        //     Some(ref op) => ast::Expression::Op((
        //         Box::new(my[0].0.clone()),
        //         op.clone(),
        //         Box::new(ast::Expression::Error(((), my[0].2.clone()))),
        //         my[0].2.clone(),
        //     )),
        //     None => my[0].0.clone(),
        // }
    } else {
        // Find the first highest level operator and split recursively
        // Build a list of expr + operators with equal precedence
        for level in levels {
            let mut has_level = false;

            for (i, (_, op, _)) in my.iter().enumerate() {
                if let Some(op) = op {
                    if level.contains(op) {
                        has_level = true;
                        break;
                    }
                }
            }

            if has_level {
                // Example:
                // 0 + 0 = 0 + 0 + 0 += 0
                //       |           |
                // list = [(0 + 0), (0 + 0 + 0), (0)]
                // ops = [=, +=]

                let mut list = vec![];
                let mut ops = vec![];

                let mut cur = vec![];

                for (expr, op, span) in my {
                    if let Some(op) = op {
                        if level.contains(op) {
                            cur.push((expr.clone(), None, span.clone()));
                            list.push(cur);
                            ops.push(op.clone());
                            cur = vec![];
                        } else {
                            cur.push((expr.clone(), Some(op.clone()), span.clone()));
                        }
                    } else {
                        cur.push((expr.clone(), None, span.clone()));
                    }
                }

                if cur.len() > 0 {
                    ops.push(Op::Add); // Doesn't matter
                    list.push(cur);
                }

                if list.len() == 2 {
                    return ast::Expression::Op((
                        Box::new(assemble_op_list(&list[0])),
                        ops[0].clone(),
                        Box::new(assemble_op_list(&list[1])),
                        list[0][0].2.clone(),
                    ));
                } else {
                    let mut new_list = vec![];

                    for (i, list) in list.iter().enumerate() {
                        new_list.push(assemble_op_list(list));

                        if i < ops.len() {
                            let a = new_list.pop().unwrap();
                            if let Some(b) = new_list.pop() {
                                new_list.push(ast::Expression::Op((
                                    Box::new(b),
                                    ops[i - 1].clone(),
                                    Box::new(a),
                                    list[0].2.clone(),
                                )));
                            } else {
                                new_list.push(a);
                            }
                        } else {
                        }
                    }

                    let expr = new_list.pop().unwrap();
                    // let mut it = list.iter().zip(ops.iter());
                    // let first = it.next().unwrap();
                    // let first = ast::Expression::Op((
                    //     Box::new(assemble_op_list(&first.0)),
                    //     first.1.clone(),
                    //     Box::new(ast::Expression::Error(((), 0..0))),
                    //     first.0[0].2.clone(),
                    // ));

                    // let expr = it.fold(first, |acc, (list, op)| {
                    //     ast::Expression::Op((
                    //         Box::new(acc),
                    //         op.clone(),
                    //         Box::new(assemble_op_list(list)),
                    //         list[0].2.clone(),
                    //     ))
                    // });

                    dbg!(&expr);

                    return expr;
                    // return it.fold(first, |a, b| match a {
                    //     ast::Expression::Op(op) => ast::Expression::Op((
                    //         op.0.clone(),
                    //         op.1.clone(),
                    //         Box::new(ast::Expression::Op((
                    //             Box::new(assemble_op_list(&b.0)),
                    //             b.1.clone(),
                    //             Box::new(ast::Expression::Error(((), 0..0))),
                    //             b.0[0].2.clone(),
                    //         ))),
                    //         op.3.clone(),
                    //     )),
                    //     _ => unreachable!(),
                    // });
                }
            }
        }

        // let it = my.iter();
        // let first = it.next().unwrap();
        // it.fold(first, |a, b| {

        // });
        // for (i, op) in my.iter().fold.enumerate() {
        //     for level in levels.iter().rev() {
        //         if level.contains(&op.1.as_ref().unwrap()) {
        //             let left = &my[..i + 1];
        //             let right = &my[i + 1..];

        //             return ast::Expression::Op((
        //                 Box::new(assemble_op_list(&left)),
        //                 op.1.as_ref().unwrap().clone(),
        //                 Box::new(assemble_op_list(&right)),
        //                 op.2.clone(),
        //             ));
        //         }
        //     }
        // }

        ast::Expression::Error(((), 0..0))
    }
}

fn simple_op(
    in_sides: impl Parser<Token, ast::Expression, Error = Simple<Token>>,
    in_op: impl Parser<Token, Op, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, ast::Expression, Error = Simple<Token>> {
    let expr = in_sides
        .then(in_op.or_not())
        .map_with_span(|(left, op), span| (left, op, span))
        .repeated()
        .at_least(1)
        .map(|list| {
            assemble_op_list(&list)
            // let span = a_expr.get_span().start..b_expr.get_span().end;
            // ast::Expression::Op((Box::new(a_expr), op, Box::new(b_expr), span))
        });

    expr
}
fn op_chain(
    in_op: impl Parser<Token, ast::Expression, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, ast::Expression, Error = Simple<Token>> {
    // let op = just(Token::Op(Op::Sq)).to(Op::Sq);
    // let exponentiation = simple_op(in_op, op.clone());

    let op = choice((
        // Level 18
        just(Token::Op(Op::Sq)).to(Op::Sq),
        // Level 17
        just(Token::Op(Op::Mul)).to(Op::Mul),
        just(Token::Op(Op::Div)).to(Op::Div),
        just(Token::Op(Op::Rem)).to(Op::Rem),
        // Level 16
        just(Token::Op(Op::Add)).to(Op::Add),
        just(Token::Op(Op::Sub)).to(Op::Sub),
        // Level 15
        just(Token::Op(Op::DoubleLeft)).to(Op::DoubleLeft),
        just(Token::Op(Op::DoubleRight)).to(Op::DoubleRight),
        // Level 14
        just(Token::Op(Op::Less)).to(Op::Less),
        just(Token::Op(Op::LessEq)).to(Op::LessEq),
        just(Token::Op(Op::More)).to(Op::More),
        just(Token::Op(Op::MoreEq)).to(Op::MoreEq),
    ))
    .or(choice((
        // Level 13
        just(Token::Op(Op::EqEq)).to(Op::EqEq),
        just(Token::Op(Op::NotEq)).to(Op::NotEq),
        // Level 12
        just(Token::Op(Op::And)).to(Op::And),
        // Level 11
        just(Token::Op(Op::Hat)).to(Op::Hat),
        // Level 10
        just(Token::Op(Op::Bar)).to(Op::Bar),
        // Level 9
        just(Token::Op(Op::AndAnd)).to(Op::AndAnd),
        // Level 8
        just(Token::Op(Op::BarBar)).to(Op::BarBar),
        // Level 7
        just(Token::Op(Op::Eq)).to(Op::Eq),
        just(Token::Op(Op::PlusEq)).to(Op::PlusEq),
        just(Token::Op(Op::MinusEq)).to(Op::MinusEq),
        just(Token::Op(Op::Join)).to(Op::Join),
        just(Token::Op(Op::SubSub)).to(Op::SubSub),
        just(Token::Op(Op::RFlow)).to(Op::RFlow),
        just(Token::Op(Op::LArrow)).to(Op::LArrow),
        just(Token::Op(Op::RArrow)).to(Op::RArrow),
        just(Token::Op(Op::DoubleColon)).to(Op::DoubleColon),
    )));

    let expr = in_op
        .then(op.or_not())
        .map_with_span(|(left, op), span| (left, op, span))
        .repeated()
        .at_least(1)
        .map(|list| assemble_op_list(&list));

    // let op = just(Token::Op(Op::Mul))
    //     .to(Op::Mul)
    //     .or(just(Token::Op(Op::Div)).to(Op::Div))
    //     .or(just(Token::Op(Op::Rem)).to(Op::Rem));
    // let product = simple_op(exponentiation, op.clone());

    // let op = just(Token::Op(Op::Add))
    //     .to(Op::Add)
    //     .or(just(Token::Op(Op::Sub)).to(Op::Sub));
    // let sum = simple_op(product, op.clone());

    // let op = just(Token::Op(Op::DoubleLeft))
    //     .to(Op::DoubleLeft)
    //     .or(just(Token::Op(Op::DoubleRight)).to(Op::DoubleRight));
    // let bitwise = simple_op(sum, op.clone());

    // // Comparison ops (equal, not-equal) have equal precedence
    // let op = just(Token::Op(Op::Less))
    //     .to(Op::Less)
    //     .or(just(Token::Op(Op::LessEq)).to(Op::LessEq))
    //     .or(just(Token::Op(Op::More)).to(Op::More))
    //     .or(just(Token::Op(Op::MoreEq)).to(Op::MoreEq));
    // let compare_num = simple_op(bitwise, op.clone());

    // let op = just(Token::Op(Op::EqEq))
    //     .to(Op::EqEq)
    //     .or(just(Token::Op(Op::NotEq)).to(Op::NotEq));
    // let compare = simple_op(compare_num, op.clone());

    // let op = just(Token::Op(Op::And)).to(Op::And);
    // let and = simple_op(compare, op.clone());

    // let op = just(Token::Op(Op::Hat)).to(Op::Hat);
    // let hat = simple_op(and, op.clone());

    // let op = just(Token::Op(Op::Bar)).to(Op::Bar);
    // let bar = simple_op(hat, op.clone());

    // let op = just(Token::Op(Op::AndAnd)).to(Op::AndAnd);
    // let and_and = simple_op(bar, op.clone());

    // let op = just(Token::Op(Op::BarBar)).to(Op::BarBar);
    // let bar_bar = simple_op(and_and, op.clone());

    // let op = just(Token::Op(Op::Eq))
    //     .to(Op::Eq)
    //     .or(just(Token::Op(Op::PlusEq)).to(Op::PlusEq))
    //     .or(just(Token::Op(Op::MinusEq)).to(Op::MinusEq))
    //     .or(just(Token::Op(Op::Join)).to(Op::Join))
    //     .or(just(Token::Op(Op::SubSub)).to(Op::SubSub))
    //     .or(just(Token::Op(Op::RFlow)).to(Op::RFlow))
    //     .or(just(Token::Op(Op::LArrow)).to(Op::LArrow))
    //     .or(just(Token::Op(Op::RArrow)).to(Op::RArrow));
    // let assign = simple_op(bar_bar, op.clone());

    return expr;
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
        .map_with_span(|(names, path), _span| ast::Import { name: names, path })
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
                    |_span| Token::Fn,
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

        let expression =
            recursive(|expr| {
                let val = select! {
                    Token::Null => Value::Null,
                    Token::Bool(x) => Value::Bool(x),
                    Token::Int(x) => Value::Int(x),
                    Token::Real(n) => {
                        let has_exponent = n.chars().any(|c| c == 'e' || c == 'E');
                        let lower = n.to_lowercase();
                        let split = lower.split('e').collect::<Vec<_>>();
                        let (mantissa, exponent) = if split.len() == 2 {
                            (split[0], split[1])
                        } else {
                            (n.as_str(), "0")
                        };

                        let mantissa = mantissa.parse::<f64>().unwrap();
                        let exponent = exponent.parse::<i32>().unwrap();

                        Value::Real(mantissa * 10f64.powi(exponent))
                    },
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

                let unary_pre = just(Token::Op(Op::Sub))
                    .to(Op::Sub)
                    .or(just(Token::Op(Op::SubSub)).to(Op::SubSub))
                    .or(just(Token::Op(Op::Join)).to(Op::Join))
                    .or(just(Token::Op(Op::Not)).to(Op::Not))
                    .or(just(Token::Op(Op::Tilda)).to(Op::Tilda))
                    .then(atom.clone())
                    .map_with_span(|(op, expr), span| {
                        ast::Expression::Op((
                            Box::new(ast::Expression::Error(((), span.clone()))),
                            op,
                            Box::new(expr),
                            span,
                        ))
                    });

                let op = just(Token::Op(Op::Dot)).to(Op::Dot);
                let dot =
                    unary_pre
                        .clone()
                        .then(
                            op.clone()
                                .then(unary_pre.clone().or_not().map_with_span(
                                    |val, span| match val {
                                        Some(val) => val,
                                        None => ast::Expression::Error(((), span)),
                                    },
                                ))
                                .repeated(),
                        )
                        .foldl(|a, (op, b)| {
                            let span = a.get_span().start..b.get_span().end;
                            ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                        });

                // Function calls have very high precedence so we prioritise them
                let ident_expr = identifier
                    .clone()
                    .map(|id| ast::Expression::Identifier((id.clone(), id.span)));
                let call = ident_expr
                    .clone()
                    .then(
                        items
                            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                            .map_with_span(|args, span: Span| (args, span))
                            .repeated(),
                    )
                    .foldl(|f, args| {
                        let span = f.get_span().start..args.1.end;
                        let ident_extract = match f {
                            ast::Expression::Identifier((id, _)) => id,
                            _ => unreachable!(),
                        };
                        ast::Expression::Call((
                            ast::Call {
                                args: args.0,
                                expression: Box::new(ast::Expression::Error((
                                    (),
                                    ident_extract.span.clone(),
                                ))),
                                method: Some(ident_extract),
                                span: span.clone(),
                            },
                            span,
                        ))
                    });

                let op = just(Token::Op(Op::Dot)).to(Op::Dot);
                let call_dot_chain = call
                    .clone()
                    .or(unary_pre.clone())
                    .or(atom.clone())
                    .then(
                        op.clone()
                            .then(call.clone().or(ident_expr.clone()).or_not().map_with_span(
                                |val, span| match val {
                                    Some(val) => val,
                                    None => ast::Expression::Error(((), span)),
                                },
                            ))
                            .repeated(),
                    )
                    .foldl(|a, (op, b)| {
                        let span = a.get_span().start..b.get_span().end;

                        match b {
                            ast::Expression::Call(call) => ast::Expression::Call((
                                ast::Call {
                                    args: call.0.args,
                                    expression: Box::new(a),
                                    method: call.0.method,
                                    span: call.1.clone(),
                                },
                                call.1,
                            )),
                            _ => ast::Expression::Op((Box::new(a), op, Box::new(b), span)),
                        }
                    });
                // .map(|(a, b)| {
                //     let span = a.get_span().start..b.last().unwrap().1.end;
                //     b.into_iter().fold(a, |a, (op, b)| {
                //         ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                //     })
                // });

                // let op = just(Token::Op(Op::Dot)).to(Op::Dot);
                // let dot_call =
                //     call.clone()
                //         .then(
                //             op.clone()
                //                 .then(call.clone().or(atom.clone()).or_not().map_with_span(
                //                     |val, span| match val {
                //                         Some(val) => val,
                //                         None => ast::Expression::Error(((), span)),
                //                     },
                //                 ))
                //                 .repeated(),
                //         )
                //         .foldl(|a, (op, b)| {
                //             let span = a.get_span().start..b.get_span().end;
                //             ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                //         });

                let op = just(Token::Op(Op::Dot)).to(Op::Dot);
                let dot_call = call
                    .clone()
                    .then(
                        op.clone()
                            .then(call.clone().or(atom.clone()).or_not().map_with_span(
                                |val, span| match val {
                                    Some(val) => val,
                                    None => ast::Expression::Error(((), span)),
                                },
                            ))
                            .repeated(),
                    )
                    .foldl(|a, (op, b)| {
                        let span = a.get_span().start..b.get_span().end;
                        ast::Expression::Op((Box::new(a), op, Box::new(b), span))
                    });

                let bar_bar = op_chain(call_dot_chain);

                // Sum ops (add and subtract) have equal precedence

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
                let ternary = bar_bar;

                ternary
            });

        let args = identifier
            .clone()
            .then(
                just(Token::Ctrl(':'))
                    .ignore_then(identifier.clone())
                    .or_not(),
            )
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

        let let_end = end_with_semicolon!(just(Token::Op(Op::Eq))
            .then(expression.clone().labelled("let value"))
            .map(|(_, expr)| Some(expr)));

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

        let bounded_block = block_recur
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
                |_span: Span| vec![],
            ))
            .map_with_span(|roots, span| ast::Block {
                roots: roots,
                span: span.clone(),
            });

        let _for_each = just(Token::For)
            .ignore_then(
                identifier
                    .labelled("for identifier")
                    .then(
                        just(Token::Ctrl(','))
                            .ignore_then(identifier.clone())
                            .or_not(),
                    )
                    .then_ignore(just(Token::In))
                    .then(expression.clone().labelled("for iterable"))
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(bounded_block.clone())
            .map_with_span(|(((variable, counter), iterable), body), span| {
                ast::Root::ForEach(ast::ForEach {
                    counter,
                    iterable,
                    variable,
                    body,
                    span,
                })
            })
            .labelled("for");

        let _for = just(Token::For)
            .ignore_then(
                _let.clone()
                    .then(expression.clone().labelled("for condition"))
                    .then_ignore(just(Token::Ctrl(';')))
                    .then(expression.clone().labelled("for increment"))
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(bounded_block.clone())
            .map_with_span(|((((variable), condition), increment), body), span| {
                ast::Root::For(ast::For {
                    first: Box::new(variable),
                    second: condition,
                    third: increment,
                    body,
                    span,
                })
            })
            .labelled("for");

        let _while = just(Token::While)
            .ignore_then(
                expression
                    .clone()
                    .labelled("while condition")
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(bounded_block.clone())
            .map_with_span(|(condition, body), span| {
                ast::Root::While(ast::While {
                    condition,
                    body,
                    span,
                })
            })
            .labelled("while");

        let _break = end_with_semicolon!(just(Token::Break))
            .map_with_span(|_, span| ast::Root::Break(span))
            .labelled("break");

        let _continue = end_with_semicolon!(just(Token::Continue))
            .map_with_span(|_, span| ast::Root::Continue(span))
            .labelled("continue");

        let _if = just(Token::If)
            .ignore_then(
                expression
                    .clone()
                    .labelled("if condition")
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(
                bounded_block.clone().then(
                    just(Token::Else)
                        .ignore_then(
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
                                            |_span: Span| vec![],
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
                        )
                        .or_not(),
                ),
            )
            .map_with_span(|(cond, (block, ifs)), span| {
                let mut else_body: Option<ast::Block> = None;
                let mut else_ifs = vec![];
                if let Some(ifs) = ifs {
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
                        |_span: Span| vec![],
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

        let expression_statement =
            end_with_semicolon!(expression.clone().map(|expr| ast::Root::Expression(expr)));

        shader_or_main_block
            .or(_while)
            .or(_let)
            .or(_if)
            .or(_return)
            .or(_break)
            .or(_continue)
            .or(_for)
            .or(_for_each)
            .or(_struct)
            .or(_impl)
            .or(func)
            .or(import.clone().map(|imp| ast::Root::Import(imp)))
            .or(expression_statement)
            .repeated()
    })
}

pub struct ParseError {
    pub span: Span,
    pub msg: String,
    pub simple_msg: String,
}

pub fn parse(file_name: String, src: &str) -> (Option<Vec<ast::Root>>, Vec<ParseError>) {
    let (tokens, errs) = lexer().parse_recovery(src);

    let (ast, parse_errs) = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (ast, parse_errs) = block_parser()
            .then_ignore(end())
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

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
                chumsky::error::SimpleReason::Unclosed { span: _, delimiter } => {
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
