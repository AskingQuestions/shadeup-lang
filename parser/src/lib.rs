use crate::validator::TypedIntermediate;
use std::collections::HashMap;

pub mod ast;
pub mod generator;
pub mod graph;
pub mod parser2;
pub mod printer;
pub mod validator;

use std::str;

use crate::printer::AlertLevel;
use std::io::{BufWriter, Write};

use ast::{Span, USizeTuple};
use printer::SpannedAlert;
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

// #[derive(Debug, Clone, Copy)]
// #[cfg_attr(feature = "wasm", wasm_bindgen)]
// pub enum AlertLevel {
//     Error,
//     Warning,
//     Info,
// }

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct SourceLocation {
    pub start_line_and_column: ast::USizeTuple,
    pub end_line_and_column: ast::USizeTuple,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Alert {
    message: String,
    simple_message: String,
    pub level: AlertLevel,
    pub location: SourceLocation,
    pub span: ast::USizeTuple,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl Alert {
    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn message(&self) -> String {
        self.message.clone()
    }

    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn simple_message(&self) -> String {
        self.simple_message.clone()
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub name: String,
    pub source: String,
    pub ast: Option<Vec<ast::Root>>,
    pub validation: Vec<SpannedAlert>,
    pub alerts: Vec<Alert>,
    pub generated: String,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Environment {
    files: HashMap<String, File>,
    graph: graph::SymbolGraph,
}

fn capture_ariadne_message<'a>(
    file_name: &'a str,
    src: &'a str,
    report: ariadne::Report<(&'a str, Span)>,
) -> String {
    let mut output = BufWriter::new(Vec::new());
    report
        .write((file_name, ariadne::Source::from(&src)), &mut output)
        .unwrap();

    str::from_utf8(output.by_ref().buffer()).unwrap().to_owned()
}

fn level_to_ariadne_level(level: AlertLevel) -> ariadne::ReportKind {
    match level {
        AlertLevel::Error => ariadne::ReportKind::Error,
        AlertLevel::Warning => ariadne::ReportKind::Warning,
        AlertLevel::Info => ariadne::ReportKind::Advice,
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            files: HashMap::new(),
            graph: graph::SymbolGraph::new(),
        }
    }

    pub fn set_file(&mut self, name: String, source: String) {
        self.files.insert(
            name.clone(),
            File {
                name,
                source,
                ast: None,
                validation: Vec::new(),
                alerts: Vec::new(),
                generated: String::new(),
            },
        );
    }

    pub fn get_file(&mut self, name: &str) -> Option<&mut File> {
        self.files.get_mut(name)
    }

    pub fn parse_file(&mut self, name: &str) -> Result<(), ()> {
        let file = self.files.get_mut(name).unwrap();

        let source = file.source.as_str();

        let (ast, errs) = parser2::parse(name.to_owned(), source);

        let mut alerts = vec![];

        for err in errs {
            let loc =
                ast::Location::new(name.to_string(), USizeTuple(err.span.start, err.span.end));

            alerts.push(Alert {
                message: err.msg,
                simple_message: err.simple_msg,
                level: AlertLevel::Error,
                location: SourceLocation {
                    start_line_and_column: loc.get_start_line_and_column(source),
                    end_line_and_column: loc.get_end_line_and_column(source),
                },
                span: USizeTuple(err.span.start, err.span.end),
            });
        }

        if ast.is_some() {
            let ast_ref = ast.as_ref().unwrap();
            let mut validation_errors = self.graph.update_file_first_pass(name, ast_ref);
            file.ast = ast;
            file.validation.extend(validation_errors);
        } else {
            self.graph
                .update_file_first_pass(name, ast.as_ref().unwrap());
        }

        file.alerts.extend(alerts);

        Ok(())
    }

    pub fn process_file(&mut self, name: &str) -> Result<(), ()> {
        let mut alerts = vec![];

        let mut generated = String::new();
        let file = self.files.get_mut(name).unwrap();
        if let Some(ref ast_ref) = file.ast {
            file.validation
                .extend(self.graph.update_file_second_pass(name, ast_ref));

            let (_alerts, typed) = validator::validate(&self.graph, name);
            file.validation.extend(_alerts);

            generated = generator::generate(&self.graph, name, &typed).javascript;
        } else {
            let ast = vec![];
            self.graph.update_file_second_pass(name, &ast);
        }

        let file = self.files.get(name).unwrap();
        for err in &file.validation {
            let source = file.source.as_str();
            let (pretty_msg, simple_msg) = if err.other_message.is_some()
                && err.other_location.is_some()
            {
                (
                    capture_ariadne_message(
                        name,
                        source,
                        ariadne::Report::build(
                            level_to_ariadne_level(err.level),
                            name,
                            err.location.span.0,
                        )
                        .with_message(err.label.clone())
                        .with_label(
                            ariadne::Label::new((name, err.location.span.0..err.location.span.1))
                                .with_message(err.message.clone()),
                        )
                        .with_label(
                            ariadne::Label::new((
                                name,
                                err.other_location.clone().unwrap().span.0
                                    ..err.other_location.clone().unwrap().span.1,
                            ))
                            .with_message(err.other_message.clone().unwrap()),
                        )
                        .finish(),
                    ),
                    format!(
                        "{}: {}, {}",
                        err.label.clone(),
                        err.message.clone(),
                        err.other_message.clone().unwrap()
                    ),
                )
            } else {
                (
                    capture_ariadne_message(
                        name,
                        source,
                        ariadne::Report::build(
                            level_to_ariadne_level(err.level),
                            name,
                            err.location.span.0,
                        )
                        .with_message(err.label.clone())
                        .with_label(
                            ariadne::Label::new((name, err.location.span.0..err.location.span.1))
                                .with_message(err.message.clone()),
                        )
                        .finish(),
                    ),
                    format!("{}: {}", err.label.clone(), err.message.clone()),
                )
            };

            alerts.push(Alert {
                message: pretty_msg,
                simple_message: simple_msg,
                level: err.level,
                location: SourceLocation {
                    start_line_and_column: err
                        .location
                        .get_start_line_and_column(file.source.as_str()),
                    end_line_and_column: err.location.get_end_line_and_column(file.source.as_str()),
                },
                span: USizeTuple(err.location.span.0, err.location.span.1),
            });
        }

        self.get_file(name).unwrap().alerts.extend(alerts);
        self.get_file(name).unwrap().generated = generated;

        return Ok(());
    }
}
