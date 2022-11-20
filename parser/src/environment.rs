use crate::validator::{TypedExpression, TypedIntermediate, TypedStatement};
use std::collections::HashMap;

use std::str;

use crate::printer::AlertLevel;
use std::io::{BufWriter, Write};

use crate::ast::{Span, USizeTuple};
use crate::graph::SymbolDefinition;
use crate::printer::SpannedAlert;
use crate::validator::{ExpandedType, TypedBody, TypedFunction, TypedFunctionParameter};

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

#[derive(Clone, Copy)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct SourceLocation {
    pub start_line_and_column: crate::ast::USizeTuple,
    pub end_line_and_column: crate::ast::USizeTuple,
}

#[allow(dead_code)]
#[derive(Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Alert {
    message: String,
    simple_message: String,
    pub level: AlertLevel,
    pub location: SourceLocation,
    pub span: crate::ast::USizeTuple,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl Alert {
    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn message(&self) -> String {
        self.message.clone()
    }

    #[allow(dead_code)]
    #[cfg_attr(feature = "wasm", wasm_bindgen(getter))]
    pub fn simple_message(&self) -> String {
        self.simple_message.clone()
    }
}

#[derive(Clone)]
pub struct File {
    pub name: String,
    pub source: String,
    pub ast: Option<Vec<crate::ast::Root>>,
    pub validation: Vec<SpannedAlert>,
    pub alerts: Vec<Alert>,
    pub typed: Option<TypedIntermediate>,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Environment {
    files: HashMap<String, File>,
    graph: crate::graph::SymbolGraph,
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
            graph: crate::graph::SymbolGraph::new(),
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
                typed: None,
            },
        );
    }

    pub fn get_file(&mut self, name: &str) -> Option<&mut File> {
        self.files.get_mut(name)
    }

    pub fn parse_file(&mut self, name: &str) -> Result<(), ()> {
        let file = self.files.get_mut(name).unwrap();

        let source = file.source.as_str();

        let (ast, errs) = crate::parser2::parse(name.to_owned(), source);

        let mut alerts = vec![];

        for err in errs {
            let loc = crate::ast::Location::new(
                name.to_string(),
                USizeTuple(err.span.start, err.span.end),
            );

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
            let validation_errors = self.graph.update_file_first_pass(name, ast_ref);
            file.ast = ast;
            file.validation.extend(validation_errors);
        } else {
            self.graph.update_file_first_pass(name, &Vec::new());
        }

        file.alerts.extend(alerts);

        Ok(())
    }

    pub fn process_file(&mut self, name: &str) -> Result<(), ()> {
        let mut alerts = vec![];

        let mut typed = None;

        let file = self.files.get_mut(name).unwrap();
        if let Some(ref ast_ref) = file.ast {
            file.validation
                .extend(self.graph.update_file_second_pass(name, ast_ref));

            let (_alerts, _typed) = crate::validator::validate(&self.graph, name);
            file.validation.extend(_alerts);

            typed = Some(_typed);

            // generated = generator::generate(&self.graph, name, &typed).javascript;
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
        self.get_file(name).unwrap().typed = typed;

        return Ok(());
    }

    pub fn generate_file(&mut self, name: &str) -> String {
        let mut typed = TypedIntermediate {
            functions: HashMap::new(),
            structs: Vec::new(),
            shaders: Vec::new(),
        };

        for (_, _type) in self.graph.primitive.iter() {
            if let SymbolDefinition::Type(ref sym_type) = _type.definition {
                for method in &sym_type.methods {
                    let func_typed = TypedFunction {
                        tagged: true,
                        tagging: false,
                        tags: method.1.tags.clone(),
                        body: TypedBody {
                            tags: vec![],
                            statements: vec![],
                        },
                        javascript: method.1.javascript.clone(),
                        name: method.0.clone(),
                        parameters: method
                            .1
                            .parameters
                            .iter()
                            .map(|(name, _type, _)| TypedFunctionParameter {
                                name: name.clone(),
                                default_value: None,
                                type_name: ExpandedType::from_string(&self.graph, name, _type),
                            })
                            .collect(),
                        return_type: ExpandedType::from_string(
                            &self.graph,
                            name,
                            method.1.return_type.as_ref().unwrap_or(&"void".to_string()),
                        ),
                    };
                    typed
                        .functions
                        .insert(format!("__{}_{}", &_type.name, &method.0), func_typed);
                }
            }
            if let SymbolDefinition::Function(ref sym_func) = _type.definition {
                let func_typed = TypedFunction {
                    tagged: true,
                    tagging: false,
                    tags: sym_func.tags.clone(),
                    body: TypedBody {
                        tags: vec![],
                        statements: vec![],
                    },
                    javascript: sym_func.javascript.clone(),
                    name: _type.name.clone(),
                    parameters: sym_func
                        .parameters
                        .iter()
                        .map(|(name, _type, _)| TypedFunctionParameter {
                            name: name.clone(),
                            default_value: None,
                            type_name: ExpandedType::from_string(&self.graph, name, _type),
                        })
                        .collect(),
                    return_type: ExpandedType::from_string(
                        &self.graph,
                        name,
                        sym_func.return_type.as_ref().unwrap_or(&"void".to_string()),
                    ),
                };
                typed
                    .functions
                    .insert(format!("{}", &_type.get_namespaced()), func_typed);
            }
        }

        for file in self.files.values() {
            if let Some(ref _typed) = file.typed {
                let mut funcs = _typed.functions.clone();
                shift_shader_ids(&mut funcs, typed.shaders.len());
                typed.functions.extend(funcs);
                typed.shaders.extend(_typed.shaders.clone());
                typed.structs.extend(_typed.structs.clone());
            }
        }

        let keys = typed.functions.keys().cloned().collect::<Vec<String>>();

        for k in keys {
            crate::validator::tag_function(&mut typed, &k);
        }

        typed = typed.tree_shake(
            &self.graph,
            name,
            &format!("{}_test", name.replace(".", "__")),
        );

        let mut generated = String::new();

        let file = self.files.get_mut(name).unwrap();
        if let Some(ref _ast_ref) = file.ast {
            generated = crate::generator::generate(&self.graph, name, &typed).javascript;
        }

        return generated;
    }
}

fn shift_shader_ids_expression(expr: &mut TypedExpression, by: usize) {
    match expr {
        TypedExpression::Shader(inst, _) => {
            inst.shader += by;
        }
        TypedExpression::KVMap(map, _) => {
            for (_, sub) in map.iter_mut() {
                shift_shader_ids_expression(sub, by);
            }
        }
        TypedExpression::Call(_, exprs, _) => {
            for expr in exprs.iter_mut() {
                shift_shader_ids_expression(expr, by);
            }
        }
        _ => {}
    };
}

fn shift_shader_ids_body(body: &mut TypedBody, by: usize) {
    for statement in body.statements.iter_mut() {
        match statement {
            TypedStatement::Expression(expr, _) => {
                shift_shader_ids_expression(expr, by);
            }
            TypedStatement::Return(expr, _) => {
                shift_shader_ids_expression(expr, by);
            }
            TypedStatement::Let {
                name: _,
                value,
                span: _,
            } => {
                shift_shader_ids_expression(value, by);
            }
            TypedStatement::If {
                condition,
                body: if_body,
                else_ifs,
                else_body,
                span: _,
            } => {
                shift_shader_ids_expression(condition, by);
                shift_shader_ids_body(if_body, by);
                for (condition, el_body) in else_ifs {
                    shift_shader_ids_expression(condition, by);
                    shift_shader_ids_body(el_body, by);
                }
                if let Some(else_body) = else_body {
                    shift_shader_ids_body(else_body, by);
                }
            }
        };
    }
}

fn shift_shader_ids(functions: &mut HashMap<String, TypedFunction>, by: usize) {
    for (_, func) in functions {
        shift_shader_ids_body(&mut func.body, by);
    }
}
