use std::collections::HashMap;

pub mod ast;
pub mod lexer;
pub mod parser2;
pub mod printer;

use crate::printer::AlertLevel;

use ast::USizeTuple;
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
    pub alerts: Vec<Alert>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Environment {
    files: HashMap<String, File>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            files: HashMap::new(),
        }
    }

    pub fn set_file(&mut self, name: String, source: String) {
        self.files.insert(
            name.clone(),
            File {
                name,
                source,
                alerts: Vec::new(),
            },
        );
    }

    pub fn get_file(&mut self, name: &str) -> Option<&mut File> {
        self.files.get_mut(name)
    }

    pub fn parse_file(&mut self, name: &str) -> Result<(), ()> {
        let file = self.get_file(name).unwrap();

        let result = parser2::parse(file.source.as_str());

        file.alerts = vec![];

        for err in result {
            let loc = ast::Location {
                file: 0,
                span: USizeTuple(err.span.start, err.span.end),
            };

            file.alerts.push(Alert {
                message: err.msg,
                simple_message: err.simple_msg,
                level: AlertLevel::Error,
                location: SourceLocation {
                    start_line_and_column: loc.get_start_line_and_column(file.source.as_str()),
                    end_line_and_column: loc.get_end_line_and_column(file.source.as_str()),
                },
                span: USizeTuple(err.span.start, err.span.end),
            });
        }

        return Ok(());
    }
}
