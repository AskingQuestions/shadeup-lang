use std::collections::HashMap;

use crate::ast::{self, Location, Span, USizeTuple};
use crate::graph::{SymbolDefinition, SymbolGraph, SymbolNode};
use crate::printer::SpannedAlert;

pub fn validate(graph: &SymbolGraph, file_name: &str) -> Vec<SpannedAlert> {
    let mut alerts = Vec::new();

    let file = graph.files.get(file_name);

    if file.is_none() {
        return alerts;
    }

    let file = file.unwrap();

    let check_type = |_alerts: &mut Vec<SpannedAlert>, name: ast::Identifier| {
        let primitive = graph.primitive.get(&name.name);

        if primitive.is_some() {
            return;
        }

        let symbol = file.get(&name.name);

        if symbol.is_none() {
            _alerts.push(SpannedAlert::error(
                "Undefined symbol".to_string(),
                format!(
                    "The symbol '{}' was not found in this file. Did you forget to import it?",
                    name.name
                ),
                Location::new(
                    file_name.to_string(),
                    USizeTuple(name.span.start, name.span.end),
                ),
            ));
        }
    };

    for symbol in file.values() {
        match &symbol.root {
            ast::Root::Function(function) => {
                for (i, param) in function.parameters.iter().enumerate() {
                    for (j, param2) in function.parameters.iter().enumerate() {
                        if i < j && param.0.name == param2.0.name {
                            alerts.push(SpannedAlert::error_2(
                                format!("Duplicate"),
                                format!("Duplicate parameter name: '{}'", param2.0.name),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(param2.0.span.start, param2.0.span.end),
                                ),
                                format!("first defined here"),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(param.0.span.start, param.0.span.end),
                                ),
                            ));
                        }
                    }

                    check_type(&mut alerts, param.1.clone());
                }
            }
            _ => {}
        }
    }

    alerts
}
