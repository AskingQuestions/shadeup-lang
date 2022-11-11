#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::ast::{self, Expression, Location, Span, USizeTuple};
use crate::printer::SpannedAlert;
use std::collections::HashMap;

type SymbolRef = String;

#[derive(Debug, Clone)]
pub struct SymbolFunction {
    pub parameters: Vec<(String, SymbolRef, bool)>,
    pub return_type: Option<SymbolRef>,
}

#[derive(Debug, Clone)]
pub struct SymbolType {
    pub fields: Vec<(String, SymbolRef)>,
}

#[derive(Debug, Clone)]
pub struct SymbolConstant {
    pub value_type: SymbolRef,
}

#[derive(Debug, Clone)]
pub enum SymbolDefinition {
    Constant(SymbolConstant),
    Function(SymbolFunction),
    Type(SymbolType),
}

#[derive(Debug, Clone)]
pub struct SymbolNode {
    pub name: String,
    pub definition: SymbolDefinition,
    pub root: ast::Root,
    pub file: String,
    pub span: Span,
}

pub struct SymbolGraph {
    // file_name -> symbol_name -> symbol_node
    pub files: HashMap<String, HashMap<String, SymbolNode>>,
    pub primitive: HashMap<String, SymbolNode>,
}

impl SymbolGraph {
    pub fn new() -> SymbolGraph {
        let mut sg = SymbolGraph {
            files: HashMap::new(),
            primitive: HashMap::new(),
        };

        sg.add_primitive_symbols();
        sg
    }

    pub fn add_primitive_symbols(&mut self) {
        macro_rules! add_primitive {
            ($name:expr) => {
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        name: $name.to_string(),
                        definition: SymbolDefinition::Type(SymbolType { fields: Vec::new() }),
                        file: "primitives".to_string(),
                        span: Span { start: 0, end: 0 },
                        root: ast::Root::Error,
                    },
                )
            };
        }

        macro_rules! add_scalar {
            ($name:expr) => {
                add_primitive!($name);
                add_primitive!(format!("{}{}", $name, "2"));
                add_primitive!(format!("{}{}", $name, "3"));
                add_primitive!(format!("{}{}", $name, "4"));
                add_primitive!(format!("{}{}", $name, "3x3"));
                add_primitive!(format!("{}{}", $name, "4x4"));
            };
        }

        add_scalar!("int");
        add_scalar!("half");
        add_scalar!("uint");
        add_scalar!("short");
        add_scalar!("float");
        add_scalar!("double");

        add_primitive!("bool");
        add_primitive!("string");
        add_primitive!("byte");
        add_primitive!("array");
        add_primitive!("map");
        add_primitive!("void");
    }

    pub fn update_file_first_pass(
        &mut self,
        file_name: &str,
        roots: &Vec<ast::Root>,
    ) -> Vec<SpannedAlert> {
        let mut hmap = HashMap::<String, SymbolNode>::new();

        let mut alerts = vec![];

        let mut add_alert = |alert: SpannedAlert| {
            alerts.push(alert);
        };

        macro_rules! alert_already_defined {
            ($location_:expr, $name_:expr, $other_:expr) => {
                let location: Span = $location_;
                let name: &str = $name_;
                let other: Span = $other_;
                let loc = Location::new(
                    file_name.to_owned(),
                    USizeTuple(location.start, location.end),
                );

                let alert = SpannedAlert::error_2(
                    format!("Redefinition of symbol"),
                    format!("Symbol '{}' is already defined", name),
                    loc,
                    format!("here"),
                    Location::new(file_name.to_owned(), USizeTuple(other.start, other.end)),
                );

                add_alert(alert);
            };
        }

        macro_rules! alert_hiding_primitive {
            ($location_:expr, $name_:expr) => {
                let location: Span = $location_;
                let name: &str = $name_;
                let loc = Location::new(
                    file_name.to_owned(),
                    USizeTuple(location.start, location.end),
                );

                let alert = SpannedAlert::error(
                    format!("Hiding primitive"),
                    format!("Illegal redefinition of primitive {}", name),
                    loc,
                );

                add_alert(alert);
            };
        }

        for root in roots {
            match root {
                ast::Root::Import(import) => {
                    let names = &import.name;
                    let path = import.path.value.clone();

                    if !self.files.contains_key(&path) {
                        let alert = SpannedAlert::error(
                            format!("Import error"),
                            format!(
                                "Attempting to import from an unknown file, module, or package: '{}'",
                                path
                            ),
                            Location::new(
                                file_name.to_owned(),
                                USizeTuple(import.path.span.start, import.path.span.end),
                            ),
                        );

                        add_alert(alert);
                    } else {
                        // Pass, this is handled in the second pass
                    }
                }
                ast::Root::Struct(struct_) => {
                    let name = struct_.name.name.clone();
                    if hmap.contains_key(name.as_str()) {
                        alert_already_defined!(
                            struct_.name.span.clone(),
                            name.as_str(),
                            hmap.get(name.as_str()).unwrap().span.clone()
                        );
                    } else if self.primitive.contains_key(name.as_str()) {
                        alert_hiding_primitive!(struct_.name.span.clone(), name.as_str());
                    } else {
                        hmap.insert(
                            name,
                            SymbolNode {
                                file: file_name.to_owned(),
                                root: ast::Root::Struct(struct_.clone()),
                                definition: SymbolDefinition::Type(SymbolType {
                                    fields: Vec::new(),
                                }),
                                name: struct_.name.name.clone(),
                                span: struct_.name.span.clone(),
                            },
                        );
                    }
                }
                ast::Root::Function(ref function) => {
                    let name = function.name.name.clone();
                    if hmap.contains_key(name.as_str()) {
                        alert_already_defined!(
                            function.name.span.clone(),
                            name.as_str(),
                            hmap.get(name.as_str()).unwrap().span.clone()
                        );
                    } else if self.primitive.contains_key(name.as_str()) {
                        alert_hiding_primitive!(function.name.span.clone(), name.as_str());
                    } else {
                        hmap.insert(
                            name,
                            SymbolNode {
                                file: file_name.to_owned(),
                                root: ast::Root::Function(function.clone()),
                                definition: SymbolDefinition::Function(SymbolFunction {
                                    parameters: function
                                        .parameters
                                        .iter()
                                        .map(|p| {
                                            (p.0.name.clone(), p.1.name.clone(), p.2.is_some())
                                        })
                                        .collect(),
                                    return_type: if function.return_type.is_none() {
                                        None
                                    } else {
                                        Some(function.return_type.clone().unwrap().name.clone())
                                    },
                                }),
                                name: function.name.name.clone(),
                                span: function.name.span.clone(),
                            },
                        );
                    }
                }

                _ => {}
            }
        }

        let mut file = self
            .files
            .entry(file_name.to_string())
            .or_insert(HashMap::new());

        file.clear();

        file.extend(hmap);

        alerts
    }

    pub fn update_file_second_pass(
        &mut self,
        file_name: &str,
        roots: &Vec<ast::Root>,
    ) -> Vec<SpannedAlert> {
        let mut hmap = HashMap::<String, SymbolNode>::new();

        let mut alerts = vec![];

        let mut add_alert = |alert: SpannedAlert| {
            alerts.push(alert);
        };

        macro_rules! alert_already_defined {
            ($location_:expr, $name_:expr, $other_:expr) => {
                let location: Span = $location_;
                let name: &str = $name_;
                let other: Span = $other_;
                let loc = Location::new(
                    file_name.to_owned(),
                    USizeTuple(location.start, location.end),
                );

                let alert = SpannedAlert::error_2(
                    format!("Redefinition of symbol"),
                    format!("Symbol '{}' is already defined", name),
                    loc,
                    format!("here"),
                    Location::new(file_name.to_owned(), USizeTuple(other.start, other.end)),
                );

                add_alert(alert);
            };
        }

        for root in roots {
            match root {
                ast::Root::Import(import) => {
                    let names = &import.name;
                    let path = import.path.value.clone();

                    if !self.files.contains_key(&path) {
                        // Pass, we already reported this in the first pass
                    } else {
                        let file = self.files.get(path.as_str()).unwrap();

                        for symbol in names {
                            let name = symbol.name.name.clone();
                            let alias = match symbol.alias {
                                Some(ref alias) => alias.name.clone(),
                                None => name.clone(),
                            };

                            if self.primitive.contains_key(&alias) {
                                let alert = SpannedAlert::error(
                                    format!("Hiding primitive"),
                                    format!("Illegal alias to hide primitive {}", name),
                                    Location::new(
                                        file_name.to_owned(),
                                        USizeTuple(symbol.span.start, symbol.span.end),
                                    ),
                                );

                                add_alert(alert);
                            } else if !file.contains_key(name.as_str()) {
                                let alert = SpannedAlert::error(
                                    format!("Import error"),
                                    format!("Symbol '{}' not found in '{}'. Is it marked as public? (e.g. pub fn MyFunc())", name, path),
                                    Location::new(
                                        file_name.to_owned(),
                                        USizeTuple(symbol.span.start, symbol.span.end),
                                    ),
                                );

                                add_alert(alert);
                            } else {
                                if self
                                    .files
                                    .get(file_name)
                                    .unwrap()
                                    .contains_key(alias.as_str())
                                {
                                    let other = self
                                        .files
                                        .get(file_name)
                                        .unwrap()
                                        .get(alias.as_str())
                                        .unwrap()
                                        .span
                                        .clone();
                                    let alert = SpannedAlert::error_2(
                                        format!("Import error"),
                                        format!("Symbol '{}' is already defined in this file, consider using an alias. ( e.g. import {} as _{} from '{}'; )", alias, name, alias, path),
                                        Location::new(
                                            file_name.to_owned(),
                                            USizeTuple(symbol.span.start, symbol.span.end),
                                        ),
                                        format!("defined in this file here"),
                                        Location::new(
                                            file_name.to_owned(),
                                            USizeTuple(other.start, other.end),
                                        ),
                                    );

                                    add_alert(alert);
                                } else {
                                    let symbol = file.get(name.as_str()).unwrap();

                                    hmap.insert(alias.clone(), symbol.clone());
                                }
                            }
                        }
                    }
                }

                _ => {}
            }
        }

        let mut file = self
            .files
            .entry(file_name.to_string())
            .or_insert(HashMap::new());

        file.extend(hmap);

        alerts
    }
}
