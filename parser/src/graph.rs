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
    pub methods: Vec<(String, SymbolFunction)>,
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

impl SymbolDefinition {
    pub fn get_name(&self) -> &str {
        match self {
            SymbolDefinition::Constant(_) => "constant",
            SymbolDefinition::Function(_) => "function",
            SymbolDefinition::Type(_) => "type",
        }
    }
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
            ($name:expr, $methods:expr) => {
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        name: $name.to_string(),
                        definition: SymbolDefinition::Type(SymbolType {
                            fields: Vec::new(),
                            methods: $methods,
                        }),
                        file: "primitives".to_string(),
                        span: Span { start: 0, end: 0 },
                        root: ast::Root::Error,
                    },
                )
            };
        }

        macro_rules! add_scalar {
            ($name:expr) => {
                let base_scalar_methods = |type_name: String| {
                    vec![
                        (
                            "_operator_plus".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                            },
                        ),
                        (
                            "_operator_minus".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                            },
                        ),
                        (
                            "_operator_divide".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                            },
                        ),
                        (
                            "_operator_multiply".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                            },
                        ),
                        (
                            "_operator_modulo".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                            },
                        ),
                        (
                            "_operator_equals".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some("bool".to_string()),
                            },
                        ),
                        (
                            "_operator_not_equals".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some("bool".to_string()),
                            },
                        ),
                        (
                            "_operator_less_than".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some("bool".to_string()),
                            },
                        ),
                        (
                            "_operator_greater_than".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some("bool".to_string()),
                            },
                        ),
                        (
                            "_operator_less_than_or_equals".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some("bool".to_string()),
                            },
                        ),
                        (
                            "_operator_greater_than_or_equals".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some("bool".to_string()),
                            },
                        ),
                    ]
                };
                let mutli_scalar_methods = |type_name: String, single_name: String| {
                    vec![(
                        "_operator_cross".to_string(),
                        SymbolFunction {
                            parameters: vec![("other".to_owned(), type_name.clone(), false)],
                            return_type: Some(single_name.clone()),
                        },
                    )]
                };
                add_primitive!(
                    $name,
                    [
                        vec![(
                            "_operator_multiply".to_string(),
                            SymbolFunction {
                                parameters: vec![("other".to_owned(), $name.to_string(), false)],
                                return_type: Some($name.to_string()),
                            },
                        )],
                        base_scalar_methods($name.to_string())
                    ]
                    .concat()
                );
                add_primitive!(
                    format!("{}{}", $name, "2"),
                    [
                        mutli_scalar_methods(format!("{}{}", $name, "2"), $name.to_string()),
                        base_scalar_methods(format!("{}{}", $name, "2"))
                    ]
                    .concat()
                );
                add_primitive!(
                    format!("{}{}", $name, "3"),
                    [
                        mutli_scalar_methods(format!("{}{}", $name, "3"), $name.to_string()),
                        base_scalar_methods(format!("{}{}", $name, "3"))
                    ]
                    .concat()
                );
                add_primitive!(
                    format!("{}{}", $name, "4"),
                    [
                        mutli_scalar_methods(format!("{}{}", $name, "4"), $name.to_string()),
                        base_scalar_methods(format!("{}{}", $name, "4"))
                    ]
                    .concat()
                );
                add_primitive!(format!("{}{}", $name, "3x3"), vec![]);
                add_primitive!(format!("{}{}", $name, "4x4"), vec![]);
            };
        }

        add_scalar!("int");
        add_scalar!("half");
        add_scalar!("uint");
        add_scalar!("short");
        add_scalar!("float");
        add_scalar!("double");

        add_primitive!("bool", Vec::new());
        add_primitive!("string", Vec::new());
        add_primitive!("byte", Vec::new());
        add_primitive!("array", Vec::new());
        add_primitive!("map", Vec::new());
        add_primitive!("function", Vec::new());
        add_primitive!("texture2d", Vec::new());
        add_primitive!("texture3d", Vec::new());
        add_primitive!("void", Vec::new());
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
                                    methods: Vec::new(),
                                    fields: struct_
                                        .fields
                                        .iter()
                                        .map(|field| (field.0.name.clone(), field.1.name.clone()))
                                        .collect(),
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
                ast::Root::Impl(_impl) => {
                    let mut this_file = self.files.get_mut(file_name).unwrap();
                    let name = _impl.name.name.clone();
                    if self.primitive.contains_key(name.as_str()) {
                        add_alert(SpannedAlert::error(
                            format!("You cannot extend primitives"),
                            format!("Attempting to extend '{}'", name),
                            Location::new(
                                file_name.to_owned(),
                                USizeTuple(_impl.name.span.start, _impl.name.span.end),
                            ),
                        ));
                    } else if !this_file.contains_key(name.as_str()) {
                        add_alert(SpannedAlert::error(
                            format!("Implementation error"),
                            format!("Type '{}' not found", name),
                            Location::new(
                                file_name.to_owned(),
                                USizeTuple(_impl.name.span.start, _impl.name.span.end),
                            ),
                        ));
                    } else {
                        let symb = this_file.get_mut(name.as_str()).unwrap();
                        if let SymbolDefinition::Type(symbol_type) = &mut symb.definition {
                            symbol_type
                                .methods
                                .extend(_impl.body.clone().iter().map(|method| {
                                    if let ast::Root::Function(ref function) = method {
                                        (
                                            function.name.name.clone(),
                                            SymbolFunction {
                                                parameters: function
                                                    .parameters
                                                    .clone()
                                                    .iter()
                                                    .map(|param| {
                                                        (
                                                            param.0.name.clone(),
                                                            param.1.name.clone(),
                                                            param.2.is_none(),
                                                        )
                                                    })
                                                    .collect(),
                                                return_type: match function.return_type {
                                                    Some(ref return_type) => {
                                                        Some(return_type.name.clone())
                                                    }
                                                    None => None,
                                                },
                                            },
                                        )
                                    } else {
                                        unreachable!()
                                    }
                                }));
                        } else {
                            add_alert(SpannedAlert::error(
                                format!("You cannot extend {}s", symb.definition.get_name()),
                                format!("Attempting to extend '{}'", name,),
                                Location::new(
                                    file_name.to_owned(),
                                    USizeTuple(_impl.name.span.start, _impl.name.span.end),
                                ),
                            ));
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
