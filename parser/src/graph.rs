#[allow(unused_imports)]
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::ast::{self, Location, Span, USizeTuple};
use crate::printer::SpannedAlert;
use crate::validator::{TypedTag, TypedTagType};
use std::collections::HashMap;

type SymbolRef = String;

#[derive(Clone)]
pub struct SymbolFunction {
    pub parameters: Vec<(String, SymbolRef, bool)>,
    pub return_type: Option<SymbolRef>,
    pub javascript: Option<String>,
    pub tags: Vec<TypedTag>,
    pub span: Span,
}

#[derive(Clone)]
pub struct SymbolType {
    pub fields: Vec<(String, SymbolRef)>,
    pub methods: Vec<(String, SymbolFunction)>,
}

#[derive(Clone)]
pub struct SymbolConstant {
    pub value_type: SymbolRef,
}

#[allow(dead_code)]
#[derive(Clone)]
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

#[derive(Clone)]
pub struct SymbolNode {
    pub imported: bool,
    pub aliased: bool,
    pub name: String,
    pub real_name: String,
    pub definition: SymbolDefinition,
    pub root: ast::Root,
    pub file: String,
    pub span: Span,
}

impl SymbolNode {
    pub fn get_namespaced(&self) -> String {
        format!(
            "{}_{}",
            self.file
                .replace("/", "_")
                .replace(".", "__")
                .replace("-", "___"),
            self.real_name
        )
    }
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

    pub fn get_symbol_node_in_file(
        &self,
        file_name: &str,
        symbol_name: &str,
    ) -> Option<&SymbolNode> {
        if let Some(node) = self
            .files
            .get(file_name)
            .and_then(|file| file.get(symbol_name))
        {
            Some(node)
        } else if let Some(node) = self.primitive.get(symbol_name) {
            Some(node)
        } else {
            None
        }
    }

    pub fn add_primitive_symbol(&mut self, symbol: SymbolNode) {
        self.primitive.insert(symbol.name.clone(), symbol);
    }

    pub fn add_primitive_symbols(&mut self) {
        macro_rules! add_primitive {
            ($name:expr, $methods:expr) => {
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        aliased: false,
                        imported: false,
                        name: $name.to_string(),
                        real_name: $name.to_string(),
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

        self.primitive.insert(
            "print".to_owned(),
            SymbolNode {
                aliased: false,
                imported: false,
                name: "print".to_string(),
                real_name: "print".to_string(),
                definition: SymbolDefinition::Function(SymbolFunction {
                    span: 0..0,
                    parameters: vec![("value".to_string(), "string".to_string(), false)],
                    return_type: None,
                    javascript: Some("console.log(value)".to_string()),
                    tags: vec![TypedTag {
                        tag: TypedTagType::CPUOnly,
                        span: 0..0,
                        name: "print".to_string(),
                        introduced_by: None,
                    }],
                }),
                file: "primitives".to_string(),
                span: Span { start: 0, end: 0 },
                root: ast::Root::Error,
            },
        );

        macro_rules! add_scalar {
            ($name:expr, $mask:expr) => {
                let base_integer_methods = |type_name: String| {
                    if type_name == "uint" || type_name == "int" || type_name == "short" {
                        vec![
                            (
                                "__operator_bar".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this | other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_and".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this & other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_double_right".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this >> other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_double_left".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this << other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_hat".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this ^ other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__prefix_operator_tilda".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "__this".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (~__this){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                        ]
                    } else {
                        vec![]
                    }
                };
                let base_scalar_methods = |type_name: String| {
                    [
                        base_integer_methods(type_name.clone()),
                        vec![
                            (
                                "__operator_plus".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this + other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_minus".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this - other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_divide".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this / other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_multiply".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this * other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_double_multiply".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this ** other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_modulo".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this % other){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_equals".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: Some("return __this === other;".to_string()),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_not_equals".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: Some("return __this !== other;".to_string()),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_less_than".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: Some("return __this < other;".to_string()),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_greater_than".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: Some("return __this > other;".to_string()),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_less_than_or_equals".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: Some("return __this <= other;".to_string()),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__operator_greater_than_or_equals".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: Some("return __this >= other;".to_string()),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__prefix_operator_minus".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "__this".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (-__this){};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__is_scalar".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![
                                        ("__this".to_owned(), type_name.clone(), false),
                                        ("other".to_owned(), type_name.clone(), false),
                                    ],
                                    return_type: Some("bool".to_string()),
                                    javascript: None,
                                    tags: Vec::new(),
                                },
                            ),
                            (
                                "__cast_from_scalar".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "other".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return other{};", $mask)),
                                    tags: Vec::new(),
                                },
                            ),
                        ],
                    ]
                    .concat();
                };
                let mutlti_scalar_methods = |type_name: String,
                                             single_name: String,
                                             num_fields: i32,
                                             mask: &str| {
                    let gen_op_on_fields = |op: &str| {
                        format!(
                            "return [{}]",
                            (0..num_fields)
                                .map(|i| format!("(__this[{}] {} other[{}]){}", i, op, i, mask))
                                .collect::<Vec<String>>()
                                .join(", ")
                        )
                    };
                    let gen_unary_op_on_fields = |op: &str| {
                        format!(
                            "return [{}]",
                            (0..num_fields)
                                .map(|i| format!("({}__this[{}]){}", op, i, mask))
                                .collect::<Vec<String>>()
                                .join(", ")
                        )
                    };
                    vec![
                        (
                            "__operator_cross".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(single_name.clone()),
                                javascript: Some(gen_op_on_fields("*")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__operator_plus".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields("+")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__operator_minus".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields("-")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__operator_divide".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields("/")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__operator_multiply".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields("*")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__operator_double_multiply".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields("**")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__operator_modulo".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields("%")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__prefix_operator_minus".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![("__this".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_unary_op_on_fields("-")),
                                tags: Vec::new(),
                            },
                        ),
                        (
                            format!("__is_vec_{}", num_fields),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), type_name.clone(), false),
                                ],
                                return_type: Some("bool".to_string()),
                                javascript: None,
                                tags: Vec::new(),
                            },
                        ),
                        (
                            "__cast_from_vec".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![("other".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                                javascript: Some(format!(
                                    "return [{}];",
                                    (0..num_fields)
                                        .map(|i| format!("(other[{}]{})", i, $mask))
                                        .collect::<Vec<String>>()
                                        .join(", ")
                                )),
                                tags: Vec::new(),
                            },
                        ),
                    ]
                };
                add_primitive!($name, base_scalar_methods($name.to_string()));
                add_primitive!(
                    format!("{}{}", $name, "2"),
                    mutlti_scalar_methods(format!("{}{}", $name, "2"), $name.to_string(), 2, $mask)
                );
                add_primitive!(
                    format!("{}{}", $name, "3"),
                    mutlti_scalar_methods(format!("{}{}", $name, "3"), $name.to_string(), 3, $mask)
                );
                add_primitive!(
                    format!("{}{}", $name, "4"),
                    mutlti_scalar_methods(format!("{}{}", $name, "4"), $name.to_string(), 4, $mask)
                );
                add_primitive!(format!("{}{}", $name, "3x3"), vec![]);
                add_primitive!(format!("{}{}", $name, "4x4"), vec![]);
            };
        }

        add_scalar!("int", " & 0xffffffff");
        add_scalar!("uint", ">>>0 & 0xffffffff");
        add_scalar!("short", " & 0xffff");

        add_scalar!("half", "");
        add_scalar!("float", "");
        add_scalar!("double", "");

        add_primitive!(
            "bool",
            vec![
                (
                    "__prefix_operator_not".to_string(),
                    SymbolFunction {
                        span: 0..0,
                        parameters: vec![("__this".to_owned(), "bool".to_string(), false),],
                        return_type: Some("bool".to_string()),
                        javascript: Some(format!("return !__this;")),
                        tags: Vec::new(),
                    },
                ),
                (
                    "__operator_and_and".to_string(),
                    SymbolFunction {
                        span: 0..0,
                        parameters: vec![
                            ("__this".to_owned(), "bool".to_string(), false),
                            ("other".to_owned(), "bool".to_string(), false),
                        ],
                        return_type: Some("bool".to_string()),
                        javascript: Some("return __this && other;".to_string()),
                        tags: Vec::new(),
                    },
                ),
                (
                    "__operator_bar_bar".to_string(),
                    SymbolFunction {
                        span: 0..0,
                        parameters: vec![
                            ("__this".to_owned(), "bool".to_string(), false),
                            ("other".to_owned(), "bool".to_string(), false),
                        ],
                        return_type: Some("bool".to_string()),
                        javascript: Some("return __this || other;".to_string()),
                        tags: Vec::new(),
                    },
                ),
            ]
        );
        add_primitive!(
            "string",
            vec![(
                "__operator_plus".to_string(),
                SymbolFunction {
                    span: 0..0,
                    parameters: vec![
                        ("__this".to_owned(), "string".to_string(), false),
                        ("other".to_owned(), "string".to_string(), false),
                    ],
                    return_type: Some("string".to_string()),
                    javascript: Some(format!("return __this + other;")),
                    tags: Vec::new(),
                },
            )]
        );
        add_primitive!("byte", Vec::new());
        add_primitive!("array", Vec::new());
        add_primitive!("map", Vec::new());
        add_primitive!("function", Vec::new());
        add_primitive!("texture2d", Vec::new());
        add_primitive!("texture3d", Vec::new());
        add_primitive!("shader", Vec::new());
        add_primitive!("void", Vec::new());
        add_primitive!("any", Vec::new());
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
                ast::Root::Import(_import) => {
                    // Handled in second pass
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
                                aliased: false,
                                imported: false,
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
                                real_name: struct_.name.name.clone(),
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
                                aliased: false,
                                imported: false,
                                file: file_name.to_owned(),
                                root: ast::Root::Function(function.clone()),
                                definition: SymbolDefinition::Function(SymbolFunction {
                                    span: function.name.span.clone(),
                                    javascript: None,
                                    tags: Vec::new(),
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
                                real_name: function.name.name.clone(),
                                span: function.name.span.clone(),
                            },
                        );
                    }
                }

                _ => {}
            }
        }

        let file = self
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
                        let file = self.files.get(path.as_str()).unwrap();

                        for import_symbol in names {
                            let name = import_symbol.name.name.clone();
                            let alias = match import_symbol.alias {
                                Some(ref alias) => alias.name.clone(),
                                None => name.clone(),
                            };

                            if self.primitive.contains_key(&alias) {
                                let alert = SpannedAlert::error(
                                    format!("Hiding primitive"),
                                    format!("Illegal alias to hide primitive {}", name),
                                    Location::new(
                                        file_name.to_owned(),
                                        USizeTuple(
                                            import_symbol.span.start,
                                            import_symbol.span.end,
                                        ),
                                    ),
                                );

                                add_alert(alert);
                            } else if !file.contains_key(name.as_str()) {
                                let alert = SpannedAlert::error(
                                    format!("Import error"),
                                    format!("Symbol '{}' not found in '{}'. Is it marked as public? (e.g. pub fn MyFunc())", name, path),
                                    Location::new(
                                        file_name.to_owned(),
                                        USizeTuple(import_symbol.span.start, import_symbol.span.end),
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
                                            USizeTuple(import_symbol.span.start, import_symbol.span.end),
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
                                    let mut symbol_clone = symbol.clone();
                                    symbol_clone.name = alias.clone();
                                    symbol_clone.imported = true;
                                    symbol_clone.span = import_symbol.span.clone();
                                    if alias != symbol.name {
                                        symbol_clone.aliased = true;
                                    }
                                    hmap.insert(alias.clone(), symbol_clone);
                                }
                            }
                        }
                    }
                }
                ast::Root::Impl(_impl) => {
                    let this_file = self.files.get_mut(file_name).unwrap();
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
                                                span: function.name.span.clone(),
                                                javascript: None,
                                                tags: Vec::new(),
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

        let file = self
            .files
            .entry(file_name.to_string())
            .or_insert(HashMap::new());

        file.extend(hmap);

        alerts
    }
}
