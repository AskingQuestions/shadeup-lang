#[allow(unused_imports)]
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::ast::{self, Location, Span, USizeTuple};
use crate::printer::SpannedAlert;
use crate::validator::{TypedTag, TypedTagType};
use multimap::MultiMap;
use std::collections::HashMap;

type SymbolRef = String;

#[derive(Clone)]
pub struct SymbolFunction {
    pub parameters: Vec<(String, SymbolRef, bool)>,
    pub return_type: Option<SymbolRef>,
    pub javascript: Option<String>,
    pub webgl: Option<String>,
    pub tags: Vec<TypedTag>,
    pub shader_globals: Vec<String>,
    pub span: Span,
    pub method_of: Option<String>,
    pub gen_overload: bool,
}

impl Default for SymbolFunction {
    fn default() -> Self {
        SymbolFunction {
            parameters: Vec::new(),
            return_type: None,
            javascript: None,
            webgl: None,
            tags: Vec::new(),
            shader_globals: Vec::new(),
            span: 0..0,
            method_of: None,
            gen_overload: true,
        }
    }
}

impl SymbolFunction {
    pub fn get_overload_name(&self) -> String {
        if !self.gen_overload {
            return String::new();
        }
        let mut name = String::new();

        for (_, type_name, _) in &self.parameters {
            name.push_str(&format!("_{}", type_name.clone()));
        }

        name
    }
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
            translate_path_to_safe_name(&self.file),
            self.real_name
        )
    }
}

pub fn translate_path_to_safe_name(path: &str) -> String {
    path.replace("/", "_")
        .replace(".", "__")
        .replace("-", "___")
}

pub struct SymbolGraph {
    // file_name -> symbol_name -> symbol_node
    pub files: HashMap<String, HashMap<String, SymbolNode>>,
    pub primitive: MultiMap<String, SymbolNode>,
}

pub fn translate_native_code_to_js(native: &str) -> String {
    native.replace("sqrt", "Math.sqrt").to_string()
}

pub fn translate_native_code_to_glsl(native: &str) -> String {
    native.to_string()
}

impl SymbolGraph {
    pub fn new() -> SymbolGraph {
        let mut sg = SymbolGraph {
            files: HashMap::new(),
            primitive: MultiMap::new(),
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
            "ShaderVertexOutput".to_owned(),
            SymbolNode {
                aliased: false,
                imported: false,
                name: "ShaderVertexOutput".to_string(),
                real_name: "ShaderVertexOutput".to_string(),
                definition: SymbolDefinition::Type(SymbolType {
                    fields: vec![
                        ("position".to_string(), "vec3".to_string()),
                        ("uv".to_string(), "vec2".to_string()),
                        ("uv1".to_string(), "vec2".to_string()),
                        ("uv2".to_string(), "vec2".to_string()),
                        ("uv3".to_string(), "vec2".to_string()),
                        ("uv4".to_string(), "vec2".to_string()),
                        ("uv5".to_string(), "vec2".to_string()),
                        ("uv6".to_string(), "vec2".to_string()),
                        ("uv7".to_string(), "vec2".to_string()),
                        ("color".to_string(), "vec4".to_string()),
                    ],
                    methods: Vec::new(),
                }),
                file: "primitives".to_string(),
                span: Span { start: 0, end: 0 },
                root: ast::Root::Error,
            },
        );

        self.primitive.insert(
            "ShaderContext".to_owned(),
            SymbolNode {
                aliased: false,
                imported: false,
                name: "ShaderContext".to_string(),
                real_name: "ShaderContext".to_string(),
                definition: SymbolDefinition::Type(SymbolType {
                    fields: vec![
                        ("position".to_string(), "float3".to_string()),
                        ("uv".to_string(), "float2".to_string()),
                        ("uv1".to_string(), "float2".to_string()),
                        ("uv2".to_string(), "float2".to_string()),
                        ("uv3".to_string(), "float2".to_string()),
                        ("uv4".to_string(), "float2".to_string()),
                        ("uv5".to_string(), "float2".to_string()),
                        ("uv6".to_string(), "float2".to_string()),
                        ("uv7".to_string(), "float2".to_string()),
                        ("color".to_string(), "float4".to_string()),
                        ("thread".to_string(), "float3".to_string()),
                        ("group".to_string(), "float3".to_string()),
                    ],
                    methods: Vec::new(),
                }),
                file: "primitives".to_string(),
                span: Span { start: 0, end: 0 },
                root: ast::Root::Error,
            },
        );

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
                    webgl: None,
                    tags: vec![TypedTag {
                        tag: TypedTagType::CPUOnly,
                        span: 0..0,
                        name: "print".to_string(),
                        introduced_by: None,
                        type_name: "".to_string(),
                    }],
                    method_of: None,
                    ..Default::default()
                }),
                file: "primitives".to_string(),
                span: Span { start: 0, end: 0 },
                root: ast::Root::Error,
            },
        );

        self.primitive.insert(
            "draw".to_owned(),
            SymbolNode {
                aliased: false,
                imported: false,
                name: "draw".to_string(),
                real_name: "draw".to_string(),
                definition: SymbolDefinition::Function(SymbolFunction {
                    span: 0..0,
                    parameters: vec![("shade".to_string(), "shader".to_string(), false)],
                    return_type: None,
                    javascript: Some("__shadeup_dispatch_draw(shade)".to_string()),
                    webgl: None,
                    tags: vec![
                        TypedTag {
                            tag: TypedTagType::CPUOnly,
                            span: 0..0,
                            name: "draw".to_string(),
                            introduced_by: None,
                            type_name: "".to_string(),
                        },
                        TypedTag {
                            tag: TypedTagType::Throws,
                            span: 0..0,
                            name: "draw".to_string(),
                            introduced_by: None,
                            type_name: "".to_string(),
                        },
                    ],
                    method_of: None,
                    ..Default::default()
                }),
                file: "primitives".to_string(),
                span: Span { start: 0, end: 0 },
                root: ast::Root::Error,
            },
        );

        self.primitive.insert(
            "sleep".to_owned(),
            SymbolNode {
                aliased: false,
                imported: false,
                name: "sleep".to_string(),
                real_name: "sleep".to_string(),
                definition: SymbolDefinition::Function(SymbolFunction {
                    span: 0..0,
                    parameters: vec![("value".to_string(), "float".to_string(), false)],
                    return_type: None,
                    javascript: Some(
                        "await new Promise((res, resole) => {setTimeout(res, value * 1000)})"
                            .to_string(),
                    ),
                    webgl: None,
                    tags: vec![
                        TypedTag {
                            tag: TypedTagType::CPUOnly,
                            span: 0..0,
                            name: "sleep".to_string(),
                            introduced_by: None,
                            type_name: "".to_string(),
                        },
                        TypedTag {
                            tag: TypedTagType::Async,
                            span: 0..0,
                            name: "sleep".to_string(),
                            introduced_by: None,
                            type_name: "".to_string(),
                        },
                    ],
                    method_of: None,
                    ..Default::default()
                }),
                file: "primitives".to_string(),
                span: Span { start: 0, end: 0 },
                root: ast::Root::Error,
            },
        );

        macro_rules! add_vec_func {
            ($name:expr, $_type:expr, $return_type:expr, $src1_js:expr, $src1_gl:expr, $src2_js:expr, $src2_gl:expr, $src3_js:expr, $src3_gl:expr, $src4_js:expr,$src4_gl:expr) => {
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        aliased: false,
                        imported: false,
                        name: $name.to_string(),
                        real_name: $name.to_string(),
                        definition: SymbolDefinition::Function(SymbolFunction {
                            parameters: vec![
                                ("a".to_string(), format!("{}", $_type), false),
                                ("b".to_string(), format!("{}", $_type), false),
                            ],
                            return_type: Some($return_type.to_string()),
                            javascript: Some($src1_js.to_string()),
                            webgl: Some($src1_gl.to_string()),
                            ..Default::default()
                        }),
                        file: "primitives".to_string(),
                        span: Span { start: 0, end: 0 },
                        root: ast::Root::Error,
                    },
                );
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        aliased: false,
                        imported: false,
                        name: $name.to_string(),
                        real_name: $name.to_string(),
                        definition: SymbolDefinition::Function(SymbolFunction {
                            parameters: vec![
                                ("a".to_string(), format!("{}2", $_type), false),
                                ("b".to_string(), format!("{}2", $_type), false),
                            ],
                            return_type: Some($return_type.to_string()),
                            javascript: Some($src2_gl.to_string()),
                            webgl: Some($src2_gl.to_string()),
                            ..Default::default()
                        }),
                        file: "primitives".to_string(),
                        span: Span { start: 0, end: 0 },
                        root: ast::Root::Error,
                    },
                );
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        aliased: false,
                        imported: false,
                        name: $name.to_string(),
                        real_name: $name.to_string(),
                        definition: SymbolDefinition::Function(SymbolFunction {
                            parameters: vec![
                                ("a".to_string(), format!("{}3", $_type), false),
                                ("b".to_string(), format!("{}3", $_type), false),
                            ],
                            return_type: Some($return_type.to_string()),
                            javascript: Some($src3_js.to_string()),
                            webgl: Some($src3_gl.to_string()),
                            ..Default::default()
                        }),
                        file: "primitives".to_string(),
                        span: Span { start: 0, end: 0 },
                        root: ast::Root::Error,
                    },
                );
                self.primitive.insert(
                    $name.to_owned(),
                    SymbolNode {
                        aliased: false,
                        imported: false,
                        name: $name.to_string(),
                        real_name: $name.to_string(),
                        definition: SymbolDefinition::Function(SymbolFunction {
                            parameters: vec![
                                ("a".to_string(), format!("{}4", $_type), false),
                                ("b".to_string(), format!("{}4", $_type), false),
                            ],
                            return_type: Some($return_type.to_string()),
                            javascript: Some($src4_js.to_string()),
                            webgl: Some($src4_gl.to_string()),
                            ..Default::default()
                        }),
                        file: "primitives".to_string(),
                        span: Span { start: 0, end: 0 },
                        root: ast::Root::Error,
                    },
                );
            };
        }

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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
                                },
                            ),
                            (
                                "__prefix_operator_join".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "__this".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (++__this);")),
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
                                },
                            ),
                            (
                                "__prefix_operator_minus_minus".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "__this".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (--__this);")),
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
                                },
                            ),
                            (
                                "__postfix_operator_join".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "__this".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this++);")),
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
                                },
                            ),
                            (
                                "__postfix_operator_minus_minus".to_string(),
                                SymbolFunction {
                                    span: 0..0,
                                    parameters: vec![(
                                        "__this".to_owned(),
                                        type_name.clone(),
                                        false,
                                    )],
                                    return_type: Some(type_name.clone()),
                                    javascript: Some(format!("return (__this--);")),
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
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
                                    webgl: Some(format!(
                                        "return {}(other);",
                                        crate::webgl::translate_type(&type_name)
                                    )),
                                    tags: Vec::new(),
                                    method_of: None,
                                    ..Default::default()
                                },
                            ),
                        ],
                    ]
                    .concat()
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
                    let gen_op_on_fields_single = |op: &str| {
                        format!(
                            "return [{}]",
                            (0..num_fields)
                                .map(|i| format!("(__this[{}] {} other){}", i, op, mask))
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
                    let mut make_params = vec![];
                    let param_names = vec!["x", "y", "z", "w"];

                    for i in 0..num_fields {
                        make_params.push((
                            param_names[i as usize].to_string(),
                            single_name.clone(),
                            false,
                        ));
                    }
                    vec![
                        (
                            "__make_vec".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: make_params.clone(),
                                return_type: Some(type_name.clone()),
                                javascript: Some(format!(
                                    "return [{}];",
                                    param_names[0..num_fields as usize].join(", ")
                                )),
                                webgl: Some(format!(
                                    "return {}({});",
                                    crate::webgl::translate_type(&type_name),
                                    param_names[0..num_fields as usize].join(", ")
                                )),
                                tags: Vec::new(),
                                method_of: None,
                                gen_overload: false,
                                ..Default::default()
                            },
                        ),
                        (
                            "__construct".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: make_params.clone(),
                                return_type: Some(type_name.clone()),
                                javascript: Some(format!(
                                    "return [{}];",
                                    param_names[0..num_fields as usize].join(", ")
                                )),
                                webgl: Some(format!(
                                    "return {}({});",
                                    crate::webgl::translate_type(&type_name),
                                    param_names[0..num_fields as usize].join(", ")
                                )),
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__operator_plus".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), single_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields_single("+")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__operator_minus".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), single_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields_single("-")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__operator_divide".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), single_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields_single("/")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__operator_multiply".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), single_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields_single("*")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__operator_double_multiply".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), single_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields_single("**")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__operator_modulo".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![
                                    ("__this".to_owned(), type_name.clone(), false),
                                    ("other".to_owned(), single_name.clone(), false),
                                ],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_op_on_fields_single("%")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                        (
                            "__prefix_operator_minus".to_string(),
                            SymbolFunction {
                                span: 0..0,
                                parameters: vec![("__this".to_owned(), type_name.clone(), false)],
                                return_type: Some(type_name.clone()),
                                javascript: Some(gen_unary_op_on_fields("-")),
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: None,
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
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
                                webgl: Some(format!(
                                    "return {}({});",
                                    crate::webgl::translate_type(&type_name),
                                    (0..num_fields)
                                        .map(|i| format!("0"))
                                        .collect::<Vec<String>>()
                                        .join(", ")
                                )),
                                tags: Vec::new(),
                                method_of: None,
                                ..Default::default()
                            },
                        ),
                    ]
                };

                add_primitive!($name, base_scalar_methods($name.to_string()));

                let name2 = format!("{}{}", $name, "2");
                add_primitive!(
                    name2.clone(),
                    mutlti_scalar_methods(format!("{}{}", $name, "2"), $name.to_string(), 2, $mask)
                );

                let name3 = format!("{}{}", $name, "3");
                add_primitive!(
                    name3.clone(),
                    mutlti_scalar_methods(format!("{}{}", $name, "3"), $name.to_string(), 3, $mask)
                );

                let name4 = format!("{}{}", $name, "4");
                add_primitive!(
                    name4.clone(),
                    mutlti_scalar_methods(format!("{}{}", $name, "4"), $name.to_string(), 4, $mask)
                );

                add_vec_func!(
                    "dist",
                    $name.to_string(),
                    "float",
                    "return Math.abs(a - b);",
                    "return float(abs(a - b));",
                    "return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));",
                    "return dist(a, b);",
                    "return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));",
                    "return dist(a, b);",
                    "return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));",
                    "return dist(a, b);"
                );

                add_primitive!(format!("{}{}", $name, "3x3"), vec![]);
                add_primitive!(format!("{}{}", $name, "4x4"), vec![]);
            };
        }

        let gen_eq_ops = |type_name: String| {
            vec![
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
                        webgl: Some("return __this == other;".to_string()),
                        tags: Vec::new(),
                        method_of: None,
                        ..Default::default()
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
                        webgl: Some("return __this != other;".to_string()),
                        tags: Vec::new(),
                        method_of: None,
                        ..Default::default()
                    },
                ),
            ]
        };

        add_scalar!("int", " & 0xffffffff");
        add_scalar!("uint", ">>>0 & 0xffffffff");
        add_scalar!("short", " & 0xffff");

        add_scalar!("half", "");
        add_scalar!("float", "");
        add_scalar!("double", "");

        add_primitive!(
            "bool",
            [
                vec![
                    (
                        "__prefix_operator_not".to_string(),
                        SymbolFunction {
                            span: 0..0,
                            parameters: vec![("__this".to_owned(), "bool".to_string(), false),],
                            return_type: Some("bool".to_string()),
                            javascript: Some(format!("return !__this;")),
                            webgl: Some(format!("return !__this;")),
                            tags: Vec::new(),
                            method_of: None,
                            ..Default::default()
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
                            webgl: Some("return __this && other;".to_string()),
                            tags: Vec::new(),
                            method_of: None,
                            ..Default::default()
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
                            webgl: Some("return __this || other;".to_string()),
                            tags: Vec::new(),
                            method_of: None,
                            ..Default::default()
                        },
                    ),
                ],
                gen_eq_ops("bool".to_string())
            ]
            .concat()
        );
        add_primitive!(
            "string",
            [
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
                        webgl: None,
                        tags: Vec::new(),
                        method_of: None,
                        ..Default::default()
                    },
                )],
                gen_eq_ops("string".to_string())
            ]
            .concat()
        );
        add_primitive!("byte", gen_eq_ops("byte".to_string()));
        add_primitive!(
            "array",
            vec![
                (
                    "__operator_index".to_string(),
                    SymbolFunction {
                        span: 0..0,
                        parameters: vec![
                            ("__this".to_owned(), "array".to_string(), false),
                            ("index".to_owned(), "int".to_string(), false),
                        ],
                        return_type: Some("any".to_string()),
                        javascript: Some(format!("return __this[index];")),
                        webgl: Some(format!("return __this[index];")),
                        tags: Vec::new(),
                        method_of: None,
                        ..Default::default()
                    },
                ),
                (
                    "__operator_index_set".to_string(),
                    SymbolFunction {
                        span: 0..0,
                        parameters: vec![
                            ("__this".to_owned(), "array".to_string(), false),
                            ("index".to_owned(), "int".to_string(), false),
                            ("value".to_owned(), "any".to_string(), false),
                        ],
                        return_type: None,
                        javascript: Some(format!("__this[index] = value;")),
                        webgl: Some(format!("__this[index] = value;")),
                        tags: Vec::new(),
                        method_of: None,
                        ..Default::default()
                    },
                ),
                (
                    "push".to_string(),
                    SymbolFunction {
                        parameters: vec![
                            ("__this".to_owned(), "array".to_string(), false),
                            ("value".to_owned(), "!0".to_string(), false),
                        ],
                        return_type: None,
                        javascript: Some(format!("__this.push(value);")),
                        webgl: Some(format!("/* !error */")),
                        ..Default::default()
                    },
                ),
                (
                    "len".to_string(),
                    SymbolFunction {
                        parameters: vec![("__this".to_owned(), "array".to_string(), false),],
                        return_type: Some("int".to_string()),
                        javascript: Some(format!("__this.length")),
                        webgl: Some(format!("/* !error */")),
                        ..Default::default()
                    },
                ),
            ]
        );
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
                                    webgl: None,
                                    tags: Vec::new(),
                                    parameters: function
                                        .parameters
                                        .iter()
                                        .map(|p| {
                                            (
                                                p.0.name.clone(),
                                                if p.1.is_some() {
                                                    p.1.as_ref().unwrap().name.clone()
                                                } else {
                                                    "void".to_string()
                                                },
                                                p.2.is_some(),
                                            )
                                        })
                                        .collect(),
                                    return_type: if function.return_type.is_none() {
                                        None
                                    } else {
                                        Some(function.return_type.clone().unwrap().name.clone())
                                    },
                                    method_of: None,
                                    ..Default::default()
                                }),
                                name: function.name.name.clone(),
                                real_name: function.name.name.clone(),
                                span: function.name.span.clone(),
                            },
                        );
                    }
                }

                _ => {
                    // These are sent into the __init_file function
                    let name = "__init_file".to_string();
                    if !hmap.contains_key(name.as_str()) {
                        hmap.insert(
                            name.clone(),
                            SymbolNode {
                                aliased: false,
                                imported: false,
                                file: file_name.to_owned(),
                                root: ast::Root::Function(ast::Function {
                                    name: ast::Identifier {
                                        name: name.clone(),
                                        span: 0..0,
                                    },
                                    parameters: vec![],
                                    return_type: None,
                                    body: ast::Block {
                                        roots: vec![],
                                        span: 0..0,
                                    },
                                    span: 0..0,
                                }),
                                definition: SymbolDefinition::Function(SymbolFunction {
                                    span: 0..0,
                                    javascript: None,
                                    webgl: None,
                                    tags: Vec::new(),
                                    parameters: vec![],
                                    return_type: None,
                                    method_of: None,
                                    ..Default::default()
                                }),
                                name: name.clone(),
                                real_name: name.clone(),
                                span: 0..0,
                            },
                        );
                    }
                    let func_symbol = hmap.get_mut(name.as_str()).unwrap();

                    match func_symbol.root {
                        ast::Root::Function(ref mut func) => {
                            func.body.roots.push(root.clone());
                        }
                        _ => unreachable!(),
                    }
                }
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

                        if let Some(init_symbol) = file.get("__init_file") {
                            let mut symbol_clone = init_symbol.clone();
                            symbol_clone.name = "__init_file".to_string();
                            symbol_clone.imported = true;
                            symbol_clone.span = 0..0;
                            hmap.insert(
                                format!("{}__init_file", translate_path_to_safe_name(&path)),
                                symbol_clone,
                            );
                        }

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
                        for method in &_impl.body {
                            if let ast::Root::Function(ref function) = method {
                                let func_name = format!("{}_method_{}", name, function.name.name);
                                hmap.insert(
                                    func_name.clone(),
                                    SymbolNode {
                                        aliased: false,
                                        imported: false,
                                        file: file_name.to_owned(),
                                        root: ast::Root::Function(function.clone()),
                                        definition: SymbolDefinition::Function(SymbolFunction {
                                            span: function.name.span.clone(),
                                            javascript: None,
                                            webgl: None,
                                            tags: Vec::new(),
                                            parameters: function
                                                .parameters
                                                .iter()
                                                .map(|p| {
                                                    (
                                                        p.0.name.clone(),
                                                        if p.1.is_some() {
                                                            p.1.as_ref().unwrap().name.clone()
                                                        } else {
                                                            "void".to_string()
                                                        },
                                                        p.2.is_some(),
                                                    )
                                                })
                                                .collect(),
                                            return_type: if function.return_type.is_none() {
                                                None
                                            } else {
                                                Some(
                                                    function
                                                        .return_type
                                                        .clone()
                                                        .unwrap()
                                                        .name
                                                        .clone(),
                                                )
                                            },
                                            method_of: Some(name.clone()),
                                            ..Default::default()
                                        }),
                                        name: func_name.clone(),
                                        real_name: func_name.clone(),
                                        span: function.name.span.clone(),
                                    },
                                );
                            }
                        }
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
                                                webgl: None,
                                                tags: Vec::new(),
                                                method_of: None,
                                                parameters: function
                                                    .parameters
                                                    .clone()
                                                    .iter()
                                                    .map(|param| {
                                                        (
                                                            param.0.name.clone(),
                                                            if param.1.is_some() {
                                                                param
                                                                    .1
                                                                    .as_ref()
                                                                    .unwrap()
                                                                    .name
                                                                    .clone()
                                                            } else {
                                                                "void".to_string()
                                                            },
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
                                                ..Default::default()
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
