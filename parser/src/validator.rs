use chumsky::primitive::Container;
use serde::{Deserialize, Serialize};

use std::collections::HashMap;

use crate::ast::{self, Call, Expression, Location, Op, Span, USizeTuple};
use crate::graph::{SymbolDefinition, SymbolFunction, SymbolGraph, SymbolNode, SymbolType};
use crate::printer::SpannedAlert;

pub struct Scope<'a> {
    shader_barrier: bool,
    definitions: HashMap<String, String>,
    parent: Option<Box<&'a Scope<'a>>>,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum IntellisenseItemKind {
    Function,
    Field,
    Variable,
    Type,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IntellisenseSuggestion {
    pub value: String,
    pub label: String,
    pub kind: IntellisenseItemKind,
    pub detail: String,
    pub documentation: Option<String>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IntellisenseHint {
    pub start: usize,
    pub end: usize,
    pub label: String,
    pub kind: IntellisenseItemKind,
    pub goto_offset: usize,
    pub detail: String,
    pub suggestions: Vec<IntellisenseSuggestion>,
}

pub struct ValidationContext {
    pub alerts: Vec<SpannedAlert>,
    pub intellisense: Vec<IntellisenseHint>,
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum TypedValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
    Error,
}

#[derive(Clone)]
pub struct TypedShaderInstance {
    pub closure: HashMap<String, String>,
    pub shader: usize,
}

#[derive(Clone)]
pub struct TypedShaderDefinition {
    pub parameters: HashMap<String, String>,
    pub functions: HashMap<String, bool>,
    pub body: TypedBody,
}

#[derive(Clone)]
pub enum TypedExpression {
    Call(String, Vec<TypedExpression>, Span),
    Value(TypedValue, Span),
    Identifier(String, Span),
    KVMap(Vec<(String, TypedExpression)>, Span),
    Shader(TypedShaderInstance, Span),
    Wrap(Box<TypedExpression>, Span),
    Error(),
}

impl TypedExpression {
    pub fn get_span(&self) -> Span {
        match self {
            TypedExpression::Call(_, _, span) => span.clone(),
            TypedExpression::Value(_, span) => span.clone(),
            TypedExpression::Identifier(_, span) => span.clone(),
            TypedExpression::KVMap(_, span) => span.clone(),
            TypedExpression::Shader(_, span) => span.clone(),
            TypedExpression::Wrap(_, span) => span.clone(),
            TypedExpression::Error() => 0..0,
        }
    }
}

#[derive(Clone)]
pub struct TypedFunctionParameter {
    pub name: String,
    pub type_name: ExpandedType,
    pub default_value: Option<TypedExpression>,
}

#[derive(Clone)]
pub enum TypedStatement {
    Return(TypedExpression, Span),
    If {
        condition: TypedExpression,
        body: TypedBody,
        else_ifs: Vec<(TypedExpression, TypedBody)>,
        else_body: Option<TypedBody>,
        span: Span,
    },
    Let {
        name: String,
        value: TypedExpression,
        span: Span,
    },
    Expression(TypedExpression, Span),
}

impl TypedStatement {
    #[allow(dead_code)]
    pub fn get_span(&self) -> Span {
        match self {
            TypedStatement::Return(_, span) => span.clone(),
            TypedStatement::If { span, .. } => span.clone(),
            TypedStatement::Let { span, .. } => span.clone(),
            TypedStatement::Expression(_, span) => span.clone(),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum TypedTagType {
    Async,
    DynamicAlloc,
    CPUOnly,
    ShaderOnly,
    Recursive,
}

impl ToString for TypedTagType {
    fn to_string(&self) -> String {
        match self {
            TypedTagType::Async => "async".to_string(),
            TypedTagType::DynamicAlloc => "dynamic_alloc".to_string(),
            TypedTagType::CPUOnly => "cpu_only".to_string(),
            TypedTagType::ShaderOnly => "shader_only".to_string(),
            TypedTagType::Recursive => "recursive".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct TypedTag {
    pub tag: TypedTagType,
    pub span: Span,
    pub name: String,
    pub introduced_by: Option<Box<TypedTag>>,
}

#[derive(Clone)]
pub struct TypedBody {
    pub statements: Vec<TypedStatement>,
    pub tags: Vec<TypedTag>,
}

#[derive(Clone)]
pub struct TypedFunction {
    pub name: String,
    pub parameters: Vec<TypedFunctionParameter>,
    pub return_type: ExpandedType,
    pub body: TypedBody,
    pub javascript: Option<String>,
    pub tags: Vec<TypedTag>,
    pub tagged: bool,
    pub tagging: bool,
}

#[derive(Clone)]
pub struct TypedIntermediate {
    pub functions: HashMap<String, TypedFunction>,
    pub structs: Vec<(String, Vec<(String, String)>)>,
    pub shaders: Vec<TypedShaderDefinition>,
}

fn shake_expression(
    graph: &SymbolGraph,
    scope: &Scope,
    file_name: &str,
    in_intermediate: &TypedIntermediate,
    out_intermediate: &mut TypedIntermediate,
    typed_expression: &TypedExpression,
) {
    match typed_expression {
        TypedExpression::Call(func_name, args, _) => {
            if let Some(func) = in_intermediate.functions.get(func_name) {
                if out_intermediate.functions.get(func_name).is_none() {
                    out_intermediate
                        .functions
                        .insert(func_name.clone(), func.clone());

                    shake_body(
                        graph,
                        scope,
                        file_name,
                        in_intermediate,
                        out_intermediate,
                        &func.body,
                    );
                }

                for arg in args {
                    shake_expression(
                        graph,
                        scope,
                        file_name,
                        in_intermediate,
                        out_intermediate,
                        arg,
                    );
                }
            }
        }
        TypedExpression::Wrap(w, _) => {
            shake_expression(
                graph,
                scope,
                file_name,
                in_intermediate,
                out_intermediate,
                w,
            );
        }
        TypedExpression::Value(_, _) => {}
        TypedExpression::Identifier(_ident, _) => {}
        TypedExpression::KVMap(_, _) => {}
        TypedExpression::Shader(_, _span) => {}
        TypedExpression::Error() => {}
    }
}

// Recursively descends into the Typed tree and adds references to the intermediate
fn shake_body(
    graph: &SymbolGraph,
    new_scope: &Scope,
    file_name: &str,
    in_intermediate: &TypedIntermediate,
    out_intermediate: &mut TypedIntermediate,
    typed_body: &TypedBody,
) {
    for statement in &typed_body.statements {
        match statement {
            TypedStatement::Return(expr, _span) => {
                shake_expression(
                    graph,
                    new_scope,
                    file_name,
                    in_intermediate,
                    out_intermediate,
                    expr,
                );
            }
            TypedStatement::If {
                condition,
                body,
                else_ifs,
                else_body,
                span: _,
            } => {
                shake_expression(
                    graph,
                    new_scope,
                    file_name,
                    in_intermediate,
                    out_intermediate,
                    condition,
                );
                shake_body(
                    graph,
                    new_scope,
                    file_name,
                    in_intermediate,
                    out_intermediate,
                    body,
                );
                for else_if in else_ifs {
                    shake_expression(
                        graph,
                        new_scope,
                        file_name,
                        in_intermediate,
                        out_intermediate,
                        &else_if.0,
                    );
                    shake_body(
                        graph,
                        new_scope,
                        file_name,
                        in_intermediate,
                        out_intermediate,
                        &else_if.1,
                    );
                }
                if let Some(else_body) = else_body {
                    shake_body(
                        graph,
                        new_scope,
                        file_name,
                        in_intermediate,
                        out_intermediate,
                        else_body,
                    );
                }
            }
            TypedStatement::Let {
                name: _,
                value,
                span: _,
            } => {
                shake_expression(
                    graph,
                    new_scope,
                    file_name,
                    in_intermediate,
                    out_intermediate,
                    value,
                );
            }
            TypedStatement::Expression(expr, _) => {
                shake_expression(
                    graph,
                    new_scope,
                    file_name,
                    in_intermediate,
                    out_intermediate,
                    expr,
                );
            }
        }
    }
}

impl TypedIntermediate {
    pub fn tree_shake(
        &self,
        graph: &SymbolGraph,
        file_name: &str,
        root_func: &str,
    ) -> TypedIntermediate {
        let mut out = TypedIntermediate {
            functions: HashMap::new(),
            structs: self.structs.clone(),
            shaders: self.shaders.clone(),
        };

        out.functions
            .insert(root_func.to_owned(), self.functions[root_func].clone());

        let scope = Scope::new();

        shake_body(
            graph,
            &scope,
            file_name,
            self,
            &mut out,
            &self.functions[root_func].body,
        );

        return out;
    }

    pub fn tree_shake_shader(
        &self,
        graph: &SymbolGraph,
        file_name: &str,
        shader: TypedShaderDefinition,
    ) -> TypedIntermediate {
        let mut out = TypedIntermediate {
            functions: HashMap::new(),
            structs: self.structs.clone(),
            shaders: Vec::new(),
        };

        let func = TypedFunction {
            name: "main".to_string(),
            parameters: shader
                .parameters
                .iter()
                .map(|p| TypedFunctionParameter {
                    name: p.0.clone(),
                    type_name: ExpandedType::from_string(graph, file_name, p.1),
                    default_value: None,
                })
                .collect(),
            return_type: ExpandedType::from_string(graph, file_name, "void"),
            body: shader.body,
            javascript: None,
            tags: Vec::new(),
            tagged: true,
            tagging: false,
        };

        out.functions.insert("main".to_string(), func.clone());

        let scope = Scope::new();

        shake_body(graph, &scope, file_name, self, &mut out, &func.body);

        return out;
    }
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            parent: None,
            shader_barrier: false,
        }
    }

    pub fn check(&self, name: &str) -> Option<String> {
        if let Some(definition) = self.definitions.get(name) {
            Some(definition.clone())
        } else if let Some(parent) = &self.parent {
            parent.check(name)
        } else {
            None
        }
    }

    pub fn check_with_shader_barrier(
        &self,
        name: &str,
        broke_barrier: bool,
    ) -> Option<(String, bool)> {
        if let Some(definition) = self.definitions.get(name) {
            Some((definition.clone(), broke_barrier))
        } else if let Some(parent) = &self.parent {
            let mut did_break = broke_barrier;
            if self.shader_barrier {
                did_break = true;
            }
            parent.check_with_shader_barrier(name, did_break)
        } else {
            None
        }
    }
}

fn check_type_local<'a>(
    _context: &mut ValidationContext,
    graph: &'a SymbolGraph,
    file_name: &str,
    name: &ast::Identifier,
) -> Option<&'a SymbolType> {
    let primitive = graph.primitive.get(&name.name);

    if primitive.is_some() {
        let inner_wrap =
            if let SymbolDefinition::Type(ref symbol_type) = primitive.unwrap().definition {
                symbol_type
            } else {
                unreachable!()
            };
        return Some(inner_wrap);
    }

    let file = graph.files.get(file_name).unwrap();

    let symbol = file.get(&name.name);

    if symbol.is_none() {
        _context.alerts.push(SpannedAlert::error(
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

        return None;
    }

    if symbol.is_some() {
        let symbol = symbol.unwrap();

        if let SymbolDefinition::Type(def_type) = &symbol.definition {
            return Some(def_type);
        } else {
            _context.alerts.push(SpannedAlert::error(
                "Expected a type".to_string(),
                format!(
                    "The symbol '{}' is not a type but a {}",
                    name.name,
                    symbol.definition.get_name()
                ),
                Location::new(
                    file_name.to_string(),
                    USizeTuple(name.span.start, name.span.end),
                ),
            ));

            return None;
        }
    }

    None
}

fn check_method_local<'a>(
    _context: &mut ValidationContext,
    graph: &'a SymbolGraph,
    file_name: &str,
    span: &Span,
    symbol: &SymbolType,
    _generics: &Vec<ExpandedType>,
    method_name: &str,
    args: &Vec<ExpandedType>,
) -> String {
    for method in symbol.methods.iter() {
        if method.0 == method_name {
            let mut compatible = true;
            if args.len() > method.1.parameters.len() {
                compatible = false;
                _context.alerts.push(SpannedAlert::error(
                    "Too many arguments".to_string(),
                    format!(
                        "Expected {} arguments but got {}",
                        method.1.parameters.len(),
                        args.len()
                    ),
                    Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                ));
            }

            for (i, param) in method.1.parameters.iter().enumerate() {
                if i >= args.len() {
                    if param.2 {
                        continue;
                    } else {
                        compatible = false;
                        _context.alerts.push(SpannedAlert::error(
                            "Missing parameter".to_string(),
                            format!("Parameter '{}' is required but was not provided", param.0),
                            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                        ));
                        break;
                    }
                }

                let arg = &args[i];

                if !arg.is_compatible_with(&ExpandedType::from_string(
                    graph,
                    file_name,
                    param.1.as_str(),
                )) {
                    compatible = false;
                    _context.alerts.push(SpannedAlert::error(
                        "Invalid argument type".to_string(),
                        format!("Expected type '{}' but got '{}'", param.1, arg.to_string()),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                }
            }

            if compatible {
                return method
                    .1
                    .return_type
                    .clone()
                    .unwrap_or("void".to_owned())
                    .clone();
            }
        }
    }

    if method_name.starts_with("__operator_") {
        _context.alerts.push(SpannedAlert::error(
            "Undefined operator".to_string(),
            format!(
                "No operator '{}' was found that takes a right-hand side of type '{}'",
                method_name.replace("__operator_", ""),
                args[1].name
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    } else if method_name.starts_with("__prefix_operator_") {
        _context.alerts.push(SpannedAlert::error(
            "Undefined unary operator".to_string(),
            format!(
                "No operator '{}' was found for type '{}'",
                method_name.replace("__prefix_operator_", ""),
                args[0].name
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    } else {
        _context.alerts.push(SpannedAlert::error(
            "Undefined method".to_string(),
            format!("The method '{}' was not found", method_name),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    }

    return "!error".to_string();
}

pub fn get_hint_for_function(name: &str, func: &SymbolFunction) -> IntellisenseSuggestion {
    IntellisenseSuggestion {
        value: format!(
            "{}({})",
            name,
            func.parameters
                .iter()
                .enumerate()
                .map(|(i, x)| format!("${{{}:{}}}", i + 1, x.0))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        label: name.to_string(),
        kind: IntellisenseItemKind::Function,
        detail: format!(
            "({}) -> {}",
            func.parameters
                .iter()
                .map(|x| format!("{}: {}", x.0, x.1))
                .collect::<Vec<String>>()
                .join(", "),
            func.return_type.clone().unwrap_or("void".to_owned())
        ),
        documentation: None,
    }
}

pub fn add_struct_hints(ctx: &mut ValidationContext, symbol: &SymbolType, span: &Span) {
    let mut suggestions = Vec::new();
    for prop in &symbol.fields {
        suggestions.push(IntellisenseSuggestion {
            value: prop.0.clone(),
            label: prop.0.clone(),
            kind: IntellisenseItemKind::Field,
            detail: prop.1.clone(),
            documentation: None,
        });
    }

    for func in &symbol.methods {
        suggestions.push(get_hint_for_function(&func.0, &func.1));
    }
    ctx.intellisense.push(IntellisenseHint {
        start: span.end,
        end: span.end,
        goto_offset: span.end,
        label: "dot".to_string(),
        kind: IntellisenseItemKind::Type,
        detail: ">".to_string(),
        suggestions: suggestions,
    });
}

pub fn match_method<'a>(
    _context: &mut ValidationContext,
    scope: &Scope,
    graph: &'a SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    symbol: &'a SymbolType,
    method_name: &str,
    args: &Vec<ExpandedType>,
) -> Option<&'a SymbolFunction> {
    for method in &symbol.methods {
        if method.0 == method_name {
            let mut compatible = true;
            if args.len() > method.1.parameters.len() {
                compatible = false;
            }

            for (i, param) in method.1.parameters.iter().enumerate() {
                if i >= args.len() {
                    if param.2 {
                        continue;
                    } else {
                        compatible = false;
                        break;
                    }
                }

                let arg = &args[i];
                let param_type = ExpandedType::from_string(graph, file_name, param.1.as_str());
                if !arg.is_compatible_with(&param_type) {
                    compatible = false;
                }
            }

            if compatible {
                return Some(&method.1);
            }
        }
    }

    None
}

pub fn get_call_local(
    _context: &mut ValidationContext,
    scope: &Scope,
    graph: &SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    call: &Call,
    func_symbol: Option<&SymbolNode>,
    func_type: &ExpandedType,
    typed_expr: &TypedExpression,
) -> (String, TypedExpression) {
    let mut typed_args = Vec::new();

    if func_type.name == "function" {
        let mut out_type = "!error".to_string();
        if func_type.generics.len() > 0 {
            out_type = func_type.generics[0].name.clone();
        }

        if call.args.len() > func_type.generics.len() - 1 {
            _context.alerts.push(SpannedAlert::error(
                "Too many arguments".to_string(),
                format!(
                    "Expected {} arguments but got {}",
                    func_type.generics.len() - 1,
                    call.args.len()
                ),
                Location::new(
                    file_name.to_string(),
                    USizeTuple(call.span.start, call.span.end),
                ),
            ));
        }

        for (i, param) in func_type.generics.iter().skip(1).enumerate() {
            if i >= call.args.len() {
                if param.name == "__optional" {
                    continue;
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "Too few arguments".to_string(),
                        format!(
                            "Expected {} arguments but got {}",
                            func_type.generics.len() - 1,
                            call.args.len()
                        ),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(call.span.start, call.span.end),
                        ),
                    ));
                    break;
                }
            }

            let (arg_type_string, typed_arg) = get_type_local(
                _context,
                scope,
                graph,
                file_name,
                intermediate,
                &call.args[i],
            );

            let arg_type = ExpandedType::from_string(graph, file_name, &arg_type_string);

            let real_param_type = if param.name == "__optional" {
                &param.generics[0]
            } else {
                &param
            };

            typed_args.push(arg_type.wrap_cast(real_param_type, typed_arg));

            if !arg_type.is_compatible_with(real_param_type) {
                _context.alerts.push(SpannedAlert::error(
                    "Argument type mismatch".to_string(),
                    format!(
                        "Expected type '{}' but got type '{}'",
                        real_param_type.to_string(),
                        arg_type.to_string(),
                    ),
                    Location::new(
                        file_name.to_string(),
                        USizeTuple(call.args[i].get_span().start, call.args[i].get_span().end),
                    ),
                ));
            }
        }

        let func_name = match &typed_expr {
            TypedExpression::Identifier(name, _) => name.clone(),
            _ => "!error".to_string(),
        };

        (
            out_type,
            TypedExpression::Call(func_name, typed_args, call.span.clone()),
        )
    } else {
        if call.method.is_some() {
            if func_type.symbol_type.is_some() {
                let symbol = func_type.symbol_type.as_ref().unwrap();
                let method_name = call.method.as_ref().unwrap();
                let mut args = Vec::new();

                let (self_arg_type_string, self_typed) = get_type_local(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    &call.expression,
                );

                typed_args.push(self_typed);

                for arg in &call.args {
                    let (arg_type_string, typed_arg) =
                        get_type_local(_context, scope, graph, file_name, intermediate, &arg);
                    let arg_type = ExpandedType::from_string(graph, file_name, &arg_type_string);
                    typed_args.push(arg_type.wrap_cast(&arg_type, typed_arg));
                    args.push(arg_type);
                }

                if let Some(method_symbol) = match_method(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    symbol,
                    &method_name.name,
                    &args,
                ) {
                    (
                        method_symbol
                            .return_type
                            .as_ref()
                            .unwrap_or(&"void".to_string())
                            .clone(),
                        TypedExpression::Call(
                            format!(
                                "__{}_{}",
                                func_symbol.unwrap().get_namespaced(),
                                method_name.name
                            ),
                            typed_args,
                            call.span.clone(),
                        ),
                    )
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "No method".to_string(),
                        format!(
                            "'{}' has no method named '{}'",
                            func_type.name, method_name.name
                        ),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(call.span.start, call.span.end),
                        ),
                    ));
                    ("!error".to_string(), TypedExpression::Error())
                }
            } else {
                _context.alerts.push(SpannedAlert::error(
                    "No methods".to_string(),
                    format!("'{}' has no methods", func_type.name),
                    Location::new(
                        file_name.to_string(),
                        USizeTuple(call.span.start, call.span.end),
                    ),
                ));
                ("!error".to_string(), TypedExpression::Error())
            }
        } else {
            _context.alerts.push(SpannedAlert::error(
                "Expected something callable".to_string(),
                format!("got: '{}'", func_type.name),
                Location::new(
                    file_name.to_string(),
                    USizeTuple(call.span.start, call.span.end),
                ),
            ));
            ("!error".to_string(), TypedExpression::Error())
        }
    }
}

pub fn get_type_local(
    _context: &mut ValidationContext,
    scope: &Scope,
    graph: &SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    expr: &Expression,
) -> (String, TypedExpression) {
    match expr {
        ast::Expression::Value((value, _)) => match value {
            ast::Value::Int(ival) => (
                "int".to_string(),
                TypedExpression::Value(TypedValue::Int(ival.clone()), expr.get_span()),
            ),
            ast::Value::Real(fval) => (
                "float".to_string(),
                TypedExpression::Value(TypedValue::Float(fval.clone()), expr.get_span()),
            ),
            ast::Value::Str(sval) => (
                "string".to_string(),
                TypedExpression::Value(TypedValue::String(sval.clone()), expr.get_span()),
            ),
            ast::Value::Bool(bval) => (
                "bool".to_string(),
                TypedExpression::Value(TypedValue::Bool(bval.clone()), expr.get_span()),
            ),
            ast::Value::Null => (
                "null".to_string(),
                TypedExpression::Value(TypedValue::Null, expr.get_span()),
            ),
        },
        ast::Expression::Error(_) => ("!error".to_string(), TypedExpression::Error()),
        ast::Expression::Identifier((ident, _)) => {
            if let Some(local_type) = scope.check(&ident.name) {
                add_variable_hint(_context, &ident.name, &local_type, &ident.span);

                (
                    local_type,
                    TypedExpression::Identifier(ident.name.clone(), ident.span.clone()),
                )
            } else {
                let file = graph.files.get(file_name).unwrap();
                let mut symbol = file.get(&ident.name);
                if symbol.is_none() {
                    symbol = graph.primitive.get(&ident.name);
                }
                if let Some(symbol) = symbol {
                    match &symbol.definition {
                        SymbolDefinition::Type(_) => {
                            add_type_hint(_context, symbol, &ident.span);
                            (
                                ident.name.clone(),
                                TypedExpression::Identifier(
                                    symbol.get_namespaced(),
                                    ident.span.clone(),
                                ),
                            )
                        }

                        SymbolDefinition::Function(func) => {
                            add_function_hint(_context, symbol, &ident.span);
                            (
                                format!(
                                    "function<{}{}>",
                                    func.return_type.clone().unwrap_or("void".to_owned()),
                                    func.parameters
                                        .iter()
                                        .map(|x| if x.2 {
                                            format!(",__optional<{}>", x.1.clone())
                                        } else {
                                            format!(",{}", x.1.clone())
                                        })
                                        .collect::<Vec<String>>()
                                        .join("")
                                ),
                                TypedExpression::Identifier(
                                    format!("{}", symbol.get_namespaced()),
                                    ident.span.clone(),
                                ),
                            )
                        }
                        _ => ("!error".to_string(), TypedExpression::Error()),
                    }
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "Undefined identifier".to_string(),
                        format!("'{}' was not found in this scope", ident.name),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(ident.span.start, ident.span.end),
                        ),
                    ));
                    ("!error".to_string(), TypedExpression::Error())
                }
            }
        }
        ast::Expression::Call((call, _)) => {
            let matched_expr = match *call.expression {
                ast::Expression::Error(_) => {
                    let method_unwrap = call.method.as_ref().unwrap();
                    ast::Expression::Identifier((method_unwrap.clone(), method_unwrap.span.clone()))
                }
                _ => *call.expression.clone(),
            };
            let (func_type, typed_expr) = get_type_local(
                _context,
                scope,
                graph,
                file_name,
                intermediate,
                &matched_expr,
            );
            let func_symbol = graph.get_symbol_node_in_file(file_name, &func_type);
            let func_type = ExpandedType::from_string(graph, file_name, &func_type);
            if let Some(func_symbol) = func_symbol {
                get_call_local(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    call,
                    Some(&func_symbol),
                    &func_type,
                    &typed_expr,
                )
            } else {
                get_call_local(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    call,
                    None,
                    &func_type,
                    &typed_expr,
                )
            }
        }

        ast::Expression::InlineBlock((block, _)) => {
            let output = ExpandedType::from_string(graph, file_name, "shader");

            let mut new_scope = Scope::new();
            new_scope.parent = Some(Box::new(scope));
            new_scope.shader_barrier = true;

            let typed_body = check_body_local(
                _context,
                &mut new_scope,
                &graph,
                file_name,
                intermediate,
                &block.roots,
                &output,
            );

            let mut shader_def = TypedShaderDefinition {
                parameters: HashMap::new(),
                functions: HashMap::new(),
                body: typed_body.clone(),
            };

            build_shader_definition(
                _context,
                &graph,
                &new_scope,
                &mut shader_def,
                file_name,
                intermediate,
                &typed_body,
            );

            let shader_inst = TypedShaderInstance {
                closure: shader_def.parameters.clone(),
                shader: intermediate.shaders.len(),
            };

            intermediate.shaders.push(shader_def);

            (
                "shader".to_owned(),
                TypedExpression::Shader(shader_inst, block.span.clone()),
            )
        }
        ast::Expression::StructInstance((ident, args, _)) => {
            let checked_type = check_type_local(_context, graph, file_name, ident);
            if checked_type.is_some() {
                let symbol = graph
                    .files
                    .get(file_name)
                    .unwrap()
                    .get(&ident.name)
                    .unwrap();
                (
                    ident.name.clone(),
                    TypedExpression::Call(
                        format!("__make_struct_{}", symbol.get_namespaced()),
                        vec![TypedExpression::KVMap(
                            args.iter()
                                .map(|arg| {
                                    let (expr_type, arg_typed) = get_type_local(
                                        _context,
                                        scope,
                                        graph,
                                        file_name,
                                        intermediate,
                                        &arg.1,
                                    );

                                    (
                                        arg.0.name.clone(),
                                        ExpandedType::from_string(graph, file_name, &expr_type)
                                            .wrap_cast(
                                                &ExpandedType::from_string(
                                                    graph,
                                                    file_name,
                                                    &arg.0.name,
                                                ),
                                                arg_typed,
                                            ),
                                    )
                                })
                                .collect(),
                            ident.span.clone(),
                        )],
                        ident.span.clone(),
                    ),
                )
            } else {
                ("!error".to_owned(), TypedExpression::Error())
            }
        }
        ast::Expression::Op((lhs, op, rhs, span)) => {
            let (lhs_type, lhs_typed_expr) =
                get_type_local(_context, scope, graph, file_name, intermediate, lhs);

            let lhs_type = ExpandedType::from_string(graph, file_name, &lhs_type);

            let lhs_symbol = if lhs_type.name == "!error" {
                None
            } else {
                check_type_local(
                    _context,
                    graph,
                    file_name,
                    &ast::Identifier {
                        name: lhs_type.name.clone(),
                        span: lhs.get_span(),
                    },
                )
            };

            if let Op::Dot = op {
                if let Some(lhs_symbol) = lhs_symbol {
                    if let Some(lhs_symbol_node) =
                        graph.get_symbol_node_in_file(file_name, &lhs_type.name)
                    {
                        if let ast::Expression::Identifier(rhs_ident) = rhs.as_ref() {
                            let numeric_props =
                                vec![vec!['x', 'y', 'z', 'w'], vec!['r', 'g', 'b', 'a']];
                            if lhs_type.is_scalar() {
                                let keys = rhs_ident.0.name.as_str();
                                // Single num expansion
                                for swiz in numeric_props {
                                    if swiz[0] == keys.chars().nth(0).unwrap() {
                                        for i in 1..keys.len() {
                                            if swiz[i] != keys.chars().nth(i).unwrap() {
                                                _context.alerts.push(SpannedAlert::error(
                                                    "Invalid linear swizzle".to_string(),
                                                    format!(
                                                        "expected: '{}', got: '{}'",
                                                        swiz[i],
                                                        keys.chars().nth(i).unwrap()
                                                    ),
                                                    Location::new(
                                                        file_name.to_string(),
                                                        USizeTuple(
                                                            rhs_ident.1.start,
                                                            rhs_ident.1.end,
                                                        ),
                                                    ),
                                                ));
                                                return (
                                                    "!error".to_owned(),
                                                    TypedExpression::Error(),
                                                );
                                            }
                                        }

                                        return (
                                            format!("{}{}", lhs_type.name, keys.len()),
                                            gen_up_swizzle(
                                                _context,
                                                scope,
                                                graph,
                                                file_name,
                                                intermediate,
                                                &lhs_typed_expr,
                                                &lhs_type.name,
                                                keys.len(),
                                            ),
                                        );
                                    }
                                }

                                _context.alerts.push(SpannedAlert::error(
                                    "Scalar types can only be swizzled linearly. e.g. allowed: 1.xyz, disallowed: 1.zx".to_string(),
                                    format!("got: '{}'", keys),
                                    Location::new(
                                        file_name.to_string(),
                                        USizeTuple(rhs_ident.1.start, rhs_ident.1.end),
                                    ),
                                ));
                                return ("!error".to_owned(), TypedExpression::Error());
                            } else if lhs_type.is_any_vec() {
                                let keys = rhs_ident.0.name.as_str();
                                // Vec swizzle
                                // (3, 4).xxyy -> __swiz_2_1_1_2_2
                                for swiz in numeric_props {
                                    if swiz.contains(&keys.chars().nth(0).unwrap()) {
                                        let mut nums = vec![];
                                        for i in 0..keys.len() {
                                            if !swiz.contains(&keys.chars().nth(i).unwrap()) {
                                                _context.alerts.push(SpannedAlert::error(
                                                    "Illegal mixing of swizzles (.e.g xyb)"
                                                        .to_string(),
                                                    format!(
                                                        "expected: {}, got: {}",
                                                        swiz.iter()
                                                            .map(|c| c.to_string())
                                                            .collect::<Vec<String>>()
                                                            .join(" or "),
                                                        keys.chars().nth(i).unwrap()
                                                    ),
                                                    Location::new(
                                                        file_name.to_string(),
                                                        USizeTuple(
                                                            rhs_ident.1.start,
                                                            rhs_ident.1.end,
                                                        ),
                                                    ),
                                                ));
                                                return (
                                                    "!error".to_owned(),
                                                    TypedExpression::Error(),
                                                );
                                            }

                                            nums.push(
                                                swiz.iter()
                                                    .position(|c| {
                                                        *c == keys.chars().nth(i).unwrap()
                                                    })
                                                    .unwrap(),
                                            );
                                        }

                                        let len_str = keys.len().to_string();

                                        return (
                                            format!(
                                                "{}{}",
                                                lhs_type.get_name_without_vec(),
                                                if keys.len() == 1 {
                                                    ""
                                                } else {
                                                    len_str.as_str()
                                                }
                                            ),
                                            gen_cross_swizzle(
                                                _context,
                                                scope,
                                                graph,
                                                file_name,
                                                intermediate,
                                                &lhs_typed_expr,
                                                &lhs_type.get_name_without_vec(),
                                                lhs_type.get_vec_size(),
                                                keys.len(),
                                                &nums,
                                            ),
                                        );
                                    }
                                }

                                _context.alerts.push(SpannedAlert::error(
                                    "Scalar types can only be swizzled linearly. e.g. allowed: 1.xyz, disallowed: 1.zx".to_string(),
                                    format!("got: '{}'", keys),
                                    Location::new(
                                        file_name.to_string(),
                                        USizeTuple(rhs_ident.1.start, rhs_ident.1.end),
                                    ),
                                ));
                                return ("!error".to_owned(), TypedExpression::Error());
                            }

                            return get_type_field_dot(
                                _context,
                                graph,
                                file_name,
                                &lhs_type,
                                &lhs_typed_expr,
                                lhs_symbol,
                                lhs_symbol_node,
                                rhs_ident,
                                &span,
                            );
                        }
                        // else if let ast::Expression::Call(call) = rhs.as_ref() {
                        //     if let ast::Expression::Identifier(rhs_ident) =
                        //         call.0.expression.as_ref()
                        //     {
                        //         let (func_type, typed_expr) = get_type_field_dot(
                        //             _context,
                        //             graph,
                        //             file_name,
                        //             &lhs_type,
                        //             &lhs_typed_expr,
                        //             lhs_symbol,
                        //             lhs_symbol_node,
                        //             rhs_ident,
                        //             &span,
                        //         );

                        //         let func_type =
                        //             ExpandedType::from_string(graph, file_name, &func_type);

                        //         get_call_local(
                        //             _context,
                        //             scope,
                        //             graph,
                        //             file_name,
                        //             intermediate,
                        //             &call.0,
                        //             &func_type,
                        //             &typed_expr,
                        //         )
                        //     } else {
                        //         ("!error".to_owned(), TypedExpression::Error())
                        //     }
                        // }
                        else {
                            let (rhs_type, _rhs_typed_expr) = get_type_local(
                                _context,
                                scope,
                                graph,
                                file_name,
                                intermediate,
                                rhs,
                            );
                            let rhs_type = ExpandedType::from_string(graph, file_name, &rhs_type);
                            _context.alerts.push(SpannedAlert::error(
                                "Expected identifier".to_string(),
                                format!("got: '{}'", rhs_type.name),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(span.start, span.end),
                                ),
                            ));

                            add_struct_hints(_context, lhs_symbol, &rhs.get_span());
                            return ("!error".to_string(), TypedExpression::Error());
                        }
                    } else {
                        _context.alerts.push(SpannedAlert::error(
                            "Expected identifier after the dot".to_string(),
                            format!("got: nothing"),
                            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                        ));

                        add_struct_hints(_context, lhs_symbol, span);

                        return ("!error".to_string(), TypedExpression::Error());
                    }
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "Expected struct".to_string(),
                        format!("got: '{}'", lhs_type.name),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                    return ("!error".to_string(), TypedExpression::Error());
                }
            } else if let ast::Expression::Error((_, _)) = lhs.as_ref() {
                // Unary operator
                let (rhs_type, rhs_typed_expr) =
                    get_type_local(_context, scope, graph, file_name, intermediate, rhs);
                let rhs_type = ExpandedType::from_string(graph, file_name, &rhs_type);
                let rhs_symbol = check_type_local(
                    _context,
                    graph,
                    file_name,
                    &ast::Identifier {
                        name: rhs_type.name.clone(),
                        span: rhs.get_span(),
                    },
                );
                if let Some(rhs_symbol) = rhs_symbol {
                    let mut params = vec![rhs_type];
                    let method_op = check_method_local(
                        _context,
                        graph,
                        file_name,
                        span,
                        rhs_symbol,
                        &params[0].generics,
                        &format!("__prefix_operator_{}", op.get_code_name()),
                        &params,
                    );

                    let rhs_type_after = params.remove(0);

                    let mut rhs_type_name = rhs_type_after.name.clone();

                    if let Some(rhs_symbol_node) = graph
                        .files
                        .get(file_name)
                        .unwrap()
                        .get(&rhs_type_after.name)
                    {
                        rhs_type_name = rhs_symbol_node.get_namespaced();
                    }

                    (
                        method_op.to_owned(),
                        TypedExpression::Call(
                            format!(
                                "__{}___prefix_operator_{}",
                                rhs_type_name,
                                op.get_code_name()
                            ),
                            vec![rhs_typed_expr],
                            span.clone(),
                        ),
                    )
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "Undefined right-hand side".to_string(),
                        format!("The expression is undefined"),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                    return ("!error".to_string(), TypedExpression::Error());
                }
            } else {
                let (rhs_type, rhs_typed_expr) =
                    get_type_local(_context, scope, graph, file_name, intermediate, rhs);
                let rhs_type = ExpandedType::from_string(graph, file_name, &rhs_type);
                if let Some(lhs_symbol) = lhs_symbol {
                    let mut params = vec![lhs_type, rhs_type];
                    let method_op = check_method_local(
                        _context,
                        graph,
                        file_name,
                        span,
                        lhs_symbol,
                        &params[0].generics,
                        &format!("__operator_{}", op.get_code_name()),
                        &params,
                    );

                    let lhs_type_after = params.remove(0);

                    let mut lhs_type_name = lhs_type_after.name.clone();

                    if let Some(lhs_symbol_node) = graph
                        .files
                        .get(file_name)
                        .unwrap()
                        .get(&lhs_type_after.name)
                    {
                        lhs_type_name = lhs_symbol_node.get_namespaced();
                    }

                    (
                        method_op.to_owned(),
                        TypedExpression::Call(
                            format!("__{}___operator_{}", lhs_type_name, op.get_code_name()),
                            vec![lhs_typed_expr, rhs_typed_expr],
                            span.clone(),
                        ),
                    )
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "Undefined left-hand side".to_string(),
                        format!("The expression is undefined"),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                    return ("!error".to_string(), TypedExpression::Error());
                }
            }
        }
        Expression::List(_) => todo!(),
        Expression::Tuple((tup, span)) => {
            if tup.len() == 0 {
                _context.alerts.push(SpannedAlert::error(
                    "Undefined vector".to_string(),
                    format!("here"),
                    Location::new(
                        file_name.to_string(),
                        USizeTuple(expr.get_span().start, expr.get_span().end),
                    ),
                ));
                ("!error".to_string(), TypedExpression::Error())
            } else if tup.len() == 1 {
                let (expr_type, typed_expr) =
                    get_type_local(_context, scope, graph, file_name, intermediate, &tup[0]);
                (
                    expr_type,
                    TypedExpression::Wrap(Box::new(typed_expr), span.clone()),
                )
            } else {
                let mut types = vec![];
                let mut typed_exprs = vec![];
                for expr in tup {
                    let (expr_type, typed_expr) =
                        get_type_local(_context, scope, graph, file_name, intermediate, expr);
                    types.push(ExpandedType::from_string(graph, file_name, &expr_type));
                    typed_exprs.push(typed_expr);

                    if !types[types.len() - 1].is_scalar() {
                        _context.alerts.push(SpannedAlert::error(
                            "Non scalar vector element".to_string(),
                            format!("got: '{}'", types[types.len() - 1].name),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(expr.get_span().start, expr.get_span().end),
                            ),
                        ));
                        return ("!error".to_string(), TypedExpression::Error());
                    }

                    if types.len() > 1 {
                        if types[0].name != types[types.len() - 1].name {
                            _context.alerts.push(SpannedAlert::error(
                                "Illegal type mixing in vector".to_string(),
                                format!(
                                    "got: '{}' and '{}'",
                                    types[0].name,
                                    types[types.len() - 1].name
                                ),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(span.start, span.end),
                                ),
                            ));
                            return ("!error".to_string(), TypedExpression::Error());
                        }
                    }
                }

                if types.len() > 4 {
                    _context.alerts.push(SpannedAlert::error(
                        "Vector size too large".to_string(),
                        format!("Max of 4, got: '{}'", types.len()),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                    return ("!error".to_string(), TypedExpression::Error());
                }

                (
                    format!("{}{}", types[0].name, types.len()),
                    TypedExpression::Call(
                        format!("__{}{}___make_vec", types[0].name, types.len()),
                        typed_exprs,
                        span.clone(),
                    ),
                )
            }
        }
        Expression::Ternary(_) => todo!(),
        //_ => ("!error".to_owned(), TypedExpression::Error()),
    }
}

fn gen_cross_swizzle(
    _context: &mut ValidationContext,
    scope: &Scope,
    graph: &SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    expr: &TypedExpression,
    name: &str,
    from_len: String,
    to_len: usize,
    nums: &[usize],
) -> TypedExpression {
    let func_name = format!(
        "__swiz_cross_{}{}_{}",
        name,
        from_len,
        (0..to_len)
            .map(|i| format!("{}", nums[i]))
            .collect::<Vec<String>>()
            .join("_")
    );

    let params = vec![expr.clone()];

    if !intermediate.functions.contains_key(&func_name) {
        let tl = to_len.to_string();

        let func = TypedFunction {
            name: func_name.clone(),
            parameters: vec![TypedFunctionParameter {
                name: "a".to_owned(),
                type_name: ExpandedType::from_string(
                    graph,
                    file_name,
                    &format!("{}{}", name, from_len),
                ),
                default_value: None,
            }],
            return_type: ExpandedType::from_string(
                graph,
                file_name,
                &format!("{}{}", name, if to_len == 1 { "" } else { &tl }),
            ),
            body: TypedBody {
                statements: vec![],
                tags: vec![],
            },
            javascript: if to_len == 1 {
                Some(format!("return a[{}];", nums[0]))
            } else {
                Some(format!(
                    "return [{}];",
                    (0..to_len)
                        .map(|i| format!("a[{}]", nums[i]))
                        .collect::<Vec<String>>()
                        .join(", ")
                ))
            },
            tags: vec![],
            tagged: true,
            tagging: false,
        };

        intermediate.functions.insert(func_name.clone(), func);
    }

    let func = TypedExpression::Call(func_name, params, expr.get_span().clone());

    return func;
}

fn gen_up_swizzle(
    _context: &mut ValidationContext,
    scope: &Scope,
    graph: &SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    expr: &TypedExpression,
    name: &str,
    len: usize,
) -> TypedExpression {
    let func_name = format!("__swiz_up_{}_{}", name, len);

    let mut params = vec![expr.clone()];

    if !intermediate.functions.contains_key(&func_name) {
        let func = TypedFunction {
            name: func_name.clone(),
            parameters: vec![TypedFunctionParameter {
                name: "a".to_owned(),
                type_name: ExpandedType::from_string(graph, file_name, name.clone()),
                default_value: None,
            }],
            return_type: ExpandedType::from_string(graph, file_name, &format!("{}{}", name, len)),
            body: TypedBody {
                statements: vec![],
                tags: vec![],
            },
            javascript: Some(format!(
                "return [{}];",
                (0..len)
                    .map(|i| format!("a"))
                    .collect::<Vec<String>>()
                    .join(", ")
            )),
            tags: vec![],
            tagged: true,
            tagging: false,
        };

        intermediate.functions.insert(func_name.clone(), func);
    }

    let func = TypedExpression::Call(func_name, params, expr.get_span().clone());

    return func;
}

fn get_type_field_dot(
    _context: &mut ValidationContext,
    graph: &SymbolGraph,
    file_name: &str,
    lhs_type: &ExpandedType,
    lhs_typed_expr: &TypedExpression,
    lhs_symbol: &SymbolType,
    lhs_symbol_node: &SymbolNode,
    rhs_ident: &(ast::Identifier, Span),
    span: &Span,
) -> (String, TypedExpression) {
    let field = lhs_symbol.fields.iter().find(|x| x.0 == rhs_ident.0.name);
    let method = lhs_symbol.methods.iter().find(|x| x.0 == rhs_ident.0.name);
    if let Some(field) = field {
        add_type_field_hint(_context, lhs_symbol_node, &field.0, &field.1, span);
        return (
            field.1.clone(),
            TypedExpression::Call(
                format!(
                    "__get_struct_{}_{}",
                    lhs_symbol_node.get_namespaced(),
                    field.0
                ),
                vec![lhs_typed_expr.clone()],
                span.clone(),
            ),
        );
    } else if let Some(ref method) = method {
        add_type_method_hint(_context, lhs_symbol_node, &method.0, &method.1, span);
        return (
            format!(
                "function<{},{}>",
                method.1.return_type.clone().unwrap_or("void".to_owned()),
                method
                    .1
                    .parameters
                    .iter()
                    .map(|x| if x.2 {
                        format!("__optional<{}>", x.1.clone())
                    } else {
                        x.1.clone()
                    })
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            TypedExpression::Identifier(
                format!("__{}_{}", lhs_symbol_node.get_namespaced(), method.0),
                span.clone(),
            ),
        );
    } else {
        _context.alerts.push(SpannedAlert::error(
            "Field not found".to_string(),
            format!(
                "'{}' does not have a member named '{}'",
                lhs_type.name.clone(),
                rhs_ident.0.name.clone()
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
        return ("!error".to_string(), TypedExpression::Error());
    }
}

fn add_variable_hint(ctx: &mut ValidationContext, name: &str, local_type: &str, span: &Span) {
    ctx.intellisense.push(IntellisenseHint {
        start: span.start,
        end: span.end,
        goto_offset: span.start,
        label: format!("let {}: {}", name, local_type),
        kind: IntellisenseItemKind::Variable,
        detail: "type".to_string(),
        suggestions: vec![],
    });
}

fn build_shader_expression(
    _context: &mut ValidationContext,
    graph: &SymbolGraph,
    scope: &Scope,
    shader_def: &mut TypedShaderDefinition,
    file_name: &str,
    intermediate: &TypedIntermediate,
    typed_expression: &TypedExpression,
) {
    match typed_expression {
        TypedExpression::Call(func_name, args, _) => {
            shader_def.functions.insert(func_name.clone(), true);

            if let Some(func) = intermediate.functions.get(func_name) {
                build_shader_definition(
                    _context,
                    graph,
                    scope,
                    shader_def,
                    file_name,
                    intermediate,
                    &func.body,
                );
            }

            for arg in args {
                build_shader_expression(
                    _context,
                    graph,
                    scope,
                    shader_def,
                    file_name,
                    intermediate,
                    arg,
                );
            }
        }
        TypedExpression::Wrap(expr, _) => build_shader_expression(
            _context,
            graph,
            scope,
            shader_def,
            file_name,
            intermediate,
            expr,
        ),
        TypedExpression::Value(_, _) => {}
        TypedExpression::Identifier(ident, _) => {
            if let Some((scoped_var, barrier)) = scope.check_with_shader_barrier(ident, false) {
                if barrier {
                    shader_def
                        .parameters
                        .insert(ident.clone(), scoped_var.clone());
                }
            }
        }
        TypedExpression::KVMap(_, _) => {}
        TypedExpression::Shader(_, span) => _context.alerts.push(SpannedAlert::error(
            "Illegal shader def".to_string(),
            "Shaders cannot be defined inside other shaders".to_string(),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        )),
        TypedExpression::Error() => {}
    }
}

// Recursively descends into the Typed tree and adds references to the shader_inst
fn build_shader_definition(
    _context: &mut ValidationContext,
    graph: &SymbolGraph,
    new_scope: &Scope,
    shader_def: &mut TypedShaderDefinition,
    file_name: &str,
    intermediate: &TypedIntermediate,
    typed_body: &TypedBody,
) {
    for statement in &typed_body.statements {
        match statement {
            TypedStatement::Return(_, span) => _context.alerts.push(SpannedAlert::error(
                "Return statement not allowed in shader".to_string(),
                "Return statements are only allowed in functions".to_string(),
                Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
            )),
            TypedStatement::If {
                condition,
                body,
                else_ifs,
                else_body,
                span: _,
            } => {
                build_shader_expression(
                    _context,
                    graph,
                    new_scope,
                    shader_def,
                    file_name,
                    intermediate,
                    condition,
                );
                build_shader_definition(
                    _context,
                    graph,
                    new_scope,
                    shader_def,
                    file_name,
                    intermediate,
                    body,
                );
                for else_if in else_ifs {
                    build_shader_expression(
                        _context,
                        graph,
                        new_scope,
                        shader_def,
                        file_name,
                        intermediate,
                        &else_if.0,
                    );
                    build_shader_definition(
                        _context,
                        graph,
                        new_scope,
                        shader_def,
                        file_name,
                        intermediate,
                        &else_if.1,
                    );
                }
                if let Some(else_body) = else_body {
                    build_shader_definition(
                        _context,
                        graph,
                        new_scope,
                        shader_def,
                        file_name,
                        intermediate,
                        else_body,
                    );
                }
            }
            TypedStatement::Let {
                name: _,
                value,
                span: _,
            } => {
                build_shader_expression(
                    _context,
                    graph,
                    new_scope,
                    shader_def,
                    file_name,
                    intermediate,
                    value,
                );
            }
            TypedStatement::Expression(expr, _) => {
                build_shader_expression(
                    _context,
                    graph,
                    new_scope,
                    shader_def,
                    file_name,
                    intermediate,
                    expr,
                );
            }
        }
    }
}

#[derive(Clone)]
pub struct ExpandedType {
    name: String,
    generics: Vec<ExpandedType>,
    symbol_type: Option<SymbolType>,
}

impl ExpandedType {
    pub fn new(
        graph: &SymbolGraph,
        file_name: &str,
        name: String,
        generics: Vec<ExpandedType>,
    ) -> Self {
        let symbol_type = || {
            let primitive = graph.primitive.get(&name);

            if primitive.is_some() {
                let inner_wrap = if let SymbolDefinition::Type(ref symbol_type) =
                    primitive.unwrap().definition
                {
                    symbol_type.clone()
                } else {
                    unreachable!()
                };
                return Some(inner_wrap);
            }

            let file = graph.files.get(file_name).unwrap();

            let symbol = file.get(&name);

            if symbol.is_none() {
                return None;
            }

            if symbol.is_some() {
                let symbol = symbol.unwrap();

                if let SymbolDefinition::Type(def_type) = &symbol.definition {
                    return Some(def_type.clone());
                } else {
                    return None;
                }
            } else {
                return None;
            }
        };

        Self {
            name: name.clone(),
            generics,
            symbol_type: symbol_type(),
        }
    }

    pub fn get_name_without_vec(&self) -> String {
        let mut s = self.name.clone();
        s.pop();
        return s;
    }

    pub fn get_vec_size(&self) -> String {
        self.name
            .as_str()
            .chars()
            .last()
            .unwrap()
            .to_string()
            .parse()
            .unwrap()
    }

    pub fn is_scalar(&self) -> bool {
        if let Some(ref symbol_type) = self.symbol_type {
            return symbol_type
                .methods
                .iter()
                .find(|method| method.0 == "__is_scalar")
                .is_some();
        }

        false
    }

    pub fn is_vec(&self, size: i32) -> bool {
        if let Some(ref symbol_type) = self.symbol_type {
            return symbol_type
                .methods
                .iter()
                .find(|method| method.0 == format!("__is_vec_{}", size))
                .is_some();
        }

        false
    }

    pub fn is_any_vec(&self) -> bool {
        return self.is_vec(2) || self.is_vec(3) || self.is_vec(4);
    }

    pub fn is_compatible_with(&self, other: &ExpandedType) -> bool {
        if self.name == "any" || other.name == "any" {
            return true;
        }

        if self.is_scalar() && other.is_scalar() {
            return true;
        }

        if self.is_vec(2) && other.is_vec(2) {
            return true;
        }

        if self.is_vec(3) && other.is_vec(3) {
            return true;
        }

        if self.is_vec(4) && other.is_vec(4) {
            return true;
        }

        if self.name != other.name {
            return false;
        }

        if self.generics.len() != other.generics.len() {
            return false;
        }

        for (self_generic, other_generic) in self.generics.iter().zip(other.generics.iter()) {
            if !self_generic.is_compatible_with(other_generic) {
                return false;
            }
        }

        true
    }

    pub fn wrap_cast(&self, other: &ExpandedType, expr: TypedExpression) -> TypedExpression {
        let span = expr.get_span();
        if self.is_scalar() && other.is_scalar() {
            if other.name != self.name {
                return TypedExpression::Call(
                    format!("__{}___cast_from_scalar", self.name),
                    vec![expr],
                    span,
                );
            } else {
                return expr;
            }
        } else if (self.is_vec(2) && other.is_vec(2))
            || (self.is_vec(3) && other.is_vec(3))
            || (self.is_vec(4) && other.is_vec(4))
        {
            if other.name != self.name {
                return TypedExpression::Call(
                    format!("__{}___cast_from_vec", self.name),
                    vec![expr],
                    span,
                );
            } else {
                return expr;
            }
        } else {
            return expr;
        }
    }

    // Recursively convert types with generics: map<string, array<int>>
    pub fn from_string(graph: &SymbolGraph, file_name: &str, string: &str) -> Self {
        let mut name = String::new();
        let mut generics = vec![];
        let mut generic = String::new();
        let mut depth = 0;

        for c in string.chars() {
            if c == '<' {
                depth += 1;
                if depth == 1 {
                    continue;
                }
            } else if c == '>' {
                depth -= 1;
                if depth == 0 {
                    generics.push(ExpandedType::from_string(graph, file_name, &generic));
                    generic = String::new();
                    continue;
                }
            } else if c == ',' && depth == 1 {
                generics.push(ExpandedType::from_string(graph, file_name, &generic));
                generic = String::new();
                continue;
            }

            if depth == 0 {
                name.push(c);
            } else {
                generic.push(c);
            }
        }

        ExpandedType::new(graph, file_name, name, generics)
    }
}

impl ToString for ExpandedType {
    fn to_string(&self) -> String {
        let mut string = self.name.clone();

        if self.generics.len() > 0 {
            string.push('<');

            for (i, generic) in self.generics.iter().enumerate() {
                if i > 0 {
                    string.push(',');
                }

                string.push_str(generic.to_string().as_str());
            }

            string.push('>');
        }

        string
    }
}

fn check_body_local(
    _context: &mut ValidationContext,
    scope: &mut Scope,
    graph: &SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    body: &Vec<ast::Root>,
    output: &ExpandedType,
) -> TypedBody {
    let mut typed_body = TypedBody {
        tags: vec![],
        statements: vec![],
    };
    for root in body {
        match root {
            ast::Root::Let(_let) => {
                let t = scope.check(&_let.name.name);
                if t.is_some() {
                    _context.alerts.push(SpannedAlert::error(
                        "Hiding variable".to_string(),
                        format!("'{}' is already defined in this scope", _let.name.name),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(_let.name.span.start, _let.name.span.end),
                        ),
                    ));
                } else {
                    let mut _to_val = "void".to_owned();
                    let mut _to_typed_expr = TypedExpression::Value(TypedValue::Null, 0..0);
                    if _let.to.is_some() {
                        (_to_val, _to_typed_expr) = get_type_local(
                            _context,
                            scope,
                            graph,
                            file_name,
                            intermediate,
                            _let.to.as_ref().unwrap(),
                        );
                    }

                    let let_type = if let Some(ref _type) = _let.value_type {
                        _type.name.clone()
                    } else {
                        _to_val
                    };
                    scope
                        .definitions
                        .insert(_let.name.name.clone(), let_type.clone());

                    typed_body.statements.push(TypedStatement::Let {
                        name: _let.name.name.clone(),
                        value: _to_typed_expr,
                        span: _let.span.clone(),
                    });
                }
            }
            ast::Root::Expression(expr) => {
                let (_, typed_expr) =
                    get_type_local(_context, scope, graph, file_name, intermediate, expr);

                typed_body
                    .statements
                    .push(TypedStatement::Expression(typed_expr, expr.get_span()));
            }
            ast::Root::If(_if) => {
                let (_type, _cond_typed) = get_type_local(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    &_if.condition,
                );

                if _type != "bool" {
                    _context.alerts.push(SpannedAlert::error(
                        "Expected bool-like expression in if".to_string(),
                        format!("got: '{}'", _type),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(
                                _if.condition.get_span().start,
                                _if.condition.get_span().end,
                            ),
                        ),
                    ));
                }

                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(scope));
                let _typed_body = check_body_local(
                    _context,
                    &mut new_scope,
                    graph,
                    file_name,
                    intermediate,
                    &_if.body.roots,
                    output,
                );

                let mut else_body = None;
                let mut else_ifs = Vec::new();

                for elif in &_if.else_ifs {
                    let (_type, _else_if_typed_cond) = get_type_local(
                        _context,
                        scope,
                        graph,
                        file_name,
                        intermediate,
                        &elif.condition,
                    );
                    if _type != "bool" {
                        _context.alerts.push(SpannedAlert::error(
                            "Expected bool-like expression in else if".to_string(),
                            format!("got: '{}'", _type),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(
                                    elif.condition.get_span().start,
                                    elif.condition.get_span().end,
                                ),
                            ),
                        ));
                    }

                    let mut new_scope = Scope::new();
                    new_scope.parent = Some(Box::new(scope));
                    let else_if_typed = check_body_local(
                        _context,
                        &mut new_scope,
                        graph,
                        file_name,
                        intermediate,
                        &elif.body.roots,
                        output,
                    );

                    else_ifs.push((_else_if_typed_cond, else_if_typed));
                }

                if _if.else_body.is_some() {
                    let mut new_scope = Scope::new();
                    new_scope.parent = Some(Box::new(scope));
                    let _typed_else_body = check_body_local(
                        _context,
                        &mut new_scope,
                        graph,
                        file_name,
                        intermediate,
                        &_if.else_body.as_ref().unwrap().roots,
                        output,
                    );

                    else_body = Some(_typed_else_body);
                }

                typed_body.statements.push(TypedStatement::If {
                    condition: _cond_typed,
                    body: _typed_body,
                    else_ifs,
                    else_body,
                    span: _if.span.clone(),
                });
            }
            ast::Root::Return(_return) => {
                if _return.value.is_some() {
                    let (_type, typed_expr) = get_type_local(
                        _context,
                        scope,
                        graph,
                        file_name,
                        intermediate,
                        &_return.value.as_ref().unwrap(),
                    );
                    let expanded_type = ExpandedType::from_string(graph, file_name, &_type);
                    if !expanded_type.is_compatible_with(output) {
                        _context.alerts.push(SpannedAlert::error(
                            "Incompatible return type".to_string(),
                            format!("expected: '{}', got: '{}'", output.to_string(), _type),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(
                                    _return.value.as_ref().unwrap().get_span().start,
                                    _return.value.as_ref().unwrap().get_span().end,
                                ),
                            ),
                        ));
                    }

                    typed_body.statements.push(TypedStatement::Return(
                        expanded_type.wrap_cast(output, typed_expr),
                        _return.span.clone(),
                    ));
                } else {
                    if output.to_string() != "void" {
                        _context.alerts.push(SpannedAlert::error(
                            "Incompatible return type".to_string(),
                            format!("expected: '{}', got: '{}'", output.to_string(), "void"),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(_return.span.start, _return.span.end),
                            ),
                        ));
                    }
                }
            }
            _ => _context.alerts.push(SpannedAlert::error(
                format!("You cannot define this here"),
                format!("Attempting to define {}", root.get_kind()),
                Location::new(
                    file_name.to_string(),
                    USizeTuple(root.get_span().start, root.get_span().end),
                ),
            )),
        }
    }

    typed_body
}

#[allow(dead_code)]
pub fn validate<'a>(
    graph: &'a SymbolGraph,
    file_name: &str,
) -> (ValidationContext, TypedIntermediate) {
    let mut context = ValidationContext {
        alerts: Vec::new(),
        intellisense: Vec::new(),
    };

    let file = graph.files.get(file_name);
    let mut typed = TypedIntermediate {
        functions: HashMap::new(),
        structs: Vec::new(),
        shaders: Vec::new(),
    };

    if file.is_none() {
        return (context, typed);
    }

    let file = file.unwrap();

    let check_type = |_context: &mut ValidationContext, name: &ast::Identifier| {
        check_type_local(_context, graph, file_name, name);
    };

    // let parse_symbol_type =
    //     |_context: &mut ValidationContext, scope: Scope, expr: Expression| -> ExpandedType {};

    // let get_type = |_context: &mut ValidationContext,
    //                 scope: &Scope,
    //                 expr: &Expression|
    //  -> (String, TypedExpression) {

    // };

    let _check_body = |_context: &mut ValidationContext,
                       scope: &mut Scope,
                       body: &Vec<ast::Root>,
                       intermediate: &mut TypedIntermediate,
                       output: &ExpandedType|
     -> TypedBody {
        check_body_local(
            _context,
            scope,
            graph,
            file_name,
            intermediate,
            body,
            output,
        )
    };

    fn add_function_local<'a>(
        _context: &mut ValidationContext,
        graph: &'a SymbolGraph,
        file_name: &str,
        intermediate: &mut TypedIntermediate,
        function: &ast::Function,
    ) -> TypedFunction {
        for (i, param) in function.parameters.iter().enumerate() {
            for (j, param2) in function.parameters.iter().enumerate() {
                if i < j && param.0.name == param2.0.name {
                    _context.alerts.push(SpannedAlert::error_2(
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

            check_type_local(_context, graph, file_name, &param.1);
        }

        let mut scope = Scope::new();
        for param in &function.parameters {
            scope
                .definitions
                .insert(param.0.name.clone(), param.1.name.clone());
        }

        let output = match function.return_type {
            Some(ref _type) => ExpandedType::from_string(graph, file_name, &_type.name),
            None => ExpandedType::from_string(graph, file_name, "void"),
        };

        let typed_body = check_body_local(
            _context,
            &mut scope,
            &graph,
            file_name,
            intermediate,
            &function.body.roots,
            &output,
        );

        let symbol_node = graph
            .files
            .get(file_name)
            .unwrap()
            .get(&function.name.name)
            .unwrap();

        TypedFunction {
            tagging: false,
            tagged: false,
            tags: vec![],
            javascript: None,
            name: symbol_node.get_namespaced(),
            parameters: function
                .parameters
                .iter()
                .map(|(param, _type, expr)| TypedFunctionParameter {
                    name: param.name.clone(),
                    default_value: match expr {
                        Some(expr) => Some(
                            get_type_local(
                                _context,
                                &scope,
                                graph,
                                file_name,
                                &mut TypedIntermediate {
                                    functions: HashMap::new(),
                                    structs: vec![],
                                    shaders: vec![],
                                },
                                &expr,
                            )
                            .1,
                        ),
                        None => None,
                    },
                    type_name: ExpandedType::from_string(graph, file_name, &_type.name),
                })
                .collect(),
            return_type: output,
            body: typed_body,
        }
    }

    for symbol in file.values() {
        if symbol.imported {
            continue;
        }

        match &symbol.root {
            ast::Root::Function(function) => {
                let typed_func =
                    add_function_local(&mut context, graph, file_name, &mut typed, function);
                typed.functions.insert(typed_func.name.clone(), typed_func);

                add_function_hint(&mut context, &symbol, &symbol.root.get_def_span());
            }
            ast::Root::Struct(_struct) => {
                add_type_hint(&mut context, &symbol, &symbol.root.get_def_span());

                for (i, field) in _struct.fields.iter().enumerate() {
                    for (j, field2) in _struct.fields.iter().enumerate() {
                        if i < j && field.0.name == field2.0.name {
                            context.alerts.push(SpannedAlert::error_2(
                                format!("Duplicate"),
                                format!("Duplicate field name: '{}'", field2.0.name),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(field2.0.span.start, field2.0.span.end),
                                ),
                                format!("first defined here"),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(field.0.span.start, field.0.span.end),
                                ),
                            ));
                        }
                    }

                    check_type(&mut context, &field.1);
                }

                typed.structs.push((
                    symbol.get_namespaced(),
                    _struct
                        .fields
                        .iter()
                        .map(|f| (f.0.name.clone(), f.1.name.clone()))
                        .collect(),
                ));

                let make_name = format!("__make_struct_{}", symbol.get_namespaced());
                let mut make_body = String::new();

                for field in &_struct.fields {
                    make_body.push_str(&format!("{}: fields.{}, ", field.0.name, field.0.name));
                }

                typed.functions.insert(
                    make_name.clone(),
                    TypedFunction {
                        tagged: false,
                        tagging: false,
                        tags: vec![],
                        name: make_name,
                        parameters: vec![TypedFunctionParameter {
                            name: "fields".to_string(),
                            type_name: ExpandedType::from_string(graph, file_name, "void"),
                            default_value: None,
                        }],
                        return_type: ExpandedType::from_string(
                            graph,
                            file_name,
                            &symbol.get_namespaced(),
                        ),
                        body: TypedBody {
                            tags: vec![],
                            statements: vec![],
                        },
                        javascript: Some(format!("return {{{}}};", make_body)),
                    },
                );

                for field in &_struct.fields {
                    let f_name =
                        format!("__get_struct_{}_{}", symbol.get_namespaced(), field.0.name);
                    typed.functions.insert(
                        f_name.clone(),
                        TypedFunction {
                            tagged: false,
                            tagging: false,
                            tags: vec![],
                            name: f_name,
                            parameters: vec![TypedFunctionParameter {
                                name: "struct".to_string(),
                                type_name: ExpandedType::from_string(
                                    graph,
                                    file_name,
                                    &symbol.get_namespaced(),
                                ),
                                default_value: None,
                            }],
                            return_type: ExpandedType::from_string(
                                graph,
                                file_name,
                                &symbol.get_namespaced(),
                            ),
                            body: TypedBody {
                                tags: vec![],
                                statements: vec![],
                            },
                            javascript: Some(format!("return struct.{};", field.0.name)),
                        },
                    );
                }

                // let graph_symbol = graph
                //     .files
                //     .get(file_name)
                //     .unwrap()
                //     .get(&_struct.name.name)
                //     .unwrap();

                // if let SymbolDefinition::Type(ref type_def) = graph_symbol.definition {
                //     for method in type_def.methods {
                //         method.1.
                //     }
                // }
            }
            ast::Root::Impl(_impl) => {
                for method in &_impl.body {
                    match method {
                        ast::Root::Function(func) => {
                            let func_typed = add_function_local(
                                &mut context,
                                graph,
                                file_name,
                                &mut typed,
                                &func,
                            );
                            typed.functions.insert(
                                format!("__{}_{}", _impl.name.name, func_typed.name),
                                func_typed,
                            );
                        }
                        _ => unreachable!(),
                    };
                }
            }
            _ => {}
        }
    }

    (context, typed)
}

fn add_type_field_hint(
    ctx: &mut ValidationContext,
    symbol: &SymbolNode,
    name: &str,
    ty: &str,
    span: &Span,
) {
    ctx.intellisense.push(IntellisenseHint {
        start: span.start,
        end: span.end,
        goto_offset: symbol.span.start,
        label: format!("{}::{}: {}", symbol.name, name, ty),
        kind: IntellisenseItemKind::Field,
        detail: "field".to_string(),
        suggestions: vec![],
    });
}

fn add_type_method_hint(
    ctx: &mut ValidationContext,
    symbol: &SymbolNode,
    name: &str,
    method: &SymbolFunction,
    span: &Span,
) {
    let func_hint = get_hint_for_function(name, &method);
    ctx.intellisense.push(IntellisenseHint {
        start: span.start,
        end: span.end,
        goto_offset: method.span.start,
        label: format!("{}::{}{}", symbol.name, func_hint.label, func_hint.detail),
        kind: IntellisenseItemKind::Field,
        detail: "field".to_string(),
        suggestions: vec![],
    });
}

fn add_type_hint(ctx: &mut ValidationContext, symbol: &SymbolNode, span: &Span) {
    ctx.intellisense.push(IntellisenseHint {
        start: span.start,
        end: span.end,
        goto_offset: symbol.span.start,
        label: format!("struct {}", symbol.name),
        kind: IntellisenseItemKind::Type,
        detail: "type".to_string(),
        suggestions: vec![],
    });
}

fn add_function_hint(ctx: &mut ValidationContext, symbol: &SymbolNode, span: &Span) {
    if let SymbolDefinition::Function(func_def) = &symbol.definition {
        let func_hint = get_hint_for_function(&symbol.name, &func_def);
        ctx.intellisense.push(IntellisenseHint {
            start: span.start,
            end: span.end,
            goto_offset: symbol.span.start,
            label: format!("fn {}{}", func_hint.label, func_hint.detail),
            kind: IntellisenseItemKind::Function,
            detail: "".to_string(),
            suggestions: vec![],
        });
    }
}

pub fn tag_function(intermediate: &mut TypedIntermediate, func_name: &str) {
    {
        let func = intermediate.functions.get_mut(func_name).unwrap();
        if func.tagged || func.tagging {
            return;
        }
        func.tagging = true;
    }
    let func_read = intermediate.functions.get(func_name).unwrap().body.clone();
    let tags = propagate_tags(intermediate, &func_read);

    let func2 = intermediate.functions.get_mut(func_name).unwrap();
    func2.tags = tags;
    func2.tagging = false;
    func2.tagged = false;
}

pub fn retag(func_name: &str, span: &Span, tags: &mut Vec<TypedTag>) -> Vec<TypedTag> {
    let mut new_tags = vec![];
    for tag in tags {
        new_tags.push(TypedTag {
            tag: tag.tag.clone(),
            span: span.clone(),
            name: func_name.to_owned(),
            introduced_by: Some(Box::new(tag.clone())),
        });
    }
    new_tags
}

pub fn propagate_tags(intermediate: &mut TypedIntermediate, body: &TypedBody) -> Vec<TypedTag> {
    let mut tags = vec![];

    for statement in body.statements.iter() {
        match statement {
            TypedStatement::Return(expr, _) => {
                tags.append(&mut propagate_tags_from_expression(intermediate, expr))
            }
            TypedStatement::If {
                condition,
                body,
                else_ifs,
                else_body,
                span: _,
            } => {
                tags.append(&mut propagate_tags_from_expression(intermediate, condition));
                tags.append(&mut propagate_tags(intermediate, body));
                for else_if in else_ifs {
                    tags.append(&mut propagate_tags_from_expression(
                        intermediate,
                        &else_if.0,
                    ));
                    tags.append(&mut propagate_tags(intermediate, &else_if.1));
                }
                if let Some(else_body) = else_body {
                    tags.append(&mut propagate_tags(intermediate, else_body));
                }
            }
            TypedStatement::Let {
                name: _,
                value,
                span: _,
            } => {
                tags.append(&mut propagate_tags_from_expression(intermediate, value));
            }
            TypedStatement::Expression(expr, _) => {
                tags.append(&mut propagate_tags_from_expression(intermediate, expr));
            }
        }
    }

    tags
}

pub fn propagate_tags_from_expression(
    intermediate: &mut TypedIntermediate,
    expr: &TypedExpression,
) -> Vec<TypedTag> {
    let mut tags = vec![];

    match expr {
        TypedExpression::Call(func_name, exprs, span) => {
            if let Some(_) = intermediate.functions.get(func_name) {
                tag_function(intermediate, func_name);

                let func = intermediate.functions.get(func_name).unwrap();
                tags.append(&mut retag(func_name, span, &mut func.tags.clone()));

                for expr in exprs {
                    tags.append(&mut propagate_tags_from_expression(intermediate, expr));
                }
            }
        }
        TypedExpression::Wrap(_, _) => {}
        TypedExpression::Value(_, _) => {}
        TypedExpression::Identifier(_, _) => {}
        TypedExpression::KVMap(_, _) => {}
        TypedExpression::Shader(_, _) => {}
        TypedExpression::Error() => {}
    }

    tags
}
