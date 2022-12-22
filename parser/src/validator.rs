use chumsky::primitive::Container;
use serde::{Deserialize, Serialize};

use std::any::type_name;
use std::collections::HashMap;

use crate::ast::{self, AstWalker, Call, Expression, Location, Op, Span, USizeTuple};
use crate::graph::{SymbolDefinition, SymbolFunction, SymbolGraph, SymbolNode, SymbolType};
use crate::printer::SpannedAlert;

#[derive(Serialize, Deserialize, Clone)]
pub struct ScopeVariableDefinition {
    pub type_name: String,
    pub span: Span,
    pub is_global: bool,
    pub is_constant: bool,
}

impl ScopeVariableDefinition {
    pub fn new(type_name: String, span: Span) -> ScopeVariableDefinition {
        ScopeVariableDefinition {
            type_name,
            span,
            is_global: false,
            is_constant: false,
        }
    }
}

pub struct Scope<'a> {
    shader_barrier: bool,
    definitions: HashMap<String, ScopeVariableDefinition>,
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
    pub globals: HashMap<String, String>,
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
    List(Vec<TypedExpression>, Span),
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
            TypedExpression::List(_, span) => span.clone(),
        }
    }

    pub fn scan_calls(&self, call: &dyn Fn(&str)) {
        match self {
            TypedExpression::Call(name, exprs, _) => {
                call(name);
                for expr in exprs {
                    expr.scan_calls(call);
                }
            }
            TypedExpression::Value(_, _) => {}
            TypedExpression::Identifier(_, _) => {}
            TypedExpression::KVMap(_, _) => {}
            TypedExpression::Shader(_, _) => {}
            TypedExpression::Wrap(expr, _) => expr.scan_calls(call),
            TypedExpression::Error() => {}
            TypedExpression::List(exprs, _) => {
                for expr in exprs {
                    expr.scan_calls(call);
                }
            }
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
    Set(String, TypedExpression, Span),
    If {
        condition: TypedExpression,
        body: TypedBody,
        else_ifs: Vec<(TypedExpression, TypedBody)>,
        else_body: Option<TypedBody>,
        span: Span,
    },
    Let {
        name: String,
        type_name: ExpandedType,
        value: TypedExpression,
        span: Span,
    },
    ForEach {
        value: String,
        key: Option<String>,
        iterator: TypedExpression,
        body: TypedBody,
        span: Span,
    },
    While {
        condition: TypedExpression,
        body: TypedBody,
        span: Span,
    },
    For {
        init: Box<TypedStatement>,
        condition: TypedExpression,
        update: TypedExpression,
        body: TypedBody,
        span: Span,
    },
    Break(Span),
    Continue(Span),
    Expression(TypedExpression, Span),
}

impl TypedStatement {
    #[allow(dead_code)]
    pub fn get_span(&self) -> Span {
        match self {
            TypedStatement::Set(_, _, span) => span.clone(),
            TypedStatement::Return(_, span) => span.clone(),
            TypedStatement::If { span, .. } => span.clone(),
            TypedStatement::Let { span, .. } => span.clone(),
            TypedStatement::Expression(_, span) => span.clone(),
            TypedStatement::ForEach { span, .. } => span.clone(),
            TypedStatement::While { span, .. } => span.clone(),
            TypedStatement::For { span, .. } => span.clone(),
            TypedStatement::Break(span) => span.clone(),
            TypedStatement::Continue(span) => span.clone(),
        }
    }

    pub fn scan_calls(&self, call: &dyn Fn(&str)) {
        match self {
            TypedStatement::Return(expr, _) => expr.scan_calls(call),
            TypedStatement::Set(_, expr, _) => expr.scan_calls(call),
            TypedStatement::If {
                condition,
                body,
                else_ifs,
                else_body,
                span,
            } => {
                condition.scan_calls(call);
                body.scan_calls(call);
                for (condition, body) in else_ifs {
                    condition.scan_calls(call);
                    body.scan_calls(call);
                }
                if let Some(body) = else_body {
                    body.scan_calls(call);
                }
            }
            TypedStatement::Let {
                name,
                type_name,
                value,
                span,
            } => value.scan_calls(call),
            TypedStatement::ForEach {
                value,
                key,
                iterator,
                body,
                span,
            } => {
                iterator.scan_calls(call);
                body.scan_calls(call);
            }
            TypedStatement::While {
                condition,
                body,
                span,
            } => {
                condition.scan_calls(call);
                body.scan_calls(call);
            }
            TypedStatement::For {
                init,
                condition,
                update,
                body,
                span,
            } => {
                init.as_ref().scan_calls(call);
                condition.scan_calls(call);
                update.scan_calls(call);
                body.scan_calls(call);
            }
            TypedStatement::Break(_) => {}
            TypedStatement::Continue(_) => {}
            TypedStatement::Expression(expr, _) => expr.scan_calls(call),
        }
    }
}

#[allow(dead_code)]
#[derive(PartialEq, Clone)]
pub enum TypedTagType {
    Async,
    DynamicAlloc,
    CPUOnly,
    ShaderOnly,
    Recursive,
    Throws,
    GlobalVariableAccess,
}

impl ToString for TypedTagType {
    fn to_string(&self) -> String {
        match self {
            TypedTagType::Async => "async".to_string(),
            TypedTagType::DynamicAlloc => "dynamic_alloc".to_string(),
            TypedTagType::CPUOnly => "cpu_only".to_string(),
            TypedTagType::ShaderOnly => "shader_only".to_string(),
            TypedTagType::Recursive => "recursive".to_string(),
            TypedTagType::Throws => "can except".to_string(),
            TypedTagType::GlobalVariableAccess => "global var access".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct TypedTag {
    pub tag: TypedTagType,
    pub span: Span,
    pub name: String,
    pub type_name: String,
    pub introduced_by: Option<Box<TypedTag>>,
}

#[derive(Clone)]
pub struct TypedBody {
    pub statements: Vec<TypedStatement>,
    pub tags: Vec<TypedTag>,
}

impl TypedBody {
    pub fn scan_calls(&self, call: &dyn Fn(&str)) -> bool {
        for statement in &self.statements {
            statement.scan_calls(call);
        }
        false
    }
}

#[derive(Clone)]
pub struct TypedFunction {
    pub name: String,
    pub parameters: Vec<TypedFunctionParameter>,
    pub return_type: ExpandedType,
    pub body: TypedBody,
    pub javascript: Option<String>,
    pub webgl: Option<String>,
    pub tags: Vec<TypedTag>,
    pub tagged: bool,
    pub tagging: bool,
}

impl TypedFunction {
    pub fn has_tag(&self, tag: &TypedTagType) -> bool {
        for t in &self.tags {
            if t.tag == *tag {
                return true;
            }
        }
        false
    }

    pub fn scan_calls(&self, call: &dyn Fn(&str)) {
        self.body.scan_calls(call);
    }
}

#[derive(Clone)]
pub struct TypedIntermediate {
    pub functions: HashMap<String, TypedFunction>,
    pub structs: Vec<(String, Vec<(String, String)>)>,
    pub shaders: Vec<TypedShaderDefinition>,
    pub globals: HashMap<String, String>,
}

impl TypedIntermediate {
    pub fn get_function(&self, name: &str) -> Option<&TypedFunction> {
        self.functions.get(name)
    }
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
        TypedExpression::List(expr, _) => {
            for e in expr {
                shake_expression(
                    graph,
                    scope,
                    file_name,
                    in_intermediate,
                    out_intermediate,
                    e,
                );
            }
        }
    }
}

// Recursively descends into the Typed tree and adds references to the intermediate
fn shake_statement(
    graph: &SymbolGraph,
    new_scope: &Scope,
    file_name: &str,
    in_intermediate: &TypedIntermediate,
    out_intermediate: &mut TypedIntermediate,
    statement: &TypedStatement,
) {
    match statement {
        TypedStatement::Set(_, expr, _) => {
            shake_expression(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                expr,
            );
        }
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
            type_name,
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
        TypedStatement::Break(_) => {}
        TypedStatement::Continue(_) => {}
        TypedStatement::For {
            init,
            condition,
            update,
            body,
            span,
        } => {
            shake_statement(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                &*init,
            );
            shake_expression(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                condition,
            );
            shake_expression(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                update,
            );
            shake_body(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                body,
            );
        }
        TypedStatement::While {
            condition,
            body,
            span,
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
        }

        TypedStatement::ForEach {
            value,
            key,
            iterator,
            body,
            span,
        } => {
            shake_expression(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                iterator,
            );
            shake_body(
                graph,
                new_scope,
                file_name,
                in_intermediate,
                out_intermediate,
                body,
            );
        }
    }
}
fn shake_body(
    graph: &SymbolGraph,
    new_scope: &Scope,
    file_name: &str,
    in_intermediate: &TypedIntermediate,
    out_intermediate: &mut TypedIntermediate,
    typed_body: &TypedBody,
) {
    for statement in &typed_body.statements {
        shake_statement(
            graph,
            new_scope,
            file_name,
            in_intermediate,
            out_intermediate,
            statement,
        );
    }
}

impl TypedIntermediate {
    pub fn tree_shake(
        &self,
        graph: &SymbolGraph,
        file_name: &str,
        root_funcs: Vec<&str>,
    ) -> TypedIntermediate {
        let mut out = TypedIntermediate {
            functions: HashMap::new(),
            structs: self.structs.clone(),
            shaders: self.shaders.clone(),
            globals: self.globals.clone(),
        };

        let scope = Scope::new();

        for root_func in root_funcs {
            if self.functions.contains_key(root_func) {
                out.functions
                    .insert(root_func.to_owned(), self.functions[root_func].clone());

                shake_body(
                    graph,
                    &scope,
                    file_name,
                    self,
                    &mut out,
                    &self.functions[root_func].body,
                );
            }
        }

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
            globals: self.globals.clone(),
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
            webgl: None,
            tags: Vec::new(),
            tagged: false,
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
            Some(definition.type_name.clone())
        } else if let Some(parent) = &self.parent {
            parent.check(name)
        } else {
            None
        }
    }

    pub fn get(&self, name: &str) -> Option<&ScopeVariableDefinition> {
        if let Some(definition) = self.definitions.get(name) {
            Some(definition)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
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
            Some((definition.type_name.clone(), broke_barrier))
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
) -> (Option<&'a SymbolType>, Option<&'a SymbolNode>) {
    let primitive = graph.primitive.get(&name.name);

    if primitive.is_some() {
        let inner_wrap =
            if let SymbolDefinition::Type(ref symbol_type) = primitive.unwrap().definition {
                symbol_type
            } else {
                unreachable!()
            };
        return (Some(inner_wrap), primitive);
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

        return (None, None);
    }

    if symbol.is_some() {
        let symbol = symbol.unwrap();

        if let SymbolDefinition::Type(def_type) = &symbol.definition {
            return (Some(def_type), Some(symbol));
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

            return (None, None);
        }
    }

    (None, None)
}

fn check_method_local<'a>(
    _context: &mut ValidationContext,
    graph: &'a SymbolGraph,
    file_name: &str,
    span: &Span,
    symbol: &'a SymbolType,
    _generics: &Vec<ExpandedType>,
    method_name: &str,
    args: &Vec<ExpandedType>,
) -> (String, Option<&'a SymbolFunction>) {
    let mut expects = Vec::new();
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

                let param_type = ExpandedType::from_string(graph, file_name, param.1.as_str());

                if !arg.is_compatible_with(&param_type) {
                    compatible = false;
                    // _context.alerts.push(SpannedAlert::error(
                    //     "Invalid argument type".to_string(),
                    //     format!("Expected type '{}' but got '{}'", param.1, arg.to_string()),
                    //     Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    // ));
                    expects.push(param.1.clone());
                }
            }

            if compatible {
                let mut return_type = method
                    .1
                    .return_type
                    .clone()
                    .unwrap_or("void".to_owned())
                    .clone();
                if return_type.starts_with("!") {
                    return_type = _generics[return_type[1..].parse::<usize>().unwrap()].to_string();
                }
                return (return_type, Some(&method.1));
            }
        }
    }

    if expects.len() > 0 {
        if method_name.starts_with("__operator_") {
            _context.alerts.push(SpannedAlert::error(
                "Invalid operand type".to_string(),
                format!(
                    "Expected right-hand type '{}' but got '{}'",
                    expects.join(" or "),
                    args[1].to_string()
                ),
                Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
            ));
            return ("!error".to_owned(), None);
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
    } else if method_name.starts_with("__postfix_operator_") {
        _context.alerts.push(SpannedAlert::error(
            "Undefined unary operator".to_string(),
            format!(
                "No operator '{}' was found for type '{}'",
                method_name.replace("__postfix_operator_", ""),
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

    return ("!error".to_string(), None);
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
    symbol_expanded: &ExpandedType,
    method_name: &str,
    args_base: &Vec<ExpandedType>,
    span: &Span,
) -> Option<(&'a str, &'a SymbolFunction)> {
    let mut args = Vec::new();
    args.extend_from_slice(args_base);
    let mut missed = Vec::new();
    for method in &symbol.methods {
        if method.0 == method_name
            || (symbol_expanded.name == method_name && method.0 == "__construct")
        {
            let mut compatible = true;
            if args.len() > method.1.parameters.len() {
                compatible = false;
            }

            let mut expected = Vec::new();

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
                let expanded_raw = ExpandedType::from_string(graph, file_name, param.1.as_str());
                let param_type = if param.1.starts_with("!") {
                    let index = (param.1[1..]).parse::<usize>().unwrap();
                    symbol_expanded.generics.get(index).unwrap()
                } else {
                    &expanded_raw
                };

                if i > 0 {
                    expected.push(param_type.to_string());
                }

                if !arg.is_compatible_with(param_type) {
                    compatible = false;
                }
            }

            if compatible {
                return Some((&method.0, &method.1));
            } else {
                missed.push((method, expected));
            }
        }
    }

    if (missed.len() == 1) {
        let method = &missed[0];
        let expected = &method.1;
        let got = args
            .iter()
            .skip(1)
            .map(|x| x.to_string())
            .collect::<Vec<String>>();
        _context.alerts.push(SpannedAlert::error(
            "Argument mismatch".to_string(),
            format!(
                "'{}::{}' expected ({}), but got ({})",
                symbol_expanded.name,
                method.0 .0,
                expected.join(", "),
                got.join(", ")
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    } else if missed.len() > 1 {
        let method = &missed[0];
        let got = args.iter().map(|x| x.to_string()).collect::<Vec<String>>();
        _context.alerts.push(SpannedAlert::error(
            "No overload".to_string(),
            format!(
                "'{}::{}' has no overload that matches the given arguments {}",
                symbol_expanded.name,
                method.0 .0,
                got.join(", ")
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    } else {
        _context.alerts.push(SpannedAlert::error(
            "No method".to_string(),
            format!(
                "'{}' has no method named: '{}'",
                symbol_expanded.name, method_name,
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    }

    None
}

pub fn match_function<'a>(
    _context: &mut ValidationContext,
    scope: &Scope,
    graph: &'a SymbolGraph,
    file_name: &str,
    intermediate: &mut TypedIntermediate,
    func_name: &str,
    args: &Vec<ExpandedType>,
) -> Option<(String, &'a SymbolFunction)> {
    let syms = graph
        .files
        .get(file_name)
        .unwrap()
        .iter()
        .map(|x| (x.0, vec![x.1]));
    let all_syms = graph
        .primitive
        .iter_all()
        .map(|x| (x.0, x.1.iter().map(|x| x).collect()));
    for func in syms.chain(all_syms) {
        let mut matching_overload = None;
        let mut partial_overload = None;
        let mut exact_overload = None;
        for overload in func.1 {
            if let SymbolDefinition::Function(ref func_def) = overload.definition {
                if func.0 == func_name {
                    let mut compatible = true;
                    let mut partial_compatible = false;
                    let mut exact_compatible = true;
                    if args.len() > func_def.parameters.len() {
                        compatible = false;
                    }

                    for (i, param) in func_def.parameters.iter().enumerate() {
                        if i >= args.len() {
                            if param.2 {
                                continue;
                            } else {
                                compatible = false;
                                break;
                            }
                        }

                        let arg = &args[i];
                        let param_type =
                            ExpandedType::from_string(graph, file_name, param.1.as_str());

                        if !arg.is_compatible_with(&param_type) {
                            compatible = false;
                        }

                        if arg.name != param_type.name {
                            exact_compatible = false;
                        } else {
                            partial_compatible = true;
                        }
                    }

                    if compatible {
                        matching_overload = Some((overload.get_namespaced(), func_def));
                    }

                    if exact_compatible {
                        exact_overload = Some((overload.get_namespaced(), func_def));
                    }

                    if partial_compatible {
                        partial_overload = Some((overload.get_namespaced(), func_def));
                    }
                }
            }
        }

        if let Some(overload) = exact_overload {
            return Some((overload.0, overload.1));
        }

        if let Some(overload) = partial_overload {
            return Some((overload.0, overload.1));
        }

        if let Some(overload) = matching_overload {
            return Some((overload.0, overload.1));
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

        let mut param_types = String::new();

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

            param_types.push_str(&format!("_{}", param.to_safe_string()));

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

        let mut func_name = match &typed_expr {
            TypedExpression::Identifier(name, _) => name.clone(),
            _ => "!error".to_string(),
        };

        (
            out_type,
            TypedExpression::Call(
                format!("{}{}", func_name, param_types),
                typed_args,
                call.span.clone(),
            ),
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
                args.push(ExpandedType::from_string(
                    graph,
                    file_name,
                    &self_arg_type_string,
                ));

                for arg in &call.args {
                    let (arg_type_string, typed_arg) =
                        get_type_local(_context, scope, graph, file_name, intermediate, &arg);
                    let arg_type = ExpandedType::from_string(graph, file_name, &arg_type_string);
                    typed_args.push(typed_arg);
                    args.push(arg_type);
                }

                if let Some((real_method_name, method_symbol)) = match_method(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    symbol,
                    &func_type,
                    &method_name.name,
                    &args,
                    &call.span,
                ) {
                    if real_method_name == "__construct" {
                        typed_args.remove(0);
                    }

                    (
                        method_symbol
                            .return_type
                            .as_ref()
                            .unwrap_or(&"void".to_string())
                            .clone(),
                        TypedExpression::Call(
                            format!(
                                "{}_method_{}{}",
                                func_symbol.unwrap().get_namespaced(),
                                real_method_name,
                                method_symbol.get_overload_name()
                            ),
                            typed_args
                                .into_iter()
                                .enumerate()
                                .map(|(i, x)| {
                                    let param_type_name = &method_symbol.parameters[i].1;
                                    let expanded_raw = ExpandedType::from_string(
                                        graph,
                                        file_name,
                                        param_type_name,
                                    );
                                    let param_type = if param_type_name.starts_with("!") {
                                        let index =
                                            (param_type_name[1..]).parse::<usize>().unwrap();
                                        func_type.generics.get(index).unwrap()
                                    } else {
                                        &expanded_raw
                                    };
                                    param_type.wrap_cast(&args[i], x)
                                })
                                .collect(),
                            call.span.clone(),
                        ),
                    )
                } else {
                    // Error reporting is handled in the match_method function
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
            if ident.name == "g" {
                dbg!(&ident);
            }
            if let Some((local_type, broke_barrier)) =
                scope.check_with_shader_barrier(&ident.name, false)
            {
                add_variable_hint(_context, &ident.name, &local_type, &ident.span);

                let scope_def = scope.get(&ident.name).unwrap();

                (
                    local_type,
                    TypedExpression::Identifier(
                        if scope_def.is_global {
                            format!(
                                "${}_{}",
                                crate::graph::translate_path_to_safe_name(file_name),
                                ident.name
                            )
                        } else {
                            ident.name.clone()
                        },
                        ident.span.clone(),
                    ),
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
            if let ast::Expression::Error((_, _)) = &*call.expression {
                let method_unwrap = call.method.as_ref().unwrap();
                let mut arg_types = Vec::new();
                let mut arg_exprs = Vec::new();
                for i in 0..call.args.len() {
                    let (arg_type_string, typed_arg) = get_type_local(
                        _context,
                        scope,
                        graph,
                        file_name,
                        intermediate,
                        &call.args[i],
                    );

                    arg_types.push(ExpandedType::from_string(
                        graph,
                        file_name,
                        &arg_type_string,
                    ));

                    arg_exprs.push(typed_arg);
                }

                let matched_func = match_function(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    &method_unwrap.name,
                    &arg_types,
                );
                if let Some(matched_func) = matched_func {
                    let mut casted_args = Vec::new();
                    for i in 0..matched_func.1.parameters.len() {
                        let expanded = ExpandedType::from_string(
                            graph,
                            file_name,
                            &matched_func.1.parameters[i].1,
                        );
                        if i < arg_types.len() {
                            casted_args
                                .push(expanded.wrap_cast(&arg_types[i], arg_exprs[i].clone()));
                        }
                    }
                    return (
                        matched_func
                            .1
                            .return_type
                            .clone()
                            .unwrap_or("void".to_owned()),
                        TypedExpression::Call(
                            format!("{}{}", matched_func.0, matched_func.1.get_overload_name()),
                            casted_args,
                            call.span.clone(),
                        ),
                    );
                }
            }

            let (func_type, typed_expr) = get_type_local(
                _context,
                scope,
                graph,
                file_name,
                intermediate,
                &matched_expr,
            );
            let func_type = ExpandedType::from_string(graph, file_name, &func_type);
            let func_symbol = graph.get_symbol_node_in_file(file_name, &func_type.name);
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
            new_scope.definitions.insert(
                "pixel".to_string(),
                ScopeVariableDefinition::new("float4".to_string(), 0..0),
            );
            new_scope.definitions.insert(
                "context".to_string(),
                ScopeVariableDefinition::new("ShaderContext".to_string(), 0..0),
            );
            new_scope.definitions.insert(
                "vertex".to_string(),
                ScopeVariableDefinition::new("ShaderVertexOutput".to_string(), 0..0),
            );
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
                globals: HashMap::new(),
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
                true,
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
        ast::Expression::StructInstance((ident, args, struct_span)) => {
            let (checked_type, _) = check_type_local(_context, graph, file_name, ident);
            if checked_type.is_some() {
                let symbol = graph
                    .files
                    .get(file_name)
                    .unwrap()
                    .get(&ident.name)
                    .unwrap();

                if let SymbolDefinition::Type(ref _type) = symbol.definition {
                    for arg in args {
                        let field = _type.fields.iter().find(|field| field.0 == arg.0.name);
                        if field.is_none() {
                            _context.alerts.push(SpannedAlert::error(
                                "Invalid type".to_string(),
                                format!(
                                    "Field {} does not exist on type {}",
                                    arg.0.name, ident.name
                                ),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(arg.0.span.start, arg.0.span.end),
                                ),
                            ));
                        } else {
                            let field_type =
                                ExpandedType::from_string(graph, file_name, &field.unwrap().1);

                            let (arg_type, arg_typed) = get_type_local(
                                _context,
                                scope,
                                graph,
                                file_name,
                                intermediate,
                                &arg.1,
                            );

                            if !field_type.is_compatible_with(&ExpandedType::from_string(
                                graph, file_name, &arg_type,
                            )) {
                                _context.alerts.push(SpannedAlert::error(
                                    "Invalid type".to_string(),
                                    format!(
                                        "Field {} is of type {} but was given {}",
                                        arg.0.name, field_type.name, arg_type
                                    ),
                                    Location::new(
                                        file_name.to_string(),
                                        USizeTuple(arg.0.span.start, arg.0.span.end),
                                    ),
                                ));
                            }
                        }
                    }
                    (
                        ident.name.clone(),
                        TypedExpression::Call(
                            format!("__make_struct_{}", symbol.get_namespaced()),
                            vec![TypedExpression::KVMap(
                                args.iter()
                                    .map(|arg| {
                                        if let Some(field) =
                                            _type.fields.iter().find(|field| field.0 == arg.0.name)
                                        {
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
                                                ExpandedType::from_string(
                                                    graph, file_name, &field.1,
                                                )
                                                .wrap_cast(
                                                    &ExpandedType::from_string(
                                                        graph, file_name, &expr_type,
                                                    ),
                                                    arg_typed,
                                                ),
                                            )
                                        } else {
                                            ("error".to_owned(), TypedExpression::Error())
                                        }
                                    })
                                    .collect(),
                                ident.span.clone(),
                            )],
                            ident.span.clone(),
                        ),
                    )
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        "Invalid type".to_string(),
                        format!("Expected type, got something else"),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(struct_span.start, struct_span.end),
                        ),
                    ));
                    ("!error".to_owned(), TypedExpression::Error())
                }
            } else {
                _context.alerts.push(SpannedAlert::error(
                    "Undefined type".to_string(),
                    format!("Type {} is not defined", ident.name),
                    Location::new(
                        file_name.to_string(),
                        USizeTuple(ident.span.start, ident.span.end),
                    ),
                ));
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
                .0
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
                let (rhs_symbol, _) = check_type_local(
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
                    let (method_op, method_symbol) = check_method_local(
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

                    let mut rhs_type_name = format!("primitives_{}", rhs_type_after.name.clone());

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
                                "{}_method___prefix_operator_{}{}",
                                rhs_type_name,
                                op.get_code_name(),
                                if method_symbol.is_some() {
                                    method_symbol.unwrap().get_overload_name()
                                } else {
                                    "".to_string()
                                }
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
                    if let ast::Expression::Error((_, _)) = rhs.as_ref() {
                        let mut params = vec![lhs_type];
                        let (method_op, method_symbol) = check_method_local(
                            _context,
                            graph,
                            file_name,
                            span,
                            lhs_symbol,
                            &params[0].generics,
                            &format!("__postfix_operator_{}", op.get_code_name()),
                            &params,
                        );

                        let lhs_type_after = params.remove(0);

                        let mut lhs_type_name = format!("primitives_{}", lhs_type_after.name);

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
                                format!(
                                    "{}_method___postfix_operator_{}{}",
                                    lhs_type_name,
                                    op.get_code_name(),
                                    if method_symbol.is_some() {
                                        method_symbol.unwrap().get_overload_name()
                                    } else {
                                        "".to_string()
                                    }
                                ),
                                vec![lhs_typed_expr, rhs_typed_expr],
                                span.clone(),
                            ),
                        )
                    } else {
                        let mut params = vec![lhs_type, rhs_type];
                        let (method_op, method_symbol) = check_method_local(
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
                        let rhs_type_after = params.remove(0);

                        let mut lhs_type_name = format!("primitives_{}", lhs_type_after.name);

                        if let Some(lhs_symbol_node) = graph
                            .files
                            .get(file_name)
                            .unwrap()
                            .get(&lhs_type_after.name)
                        {
                            lhs_type_name = lhs_symbol_node.get_namespaced();
                        }

                        let cast_to_rhs;

                        if let Some(method_symbol) = method_symbol {
                            cast_to_rhs = ExpandedType::from_string(
                                graph,
                                file_name,
                                &method_symbol.parameters[1].1,
                            );
                        } else {
                            cast_to_rhs = rhs_type_after.clone();
                        }

                        let rhs_typed_expr_casted =
                            cast_to_rhs.wrap_cast(&rhs_type_after, rhs_typed_expr);
                        (
                            method_op.to_owned(),
                            TypedExpression::Call(
                                format!(
                                    "{}_method___operator_{}{}",
                                    lhs_type_name,
                                    op.get_code_name(),
                                    if method_symbol.is_some() {
                                        method_symbol.unwrap().get_overload_name()
                                    } else {
                                        "".to_string()
                                    }
                                ),
                                vec![lhs_typed_expr, rhs_typed_expr_casted],
                                span.clone(),
                            ),
                        )
                    }
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
        Expression::Index((expr, index_expr, span)) => {
            let (expr_type, typed_expr) =
                get_type_local(_context, scope, graph, file_name, intermediate, expr);

            let (index_expr_type, index_typed_expr) =
                get_type_local(_context, scope, graph, file_name, intermediate, index_expr);

            let expr_type = ExpandedType::from_string(graph, file_name, &expr_type);
            let index_expr_type = ExpandedType::from_string(graph, file_name, &index_expr_type);

            let (expr_symbol, expr_node) = check_type_local(
                _context,
                graph,
                file_name,
                &ast::Identifier {
                    name: expr_type.name.clone(),
                    span: expr.get_span(),
                },
            );

            if let Some(expr_symbol) = expr_symbol {
                let mut params = vec![expr_type, index_expr_type];
                let (method_op, method_symbol) = check_method_local(
                    _context,
                    graph,
                    file_name,
                    span,
                    expr_symbol,
                    &params[0].generics,
                    &"__index",
                    &params,
                );

                (
                    method_op.to_owned(),
                    TypedExpression::Call(
                        format!(
                            "{}_method___index{}",
                            expr_node.unwrap().get_namespaced(),
                            if method_symbol.is_some() {
                                method_symbol.unwrap().get_overload_name()
                            } else {
                                "".to_string()
                            }
                        ),
                        vec![typed_expr, index_typed_expr],
                        span.clone(),
                    ),
                )
            } else {
                ("!error".to_string(), TypedExpression::Error())
            }
        }

        Expression::List((exprs, span)) => {
            if exprs.len() == 0 {
                _context.alerts.push(SpannedAlert::error(
                    "Undefined vector".to_string(),
                    format!("here"),
                    Location::new(
                        file_name.to_string(),
                        USizeTuple(expr.get_span().start, expr.get_span().end),
                    ),
                ));
                ("!error".to_string(), TypedExpression::Error())
            } else if exprs.len() == 1 {
                let (expr_type, typed_expr) =
                    get_type_local(_context, scope, graph, file_name, intermediate, &exprs[0]);
                (
                    format!("array<{}>", expr_type),
                    TypedExpression::List(vec![typed_expr], span.clone()),
                )
            } else {
                let mut types = vec![];
                let mut typed_exprs = vec![];
                let mut type_name = "".to_string();
                for expr in exprs {
                    let (expr_type, typed_expr) =
                        get_type_local(_context, scope, graph, file_name, intermediate, expr);
                    types.push(expr_type.clone());
                    typed_exprs.push(typed_expr);
                    if type_name == "" {
                        type_name = expr_type.clone();
                    } else {
                        if expr_type != type_name {
                            _context.alerts.push(SpannedAlert::error(
                                "Inconsistent types".to_string(),
                                format!("The types of the elements in the array are inconsistent"),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(expr.get_span().start, expr.get_span().end),
                                ),
                            ));
                            return ("!error".to_string(), TypedExpression::Error());
                        }
                    }
                }
                (
                    format!("array<{}>", type_name),
                    TypedExpression::List(typed_exprs, span.clone()),
                )
            }
        }
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
                        let mut high_type = types[0].name.clone();
                        for _type in &types {
                            if !_type.is_scalar() {
                                _context.alerts.push(SpannedAlert::error(
                                    "Non scalar vector element".to_string(),
                                    format!("got: '{}'", _type.name),
                                    Location::new(
                                        file_name.to_string(),
                                        USizeTuple(expr.get_span().start, expr.get_span().end),
                                    ),
                                ));
                                return ("!error".to_string(), TypedExpression::Error());
                            }

                            if high_type == "int" {
                                if _type.name == "float" {
                                    high_type = "float".to_string();
                                } else if _type.name == "uint" {
                                    high_type = "uint".to_string();
                                }
                            }
                        }

                        for i in 0..types.len() {
                            if types[i].name != high_type {
                                let old_type = types[i].clone();
                                types[i] = ExpandedType::from_string(graph, file_name, &high_type);
                                typed_exprs[i] =
                                    types[i].wrap_cast(&old_type, typed_exprs[i].clone());
                            }
                        }
                        // if types[0].name != types[types.len() - 1].name {
                        //     _context.alerts.push(SpannedAlert::error(
                        //         "Illegal type mixing in vector".to_string(),
                        //         format!(
                        //             "got: '{}' and '{}'",
                        //             types[0].name,
                        //             types[types.len() - 1].name
                        //         ),
                        //         Location::new(
                        //             file_name.to_string(),
                        //             USizeTuple(span.start, span.end),
                        //         ),
                        //     ));
                        //     return ("!error".to_string(), TypedExpression::Error());
                        // }
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
                        format!(
                            "primitives_{}{}_method___make_vec",
                            types[0].name,
                            types.len(),
                        ),
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

fn webgl_index(i: usize) -> String {
    match i {
        0 => "x".to_string(),
        1 => "y".to_string(),
        2 => "z".to_string(),
        3 => "w".to_string(),
        _ => panic!("Invalid index"),
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
        let return_type = format!("{}{}", name, if to_len == 1 { "" } else { &tl });

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
            return_type: ExpandedType::from_string(graph, file_name, &return_type),
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
            webgl: if to_len == 1 {
                Some(format!("return a.{};", webgl_index(nums[0])))
            } else {
                Some(format!(
                    "return {}({});",
                    crate::webgl::translate_type(&return_type),
                    (0..to_len)
                        .map(|i| format!("a.{}", webgl_index(nums[i])))
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
        let return_type = format!("{}{}", name, len);
        let func = TypedFunction {
            name: func_name.clone(),
            parameters: vec![TypedFunctionParameter {
                name: "a".to_owned(),
                type_name: ExpandedType::from_string(graph, file_name, name.clone()),
                default_value: None,
            }],
            return_type: ExpandedType::from_string(graph, file_name, &return_type),
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
            webgl: Some(format!(
                "return {}({});",
                crate::webgl::translate_type(&return_type),
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

            // We don't need to do this as we're now relying on tags to propagate global var access
            // if let Some(func) = intermediate.functions.get(func_name) {
            //     build_shader_definition(
            //         _context,
            //         graph,
            //         scope,
            //         shader_def,
            //         file_name,
            //         intermediate,
            //         &func.body,
            //         false,
            //     );
            // }

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
        TypedExpression::List(exprs, _) => {
            for expr in exprs {
                build_shader_expression(
                    _context,
                    graph,
                    scope,
                    shader_def,
                    file_name,
                    intermediate,
                    expr,
                );
            }
        }
        TypedExpression::Value(_, _) => {}
        TypedExpression::Identifier(ident, _) => {
            if let Some((scoped_var, barrier)) = scope.check_with_shader_barrier(ident, false) {
                let scope_def = scope.get(ident).unwrap();

                if barrier {
                    if scope_def.is_global {
                        shader_def
                            .parameters
                            .insert(format!("${}", ident), scoped_var.clone());
                    } else {
                        shader_def
                            .parameters
                            .insert(ident.clone(), scoped_var.clone());
                    }
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
fn build_shader_statement(
    _context: &mut ValidationContext,
    graph: &SymbolGraph,
    new_scope: &Scope,
    shader_def: &mut TypedShaderDefinition,
    file_name: &str,
    intermediate: &TypedIntermediate,
    statement: &TypedStatement,
    root_shader: bool,
) {
    match statement {
        TypedStatement::Set(id, expr, span) => {
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
        TypedStatement::Return(expr, span) => {
            if (root_shader) {
                _context.alerts.push(SpannedAlert::error(
                    "Return statement not allowed in shader".to_string(),
                    "Return statements are only allowed in functions".to_string(),
                    Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                ))
            } else {
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
                root_shader,
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
                    root_shader,
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
                    root_shader,
                );
            }
        }
        TypedStatement::Let {
            name: _,
            value,
            type_name,
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
        TypedStatement::Break(_) => {}
        TypedStatement::Continue(_) => {}
        TypedStatement::For {
            init,
            condition,
            update,
            body,
            span,
        } => {
            build_shader_statement(
                _context,
                graph,
                new_scope,
                shader_def,
                file_name,
                intermediate,
                &*init,
                root_shader,
            );
            build_shader_expression(
                _context,
                graph,
                new_scope,
                shader_def,
                file_name,
                intermediate,
                condition,
            );
            build_shader_expression(
                _context,
                graph,
                new_scope,
                shader_def,
                file_name,
                intermediate,
                update,
            );
            build_shader_definition(
                _context,
                graph,
                new_scope,
                shader_def,
                file_name,
                intermediate,
                body,
                root_shader,
            );
        }
        TypedStatement::While {
            condition,
            body,
            span,
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
                root_shader,
            );
        }

        TypedStatement::ForEach {
            value,
            key,
            iterator,
            body,
            span,
        } => {
            build_shader_expression(
                _context,
                graph,
                new_scope,
                shader_def,
                file_name,
                intermediate,
                iterator,
            );
            build_shader_definition(
                _context,
                graph,
                new_scope,
                shader_def,
                file_name,
                intermediate,
                body,
                root_shader,
            );
        }
    }
}
fn build_shader_definition(
    _context: &mut ValidationContext,
    graph: &SymbolGraph,
    new_scope: &Scope,
    shader_def: &mut TypedShaderDefinition,
    file_name: &str,
    intermediate: &TypedIntermediate,
    typed_body: &TypedBody,
    root_shader: bool,
) {
    for statement in &typed_body.statements {
        build_shader_statement(
            _context,
            graph,
            new_scope,
            shader_def,
            file_name,
            intermediate,
            statement,
            root_shader,
        );
    }
}

#[derive(Clone)]
pub struct ExpandedType {
    pub name: String,
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
                    format!(
                        "primitives_{}_method___cast_from_scalar_{}",
                        self.name, self.name
                    ),
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
                    format!(
                        "primitives_{}_method___cast_from_vec_{}",
                        self.name, self.name
                    ),
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

    pub fn from_string_raw(string: &str) -> Self {
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
                    generics.push(ExpandedType::from_string_raw(&generic));
                    generic = String::new();
                    continue;
                }
            } else if c == ',' && depth == 1 {
                generics.push(ExpandedType::from_string_raw(&generic));
                generic = String::new();
                continue;
            }

            if depth == 0 {
                name.push(c);
            } else {
                generic.push(c);
            }
        }

        Self {
            name: name.clone(),
            generics,
            symbol_type: None,
        }
    }

    pub fn to_safe_string(&self) -> String {
        let mut string = self.name.clone();

        if self.generics.len() > 0 {
            string.push_str("___");

            for (i, generic) in self.generics.iter().enumerate() {
                if i > 0 {
                    string.push_str("__");
                }

                string.push_str(generic.to_string().as_str());
            }

            string.push_str("___");
        }

        string
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
                        _to_val.clone()
                    };
                    scope.definitions.insert(
                        _let.name.name.clone(),
                        ScopeVariableDefinition::new(let_type.clone(), _let.span.clone()),
                    );

                    let set_to_expanded = ExpandedType::from_string(graph, file_name, &let_type);
                    let to_val_clone = _to_val.clone();
                    let expanded_type = ExpandedType::from_string(graph, file_name, &to_val_clone);

                    if !set_to_expanded.is_compatible_with(&expanded_type) {
                        _context.alerts.push(SpannedAlert::error(
                            "Type mismatch".to_string(),
                            format!(
                                "Cannot assign '{}' to '{}'",
                                set_to_expanded.to_string(),
                                expanded_type.to_string()
                            ),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(_let.span.start, _let.span.end),
                            ),
                        ));
                    }
                    typed_body.statements.push(TypedStatement::Let {
                        name: _let.name.name.clone(),
                        type_name: set_to_expanded.clone(),
                        value: expanded_type.wrap_cast(&set_to_expanded, _to_typed_expr),
                        span: _let.span.clone(),
                    });
                }
            }
            ast::Root::Expression(expr) => {
                match expr {
                    Expression::Op((a, op, b, span)) => match op {
                        Op::Eq => match a.as_ref() {
                            Expression::Identifier((id, _)) => {
                                if let Some(container_type) = scope.check(&id.name) {
                                    let expanded_type = ExpandedType::from_string(
                                        graph,
                                        file_name,
                                        &container_type,
                                    );
                                    let (set_to_type, typed_expr) = get_type_local(
                                        _context,
                                        scope,
                                        graph,
                                        file_name,
                                        intermediate,
                                        b,
                                    );

                                    let set_to_expanded =
                                        ExpandedType::from_string(graph, file_name, &set_to_type);

                                    if !set_to_expanded.is_compatible_with(&expanded_type) {
                                        _context.alerts.push(SpannedAlert::error(
                                            "Type mismatch".to_string(),
                                            format!(
                                                "Cannot assign '{}' to '{}'",
                                                set_to_expanded.to_string(),
                                                expanded_type.to_string()
                                            ),
                                            Location::new(
                                                file_name.to_string(),
                                                USizeTuple(span.start, span.end),
                                            ),
                                        ));
                                    }

                                    let scope_def = scope.get(&id.name).unwrap();
                                    let id_real_name = if scope_def.is_global {
                                        format!(
                                            "${}_{}",
                                            crate::graph::translate_path_to_safe_name(file_name),
                                            id.name
                                        ) //We need to handle sets in shaders
                                    } else {
                                        id.name.clone()
                                    };

                                    typed_body.statements.push(TypedStatement::Set(
                                        id_real_name,
                                        expanded_type.wrap_cast(&set_to_expanded, typed_expr),
                                        span.clone(),
                                    ));
                                    continue;
                                } else {
                                    _context.alerts.push(SpannedAlert::error(
                                        "Undefined variable".to_string(),
                                        format!("'{}' is not defined in this scope", id.name),
                                        Location::new(
                                            file_name.to_string(),
                                            USizeTuple(id.span.start, id.span.end),
                                        ),
                                    ));
                                }
                            }
                            Expression::Index((left, index_expr, span)) => {
                                let (expr_type, typed_expr) = get_type_local(
                                    _context,
                                    scope,
                                    graph,
                                    file_name,
                                    intermediate,
                                    left,
                                );

                                let (index_expr_type, index_typed_expr) = get_type_local(
                                    _context,
                                    scope,
                                    graph,
                                    file_name,
                                    intermediate,
                                    index_expr,
                                );

                                let (set_to_expr_type, set_to_typed_expr) = get_type_local(
                                    _context,
                                    scope,
                                    graph,
                                    file_name,
                                    intermediate,
                                    b,
                                );

                                let expr_type =
                                    ExpandedType::from_string(graph, file_name, &expr_type);
                                let index_expr_type =
                                    ExpandedType::from_string(graph, file_name, &index_expr_type);
                                let set_to_expr_type =
                                    ExpandedType::from_string(graph, file_name, &set_to_expr_type);

                                let (expr_symbol, expr_node) = check_type_local(
                                    _context,
                                    graph,
                                    file_name,
                                    &ast::Identifier {
                                        name: expr_type.name.clone(),
                                        span: expr.get_span(),
                                    },
                                );

                                if let Some(expr_symbol) = expr_symbol {
                                    let mut params =
                                        vec![expr_type.clone(), index_expr_type, set_to_expr_type];
                                    // let (method_op, method_symbol) = check_method_local(
                                    //     _context,
                                    //     graph,
                                    //     file_name,
                                    //     &expr.get_span(),
                                    //     expr_symbol,
                                    //     &params[0].generics,
                                    //     &"__index_set",
                                    //     &params,
                                    // );

                                    if let Some((real_method_name, method_symbol)) = match_method(
                                        _context,
                                        scope,
                                        graph,
                                        file_name,
                                        intermediate,
                                        expr_symbol,
                                        &expr_type,
                                        &"__index_set",
                                        &params,
                                        &span,
                                    ) {
                                        typed_body.statements.push(TypedStatement::Expression(
                                            TypedExpression::Call(
                                                format!(
                                                    "{}_method___index_set{}",
                                                    expr_node.unwrap().get_namespaced(),
                                                    method_symbol.get_overload_name()
                                                ),
                                                vec![
                                                    typed_expr,
                                                    index_typed_expr,
                                                    set_to_typed_expr,
                                                ],
                                                span.clone(),
                                            ),
                                            span.clone(),
                                        ));
                                        continue;
                                    }
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => (),
                }
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
            ast::Root::Continue(span) => {
                typed_body
                    .statements
                    .push(TypedStatement::Continue(span.clone()));
            }
            ast::Root::Break(span) => {
                typed_body
                    .statements
                    .push(TypedStatement::Break(span.clone()));
            }
            ast::Root::While(_while) => {
                let (_type, _cond_typed) = get_type_local(
                    _context,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    &_while.condition,
                );

                if _type != "bool" {
                    _context.alerts.push(SpannedAlert::error(
                        "Expected bool-like expression in while".to_string(),
                        format!("got: '{}'", _type),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(
                                _while.condition.get_span().start,
                                _while.condition.get_span().end,
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
                    &_while.body.roots,
                    output,
                );

                typed_body.statements.push(TypedStatement::While {
                    condition: _cond_typed,
                    body: _typed_body,
                    span: _while.span.clone(),
                });
            }

            ast::Root::For(_for) => {
                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(scope));

                let mut my_typed_body = check_body_local(
                    _context,
                    &mut new_scope,
                    graph,
                    file_name,
                    intermediate,
                    &vec![*_for.first.clone()],
                    output,
                );

                let statement = my_typed_body.statements.pop().unwrap();

                let (_type, typed_expr) = get_type_local(
                    _context,
                    &mut new_scope,
                    graph,
                    file_name,
                    intermediate,
                    &_for.second,
                );
                if _type != "bool" {
                    _context.alerts.push(SpannedAlert::error(
                        "Expected bool-like expression in for".to_string(),
                        format!("got: '{}'", _type),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(_for.second.get_span().start, _for.second.get_span().end),
                        ),
                    ));
                }

                let _typed_cond = typed_expr;

                let (_type, typed_expr) = get_type_local(
                    _context,
                    &mut new_scope,
                    graph,
                    file_name,
                    intermediate,
                    &_for.third,
                );

                let _typed_body = check_body_local(
                    _context,
                    &mut new_scope,
                    graph,
                    file_name,
                    intermediate,
                    &_for.body.roots,
                    output,
                );

                typed_body.statements.push(TypedStatement::For {
                    init: Box::new(statement),
                    condition: _typed_cond,
                    update: typed_expr,
                    body: _typed_body,
                    span: _for.span.clone(),
                });
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
        globals: HashMap::new(),
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

    let mut global_scope = Scope::new();

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
        func_name: &str,
        function: &ast::Function,
        parent: Option<&str>,
        global_scope: &mut Scope,
    ) -> TypedFunction {
        let mut real_params = Vec::new();
        for (i, param) in function.parameters.iter().enumerate() {
            if let Some(ref param_type) = param.1 {
                if param.0.name == "self" {
                    _context.alerts.push(SpannedAlert::error(
                        format!("No type required"),
                        format!("Self shouldn't have a type included '{}'", param_type.name),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(param_type.span.start, param_type.span.end),
                        ),
                    ));
                }
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

                real_params.push((param.0.name.clone(), param_type.name.clone()));
                check_type_local(_context, graph, file_name, &param_type);
            } else {
                if param.0.name == "self" {
                    if parent.is_none() {
                        _context.alerts.push(SpannedAlert::error(
                            format!("Self not allowed"),
                            format!("Self is only allowed in method definitions"),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(param.0.span.start, param.0.span.end),
                            ),
                        ));
                    } else {
                        let parent = parent.unwrap();

                        real_params.push((param.0.name.clone(), parent.to_string()));
                    }
                } else {
                    _context.alerts.push(SpannedAlert::error(
                        format!("Missing type"),
                        format!("Missing type for parameter '{}'", param.0.name),
                        Location::new(
                            file_name.to_string(),
                            USizeTuple(param.0.span.start, param.0.span.end),
                        ),
                    ));
                }
            }
        }

        let output = match function.return_type {
            Some(ref _type) => ExpandedType::from_string(graph, file_name, &_type.name),
            None => ExpandedType::from_string(graph, file_name, "void"),
        };

        let mut scope = Scope::new();
        let typed_body;

        if !func_name.ends_with("__init_file") {
            scope.parent = Some(Box::new(global_scope));

            for param in real_params {
                scope.definitions.insert(
                    param.0,
                    ScopeVariableDefinition::new(param.1, function.span.clone()),
                );
            }

            typed_body = check_body_local(
                _context,
                &mut scope,
                &graph,
                file_name,
                intermediate,
                &function.body.roots,
                &output,
            );
        } else {
            for param in real_params {
                scope.definitions.insert(
                    param.0,
                    ScopeVariableDefinition::new(param.1, function.span.clone()),
                );
            }

            typed_body = check_body_local(
                _context,
                &mut scope,
                &graph,
                file_name,
                intermediate,
                &function.body.roots,
                &output,
            );

            // Little hacky but whatever
            global_scope.definitions = scope.definitions.clone();
        }

        let mut func_name_overload = String::new();

        for param in &function.parameters {
            let expanded =
                ExpandedType::from_string(graph, file_name, &param.1.as_ref().unwrap().name);
            func_name_overload.push_str(&format!("_{}", expanded.to_safe_string()));
        }
        let func_name_out = format!("{}{}", func_name, func_name_overload);

        TypedFunction {
            tagging: false,
            tagged: false,
            tags: vec![],
            javascript: None,
            webgl: None,
            name: func_name_out.to_string(),
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
                                    globals: HashMap::new(),
                                },
                                &expr,
                            )
                            .1,
                        ),
                        None => None,
                    },
                    type_name: ExpandedType::from_string(
                        graph,
                        file_name,
                        if _type.is_some() {
                            &_type.as_ref().unwrap().name
                        } else {
                            "void"
                        },
                    ),
                })
                .collect(),
            return_type: output,
            body: typed_body,
        }
    }

    // We want to validate the __init_file and pollute the global scope with the
    // definitions in it.
    for symbol in file.values() {
        if symbol.imported || !symbol.name.ends_with("__init_file") {
            continue;
        }

        match &symbol.root {
            ast::Root::Function(function) => {
                if let SymbolDefinition::Function(ref symbol_def) = symbol.definition {
                    let mut typed_func = add_function_local(
                        &mut context,
                        graph,
                        file_name,
                        &mut typed,
                        &symbol.get_namespaced(),
                        function,
                        symbol_def.method_of.as_ref().map(|s| s.as_str()),
                        &mut global_scope,
                    );

                    // We need to set window variable for this file to all the vars in the global scope
                    for (name, def) in global_scope.definitions.iter() {
                        typed_func.body.statements.push(TypedStatement::Set(
                            format!(
                                "${}_{}",
                                crate::graph::translate_path_to_safe_name(file_name),
                                name
                            ),
                            TypedExpression::Identifier(name.to_string(), 0..0),
                            0..0,
                        ));

                        typed.globals.insert(
                            format!(
                                "{}_{}",
                                crate::graph::translate_path_to_safe_name(file_name),
                                name
                            ),
                            def.type_name.clone(),
                        );
                    }

                    typed.functions.insert(typed_func.name.clone(), typed_func);
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }

    for mut def in global_scope.definitions.values_mut() {
        def.is_global = true;
    }

    let funcs_that_define_shaders = file
        .values()
        .filter(|s| match &s.root {
            ast::Root::Function(f) => {
                let mut has_shader = false;
                {
                    let mut walker = AstWalker::new();
                    let walker_fn = |block: &ast::Block| {
                        has_shader = true;
                    };
                    let boxed = Box::new(walker_fn);
                    walker.shader = Some(boxed);
                    f.walk(&mut walker);
                }
                has_shader
            }
            _ => false,
        })
        .collect::<Vec<&SymbolNode>>();

    let funcs_that_dont_define_shaders = file
        .values()
        .filter(|s| match &s.root {
            ast::Root::Function(f) => {
                let mut has_shader = false;
                {
                    let mut walker = AstWalker::new();
                    let walker_fn = |block: &ast::Block| {
                        has_shader = true;
                    };
                    let boxed = Box::new(walker_fn);
                    walker.shader = Some(boxed);
                    f.walk(&mut walker);
                }
                !has_shader
            }
            _ => true,
        })
        .collect::<Vec<&SymbolNode>>();

    for symbol in [funcs_that_dont_define_shaders, funcs_that_define_shaders].concat() {
        if symbol.imported || symbol.name.ends_with("__init_file") {
            continue;
        }

        match &symbol.root {
            ast::Root::Function(function) => {
                if let SymbolDefinition::Function(ref symbol_def) = symbol.definition {
                    dbg!(&symbol.name);
                    let typed_func = add_function_local(
                        &mut context,
                        graph,
                        file_name,
                        &mut typed,
                        &symbol.get_namespaced(),
                        function,
                        symbol_def.method_of.as_ref().map(|s| s.as_str()),
                        &mut global_scope,
                    );
                    typed.functions.insert(typed_func.name.clone(), typed_func);

                    add_function_hint(&mut context, &symbol, &symbol.root.get_def_span());
                } else {
                    unreachable!()
                }
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
                        webgl: None,
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
                            webgl: None,
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
                let base_symbol = graph
                    .get_symbol_node_in_file(file_name, &_impl.name.name)
                    .unwrap();

                let namespaced_type = base_symbol.get_namespaced();
                for method in &_impl.body {
                    match method {
                        ast::Root::Function(func) => {
                            let method_symbol =
                                if let SymbolDefinition::Type(ref _type) = base_symbol.definition {
                                    _type.methods.iter().find(|m| m.0 == func.name.name)
                                } else {
                                    None
                                };

                            if let Some(ref method_symbol) = method_symbol {
                                let func_name =
                                    format!("__{}_{}", namespaced_type, method_symbol.0);
                                let func_typed = add_function_local(
                                    &mut context,
                                    graph,
                                    file_name,
                                    &mut typed,
                                    &func_name,
                                    &func,
                                    Some(&base_symbol.name),
                                    &mut global_scope,
                                );
                                typed.functions.insert(func_name, func_typed);
                            }
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
    func2.tagged = true;
}

pub fn retag(func_name: &str, span: &Span, tags: &mut Vec<TypedTag>) -> Vec<TypedTag> {
    let mut new_tags = vec![];
    for tag in tags {
        new_tags.push(TypedTag {
            tag: tag.tag.clone(),
            span: span.clone(),
            name: func_name.to_owned(),
            type_name: tag.type_name.clone(),
            introduced_by: Some(Box::new(tag.clone())),
        });
    }
    new_tags
}

pub fn propagate_tags_from_statement(
    intermediate: &mut TypedIntermediate,
    statement: &TypedStatement,
) -> Vec<TypedTag> {
    let mut tags = vec![];
    match statement {
        TypedStatement::Set(_, expr, _) => {
            tags.append(&mut propagate_tags_from_expression(intermediate, expr))
        }
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
            type_name,
            value,
            span: _,
        } => {
            tags.append(&mut propagate_tags_from_expression(intermediate, value));
        }
        TypedStatement::Expression(expr, _) => {
            tags.append(&mut propagate_tags_from_expression(intermediate, expr));
        }
        TypedStatement::For {
            init,
            condition,
            update,
            body,
            span,
        } => {
            tags.append(&mut propagate_tags_from_statement(intermediate, init));
            tags.append(&mut propagate_tags_from_expression(intermediate, condition));
            tags.append(&mut propagate_tags_from_expression(intermediate, update));
            tags.append(&mut propagate_tags(intermediate, body));
        }
        TypedStatement::While {
            condition,
            body,
            span: _,
        } => {
            tags.append(&mut propagate_tags_from_expression(intermediate, condition));
            tags.append(&mut propagate_tags(intermediate, body));
        }
        TypedStatement::ForEach {
            value,
            key,
            iterator,
            body,
            span,
        } => {
            tags.append(&mut propagate_tags_from_expression(intermediate, iterator));
            tags.append(&mut propagate_tags(intermediate, body));
        }
        TypedStatement::Break(span) => {}
        TypedStatement::Continue(span) => {}
    }

    tags
}
pub fn propagate_tags(intermediate: &mut TypedIntermediate, body: &TypedBody) -> Vec<TypedTag> {
    let mut tags = vec![];

    for statement in body.statements.iter() {
        tags.append(&mut propagate_tags_from_statement(intermediate, statement));
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
        TypedExpression::Identifier(ident, span) => {
            if ident.starts_with("$") {
                tags.push(TypedTag {
                    tag: TypedTagType::GlobalVariableAccess,
                    span: span.clone(),
                    name: ident.to_owned(),
                    type_name: ident.replace("$", "").to_owned(),
                    introduced_by: None,
                });
            }
        }
        TypedExpression::KVMap(_, _) => {}
        TypedExpression::Shader(_, _) => {}
        TypedExpression::List(exprs, _) => {
            for expr in exprs {
                tags.append(&mut propagate_tags_from_expression(intermediate, expr));
            }
        }
        TypedExpression::Error() => {}
    }

    tags
}
