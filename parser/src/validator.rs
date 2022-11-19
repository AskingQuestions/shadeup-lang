use std::collections::HashMap;



use crate::ast::{self, Expression, Location, Op, Span, USizeTuple};
use crate::graph::{SymbolDefinition, SymbolGraph, SymbolType};
use crate::printer::SpannedAlert;

pub struct Scope<'a> {
    shader_barrier: bool,
    definitions: HashMap<String, String>,
    parent: Option<Box<&'a Scope<'a>>>,
}

#[derive(Debug, Clone)]
pub enum TypedValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
    Error,
}

#[derive(Debug, Clone)]
pub struct TypedShaderInstance {
    pub closure: HashMap<String, String>,
    pub functions: HashMap<String, bool>,
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
    Call(String, Vec<TypedExpression>, Span),
    Value(TypedValue, Span),
    Identifier(String, Span),
    KVMap(Vec<(String, TypedExpression)>, Span),
    Shader(TypedShaderInstance, Span),
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
            TypedExpression::Error() => 0..0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunctionParameter {
    pub name: String,
    pub type_name: ExpandedType,
    pub default_value: Option<TypedExpression>,
}

#[derive(Debug, Clone)]
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
    pub fn get_span(&self) -> Span {
        match self {
            TypedStatement::Return(_, span) => span.clone(),
            TypedStatement::If { span, .. } => span.clone(),
            TypedStatement::Let { span, .. } => span.clone(),
            TypedStatement::Expression(_, span) => span.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedTagType {
    Async,
    DynamicAlloc,
    CPUOnly,
    ShaderOnly,
    Recursive,
}

#[derive(Debug, Clone)]
pub struct TypedTag {
    pub tag: TypedTagType,
    pub span: Span,
    pub name: String,
    pub introduced_by: Option<Box<TypedTag>>,
}

#[derive(Debug, Clone)]
pub struct TypedBody {
    pub statements: Vec<TypedStatement>,
    pub tags: Vec<TypedTag>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct TypedIntermediate {
    pub functions: HashMap<String, TypedFunction>,
    pub structs: Vec<(String, Vec<String>)>,
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
            let func = in_intermediate.functions.get(func_name).unwrap();
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
            TypedStatement::Return(_, _span) => {}
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
            TypedStatement::Let { name: _, value, span: _ } => {
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
            structs: Vec::new(),
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
    _alerts: &mut Vec<SpannedAlert>,
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

        return None;
    }

    if symbol.is_some() {
        let symbol = symbol.unwrap();

        if let SymbolDefinition::Type(def_type) = &symbol.definition {
            return Some(def_type);
        } else {
            _alerts.push(SpannedAlert::error(
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
    _alerts: &mut Vec<SpannedAlert>,
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
                _alerts.push(SpannedAlert::error(
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
                        _alerts.push(SpannedAlert::error(
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
                    _alerts.push(SpannedAlert::error(
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
        _alerts.push(SpannedAlert::error(
            "Undefined operator".to_string(),
            format!(
                "No operator '{}' was found that takes a right-hand side of type '{}'",
                method_name.replace("__operator_", ""),
                args[0].name
            ),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    } else {
        _alerts.push(SpannedAlert::error(
            "Undefined method".to_string(),
            format!("The method '{}' was not found", method_name),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        ));
    }

    return "!error".to_string();
}

pub fn get_type_local(
    _alerts: &mut Vec<SpannedAlert>,
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
                        SymbolDefinition::Type(_) => (
                            ident.name.clone(),
                            TypedExpression::Identifier(
                                symbol.get_namespaced(),
                                ident.span.clone(),
                            ),
                        ),

                        SymbolDefinition::Function(func) => (
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
                        ),
                        _ => ("!error".to_string(), TypedExpression::Error()),
                    }
                } else {
                    _alerts.push(SpannedAlert::error(
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
            let (func_type, typed_expr) = get_type_local(
                _alerts,
                scope,
                graph,
                file_name,
                intermediate,
                &call.expression,
            );

            let mut typed_args = Vec::new();

            let func_type = ExpandedType::from_string(graph, file_name, &func_type);
            if func_type.name == "function" {
                let mut out_type = "!error".to_string();
                if func_type.generics.len() > 0 {
                    out_type = func_type.generics[0].name.clone();
                }

                if call.args.len() > func_type.generics.len() - 1 {
                    _alerts.push(SpannedAlert::error(
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
                            _alerts.push(SpannedAlert::error(
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
                        _alerts,
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
                        _alerts.push(SpannedAlert::error(
                            "Type mismatch".to_string(),
                            format!(
                                "Expected type '{}' but got type '{}'",
                                real_param_type.to_string(),
                                arg_type.to_string()
                            ),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(call.span.start, call.span.end),
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
                _alerts.push(SpannedAlert::error(
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
        ast::Expression::InlineBlock((block, _)) => {
            let mut shader_inst = TypedShaderInstance {
                closure: HashMap::new(),
                functions: HashMap::new(),
            };

            let output = ExpandedType::from_string(graph, file_name, "shader");

            let mut new_scope = Scope::new();
            new_scope.parent = Some(Box::new(scope));
            new_scope.shader_barrier = true;

            let typed_body = check_body_local(
                _alerts,
                &mut new_scope,
                &graph,
                file_name,
                intermediate,
                &block.roots,
                &output,
            );

            build_shader_instance(
                _alerts,
                &graph,
                &new_scope,
                &mut shader_inst,
                file_name,
                intermediate,
                &typed_body,
            );

            (
                "shader".to_owned(),
                TypedExpression::Shader(shader_inst, block.span.clone()),
            )
        }
        ast::Expression::StructInstance((ident, args, _)) => {
            let checked_type = check_type_local(_alerts, graph, file_name, ident);
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
                                        _alerts,
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
                get_type_local(_alerts, scope, graph, file_name, intermediate, lhs);
            let (rhs_type, rhs_typed_expr) =
                get_type_local(_alerts, scope, graph, file_name, intermediate, rhs);

            let lhs_type = ExpandedType::from_string(graph, file_name, &lhs_type);
            let rhs_type = ExpandedType::from_string(graph, file_name, &rhs_type);

            let lhs_symbol = check_type_local(
                _alerts,
                graph,
                file_name,
                &ast::Identifier {
                    name: lhs_type.name.clone(),
                    span: lhs.get_span(),
                },
            );

            if let Op::Dot = op {
                if let Some(lhs_symbol) = lhs_symbol {
                    let lhs_symbol_node = graph
                        .files
                        .get(file_name)
                        .unwrap()
                        .get(&lhs_type.name)
                        .unwrap();
                    if let ast::Expression::Identifier(rhs_ident) = rhs.as_ref() {
                        let field = lhs_symbol.fields.iter().find(|x| x.0 == rhs_ident.0.name);
                        let method = lhs_symbol.methods.iter().find(|x| x.0 == rhs_ident.0.name);
                        if let Some(field) = field {
                            return (
                                field.1.clone(),
                                TypedExpression::Call(
                                    format!(
                                        "__get_struct_{}_{}",
                                        lhs_symbol_node.get_namespaced(),
                                        field.0
                                    ),
                                    vec![lhs_typed_expr],
                                    span.clone(),
                                ),
                            );
                        } else if let Some(ref method) = method {
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
                            _alerts.push(SpannedAlert::error(
                                "Field not found".to_string(),
                                format!(
                                    "'{}' does not have a member named '{}'",
                                    lhs_type.name.clone(),
                                    rhs_ident.0.name.clone()
                                ),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(span.start, span.end),
                                ),
                            ));
                            return ("!error".to_string(), TypedExpression::Error());
                        }
                    } else {
                        _alerts.push(SpannedAlert::error(
                            "Expected identifier".to_string(),
                            format!("got: '{}'", rhs_type.name),
                            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                        ));
                        return ("!error".to_string(), TypedExpression::Error());
                    }
                } else {
                    _alerts.push(SpannedAlert::error(
                        "Expected struct".to_string(),
                        format!("got: '{}'", lhs_type.name),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                    return ("!error".to_string(), TypedExpression::Error());
                }
            } else {
                if let Some(lhs_symbol) = lhs_symbol {
                    let mut params = vec![lhs_type, rhs_type];
                    let method_op = check_method_local(
                        _alerts,
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
                    _alerts.push(SpannedAlert::error(
                        "Undefined left-hand side".to_string(),
                        format!("The expression is undefined"),
                        Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
                    ));
                    return ("!error".to_string(), TypedExpression::Error());
                }
            }
        }
        _ => ("!error".to_owned(), TypedExpression::Error()),
    }
}

fn build_shader_expression(
    _alerts: &mut Vec<SpannedAlert>,
    graph: &SymbolGraph,
    scope: &Scope,
    shader_inst: &mut TypedShaderInstance,
    file_name: &str,
    intermediate: &TypedIntermediate,
    typed_expression: &TypedExpression,
) {
    match typed_expression {
        TypedExpression::Call(func_name, args, _) => {
            shader_inst.functions.insert(func_name.clone(), true);

            if let Some(func) = intermediate.functions.get(func_name) {
                build_shader_instance(
                    _alerts,
                    graph,
                    scope,
                    shader_inst,
                    file_name,
                    intermediate,
                    &func.body,
                );

                for arg in args {
                    build_shader_expression(
                        _alerts,
                        graph,
                        scope,
                        shader_inst,
                        file_name,
                        intermediate,
                        arg,
                    );
                }
            }
        }
        TypedExpression::Value(_, _) => {}
        TypedExpression::Identifier(ident, _) => {
            if let Some((scoped_var, barrier)) = scope.check_with_shader_barrier(ident, false) {
                if barrier {
                    shader_inst
                        .closure
                        .insert(ident.clone(), scoped_var.clone());
                }
            }
        }
        TypedExpression::KVMap(_, _) => {}
        TypedExpression::Shader(_, span) => _alerts.push(SpannedAlert::error(
            "Illegal shader def".to_string(),
            "Shaders cannot be defined inside other shaders".to_string(),
            Location::new(file_name.to_string(), USizeTuple(span.start, span.end)),
        )),
        TypedExpression::Error() => {}
    }
}

// Recursively descends into the Typed tree and adds references to the shader_inst
fn build_shader_instance(
    _alerts: &mut Vec<SpannedAlert>,
    graph: &SymbolGraph,
    new_scope: &Scope,
    shader_inst: &mut TypedShaderInstance,
    file_name: &str,
    intermediate: &TypedIntermediate,
    typed_body: &TypedBody,
) {
    for statement in &typed_body.statements {
        match statement {
            TypedStatement::Return(_, span) => _alerts.push(SpannedAlert::error(
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
                    _alerts,
                    graph,
                    new_scope,
                    shader_inst,
                    file_name,
                    intermediate,
                    condition,
                );
                build_shader_instance(
                    _alerts,
                    graph,
                    new_scope,
                    shader_inst,
                    file_name,
                    intermediate,
                    body,
                );
                for else_if in else_ifs {
                    build_shader_expression(
                        _alerts,
                        graph,
                        new_scope,
                        shader_inst,
                        file_name,
                        intermediate,
                        &else_if.0,
                    );
                    build_shader_instance(
                        _alerts,
                        graph,
                        new_scope,
                        shader_inst,
                        file_name,
                        intermediate,
                        &else_if.1,
                    );
                }
                if let Some(else_body) = else_body {
                    build_shader_instance(
                        _alerts,
                        graph,
                        new_scope,
                        shader_inst,
                        file_name,
                        intermediate,
                        else_body,
                    );
                }
            }
            TypedStatement::Let { name: _, value, span: _ } => {
                build_shader_expression(
                    _alerts,
                    graph,
                    new_scope,
                    shader_inst,
                    file_name,
                    intermediate,
                    value,
                );
            }
            TypedStatement::Expression(expr, _) => {
                build_shader_expression(
                    _alerts,
                    graph,
                    new_scope,
                    shader_inst,
                    file_name,
                    intermediate,
                    expr,
                );
            }
        }
    }
}

#[derive(Debug, Clone)]
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

    pub fn is_compatible_with(&self, other: &ExpandedType) -> bool {
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
    _alerts: &mut Vec<SpannedAlert>,
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
                    _alerts.push(SpannedAlert::error(
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
                            _alerts,
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
                    get_type_local(_alerts, scope, graph, file_name, intermediate, expr);

                typed_body
                    .statements
                    .push(TypedStatement::Expression(typed_expr, expr.get_span()));
            }
            ast::Root::If(_if) => {
                let (_type, _cond_typed) = get_type_local(
                    _alerts,
                    scope,
                    graph,
                    file_name,
                    intermediate,
                    &_if.condition,
                );

                if _type != "bool" {
                    _alerts.push(SpannedAlert::error(
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
                    _alerts,
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
                        _alerts,
                        scope,
                        graph,
                        file_name,
                        intermediate,
                        &elif.condition,
                    );
                    if _type != "bool" {
                        _alerts.push(SpannedAlert::error(
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
                        _alerts,
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
                        _alerts,
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
                        _alerts,
                        scope,
                        graph,
                        file_name,
                        intermediate,
                        &_return.value.as_ref().unwrap(),
                    );
                    let expanded_type = ExpandedType::from_string(graph, file_name, &_type);
                    if !expanded_type.is_compatible_with(output) {
                        _alerts.push(SpannedAlert::error(
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
                        _alerts.push(SpannedAlert::error(
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
            _ => _alerts.push(SpannedAlert::error(
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
) -> (Vec<SpannedAlert>, TypedIntermediate) {
    let mut alerts = Vec::new();

    let file = graph.files.get(file_name);
    let mut typed = TypedIntermediate {
        functions: HashMap::new(),
        structs: Vec::new(),
    };

    if file.is_none() {
        return (alerts, typed);
    }

    let file = file.unwrap();

    let check_type = |_alerts: &mut Vec<SpannedAlert>, name: &ast::Identifier| {
        check_type_local(_alerts, graph, file_name, name);
    };

    // let parse_symbol_type =
    //     |_alerts: &mut Vec<SpannedAlert>, scope: Scope, expr: Expression| -> ExpandedType {};

    // let get_type = |_alerts: &mut Vec<SpannedAlert>,
    //                 scope: &Scope,
    //                 expr: &Expression|
    //  -> (String, TypedExpression) {

    // };

    let _check_body = |_alerts: &mut Vec<SpannedAlert>,
                      scope: &mut Scope,
                      body: &Vec<ast::Root>,
                      intermediate: &mut TypedIntermediate,
                      output: &ExpandedType|
     -> TypedBody {
        check_body_local(_alerts, scope, graph, file_name, intermediate, body, output)
    };

    fn add_function_local<'a>(
        _alerts: &mut Vec<SpannedAlert>,
        graph: &'a SymbolGraph,
        file_name: &str,
        intermediate: &mut TypedIntermediate,
        function: &ast::Function,
    ) -> TypedFunction {
        for (i, param) in function.parameters.iter().enumerate() {
            for (j, param2) in function.parameters.iter().enumerate() {
                if i < j && param.0.name == param2.0.name {
                    _alerts.push(SpannedAlert::error_2(
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

            check_type_local(_alerts, graph, file_name, &param.1);
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
            _alerts,
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
                                _alerts,
                                &scope,
                                graph,
                                file_name,
                                &mut TypedIntermediate {
                                    functions: HashMap::new(),
                                    structs: vec![],
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
                    add_function_local(&mut alerts, graph, file_name, &mut typed, function);
                typed.functions.insert(typed_func.name.clone(), typed_func);
            }
            ast::Root::Struct(_struct) => {
                for (i, field) in _struct.fields.iter().enumerate() {
                    for (j, field2) in _struct.fields.iter().enumerate() {
                        if i < j && field.0.name == field2.0.name {
                            alerts.push(SpannedAlert::error_2(
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

                    check_type(&mut alerts, &field.1);
                }

                typed.structs.push((
                    symbol.get_namespaced(),
                    _struct.fields.iter().map(|f| f.0.name.clone()).collect(),
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
                                &mut alerts,
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

    (alerts, typed)
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
            TypedStatement::Let { name: _, value, span: _ } => {
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
            tag_function(intermediate, func_name);
            let func = intermediate.functions.get(func_name).unwrap();
            tags.append(&mut retag(func_name, span, &mut func.tags.clone()));

            for expr in exprs {
                tags.append(&mut propagate_tags_from_expression(intermediate, expr));
            }
        }
        TypedExpression::Value(_, _) => {}
        TypedExpression::Identifier(_, _) => {}
        TypedExpression::KVMap(_, _) => {}
        TypedExpression::Shader(_, _) => {}
        TypedExpression::Error() => {}
    }

    tags
}
