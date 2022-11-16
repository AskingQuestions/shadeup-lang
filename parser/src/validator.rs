use std::collections::HashMap;

use crate::ast::{self, Expression, Identifier, Impl, Location, Op, Span, USizeTuple};
use crate::graph::{SymbolDefinition, SymbolGraph, SymbolNode, SymbolType};
use crate::printer::SpannedAlert;

pub struct Scope<'a> {
    definitions: HashMap<String, String>,
    parent: Option<Box<&'a Scope<'a>>>,
}

pub enum TypedValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Null,
    Error,
}

pub enum TypedExpression {
    Call(String, Vec<TypedExpression>),
    Value(TypedValue),
    Identifier(String),
    KVMap(Vec<(String, TypedExpression)>),
    Error(),
}

pub struct TypedFunctionParameter<'a> {
    pub name: String,
    pub type_name: ExpandedType<'a>,
    pub default_value: Option<TypedExpression>,
}

pub enum TypedStatement {
    Return(TypedExpression),
    If {
        condition: TypedExpression,
        body: TypedBody,
        else_ifs: Vec<(TypedExpression, TypedBody)>,
        else_body: Option<TypedBody>,
    },
    Let {
        name: String,
        value: TypedExpression,
    },
    Expression(TypedExpression),
}

pub struct TypedBody {
    pub statements: Vec<TypedStatement>,
}

pub struct TypedFunction<'a> {
    pub name: String,
    pub parameters: Vec<TypedFunctionParameter<'a>>,
    pub return_type: ExpandedType<'a>,
    pub body: TypedBody,
    pub javascript: Option<String>,
}

pub struct TypedIntermediate<'a> {
    pub functions: HashMap<String, TypedFunction<'a>>,
    pub structs: Vec<(String, Vec<String>)>,
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            parent: None,
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
    generics: &Vec<ExpandedType>,
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
                TypedExpression::Value(TypedValue::Int(ival.clone())),
            ),
            ast::Value::Real(fval) => (
                "float".to_string(),
                TypedExpression::Value(TypedValue::Float(fval.clone())),
            ),
            ast::Value::Str(sval) => (
                "string".to_string(),
                TypedExpression::Value(TypedValue::String(sval.clone())),
            ),
            ast::Value::Bool(bval) => (
                "bool".to_string(),
                TypedExpression::Value(TypedValue::Bool(bval.clone())),
            ),
            ast::Value::Null => ("null".to_string(), TypedExpression::Value(TypedValue::Null)),
        },
        ast::Expression::Error(_) => ("!error".to_string(), TypedExpression::Error()),
        ast::Expression::Identifier((ident, _)) => {
            if let Some(local_type) = scope.check(&ident.name) {
                (local_type, TypedExpression::Identifier(ident.name.clone()))
            } else {
                let file = graph.files.get(file_name).unwrap();
                let symbol = file.get(&ident.name);
                if let Some(symbol) = symbol {
                    match &symbol.definition {
                        SymbolDefinition::Type(_) => (
                            ident.name.clone(),
                            TypedExpression::Identifier(symbol.get_namespaced()),
                        ),

                        SymbolDefinition::Function(func) => (
                            format!(
                                "function<{},{}>",
                                func.return_type.clone().unwrap_or("void".to_owned()),
                                func.parameters
                                    .iter()
                                    .map(|x| if x.2 {
                                        format!("__optional<{}>", x.1.clone())
                                    } else {
                                        x.1.clone()
                                    })
                                    .collect::<Vec<String>>()
                                    .join(",")
                            ),
                            TypedExpression::Identifier(format!("__{}", symbol.get_namespaced())),
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
                    TypedExpression::Identifier(name) => name.clone(),
                    _ => "!error".to_string(),
                };

                (out_type, TypedExpression::Call(func_name, typed_args))
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
        ast::Expression::InlineBlock((block, _)) => ("void".to_owned(), TypedExpression::Error()),
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
                        )],
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
                    span: 0..0,
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
                                TypedExpression::Identifier(format!(
                                    "__{}_{}",
                                    lhs_symbol_node.get_namespaced(),
                                    method.0
                                )),
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
                let mut params = vec![lhs_type, rhs_type];
                let method_op = check_method_local(
                    _alerts,
                    graph,
                    file_name,
                    span,
                    lhs_symbol.unwrap(),
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
                    ),
                )
            }
        }
        _ => ("!error".to_owned(), TypedExpression::Error()),
    }
}

pub struct ExpandedType<'a> {
    name: String,
    generics: Vec<ExpandedType<'a>>,
    symbol_type: Option<&'a SymbolType>,
}

impl<'a> ExpandedType<'a> {
    pub fn new(
        graph: &'a SymbolGraph,
        file_name: &str,
        name: String,
        generics: Vec<ExpandedType<'a>>,
    ) -> Self {
        let symbol_type = || {
            let primitive = graph.primitive.get(&name);

            if primitive.is_some() {
                let inner_wrap = if let SymbolDefinition::Type(ref symbol_type) =
                    primitive.unwrap().definition
                {
                    symbol_type
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
                    return Some(def_type);
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
        if let Some(symbol_type) = self.symbol_type {
            return symbol_type
                .methods
                .iter()
                .find(|method| method.0 == "__is_scalar")
                .is_some();
        }

        false
    }

    pub fn is_vec(&self, size: i32) -> bool {
        if let Some(symbol_type) = self.symbol_type {
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
        if self.is_scalar() && other.is_scalar() {
            if other.name != self.name {
                return TypedExpression::Call(
                    format!("__{}___cast_from_scalar", self.name),
                    vec![expr],
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
                );
            } else {
                return expr;
            }
        } else {
            return expr;
        }
    }

    // Recursively convert types with generics: map<string, array<int>>
    pub fn from_string(graph: &'a SymbolGraph, file_name: &str, string: &str) -> Self {
        let mut name = String::new();
        let mut generics = vec![];
        let mut generic = String::new();
        let mut depth = 0;

        for c in string.chars() {
            if c == '<' {
                depth += 1;
            } else if c == '>' {
                depth -= 1;
            }

            if depth == 0 {
                if c == ',' {
                    generics.push(ExpandedType::from_string(
                        graph,
                        file_name,
                        generic.as_str(),
                    ));
                    generic = String::new();
                } else {
                    name.push(c);
                }
            } else {
                generic.push(c);
            }
        }

        if generic.len() > 0 {
            generics.push(ExpandedType::from_string(
                graph,
                file_name,
                generic.as_str(),
            ));
        }

        Self::new(graph, file_name, name, generics)
    }
}

impl<'a> ToString for ExpandedType<'a> {
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

#[allow(dead_code)]
pub fn validate<'a>(
    graph: &'a SymbolGraph,
    file_name: &str,
) -> (Vec<SpannedAlert>, TypedIntermediate<'a>) {
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

    fn check_body_local(
        _alerts: &mut Vec<SpannedAlert>,
        scope: &mut Scope,
        graph: &SymbolGraph,
        file_name: &str,
        intermediate: &mut TypedIntermediate,
        body: &Vec<ast::Root>,
        output: &ExpandedType,
    ) -> TypedBody {
        let mut typed_body = TypedBody { statements: vec![] };
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
                        let mut _to_typed_expr = TypedExpression::Value(TypedValue::Null);
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
                        });
                    }
                }
                ast::Root::Expression(expr) => {
                    let (_, typed_expr) =
                        get_type_local(_alerts, scope, graph, file_name, intermediate, expr);

                    typed_body
                        .statements
                        .push(TypedStatement::Expression(typed_expr));
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

    let check_body = |_alerts: &mut Vec<SpannedAlert>,
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
    ) -> TypedFunction<'a> {
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

    for (_, (_, _type)) in graph.primitive.iter().enumerate() {
        if let SymbolDefinition::Type(ref sym_type) = _type.definition {
            for method in &sym_type.methods {
                let func_typed = TypedFunction {
                    body: TypedBody { statements: vec![] },
                    javascript: method.1.javascript.clone(),
                    name: method.0.clone(),
                    parameters: method
                        .1
                        .parameters
                        .iter()
                        .map(|(name, _type, optional)| TypedFunctionParameter {
                            name: name.clone(),
                            default_value: None,
                            type_name: ExpandedType::from_string(graph, file_name, _type),
                        })
                        .collect(),
                    return_type: ExpandedType::from_string(
                        graph,
                        file_name,
                        method.1.return_type.as_ref().unwrap_or(&"void".to_string()),
                    ),
                };
                typed
                    .functions
                    .insert(format!("__{}_{}", &_type.name, &method.0), func_typed);
            }
        }
    }

    for symbol in file.values() {
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
