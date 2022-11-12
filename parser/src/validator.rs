use std::collections::HashMap;

use crate::ast::{self, Expression, Identifier, Location, Op, Span, USizeTuple};
use crate::graph::{SymbolDefinition, SymbolGraph, SymbolNode, SymbolType};
use crate::printer::SpannedAlert;

struct Scope {
    definitions: HashMap<String, String>,
    parent: Option<Box<Scope>>,
}

impl Scope {
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

struct ExpandedType {
    name: String,
    generics: Vec<ExpandedType>,
}

impl ExpandedType {
    pub fn new(name: String, generics: Vec<ExpandedType>) -> Self {
        Self { name, generics }
    }

    pub fn is_compatible_with(&self, other: &ExpandedType) -> bool {
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

    // Recursively convert types with generics: map<string, array<int>>
    pub fn from_string(string: &str) -> Self {
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
                    generics.push(ExpandedType::from_string(generic.as_str()));
                    generic = String::new();
                } else {
                    name.push(c);
                }
            } else {
                generic.push(c);
            }
        }

        if generic.len() > 0 {
            generics.push(ExpandedType::from_string(generic.as_str()));
        }

        Self::new(name, generics)
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

#[allow(dead_code)]
pub fn validate(graph: &SymbolGraph, file_name: &str) -> Vec<SpannedAlert> {
    let mut alerts = Vec::new();

    let file = graph.files.get(file_name);

    if file.is_none() {
        return alerts;
    }

    let file = file.unwrap();

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

    let check_type = |_alerts: &mut Vec<SpannedAlert>, name: &ast::Identifier| {
        check_type_local(_alerts, graph, file_name, name);
    };

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
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(span.start, span.end),
                                ),
                            ));
                            break;
                        }
                    }

                    let arg = &args[i];

                    if !arg.is_compatible_with(&ExpandedType::from_string(param.1.as_str())) {
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

        return "void".to_string();
    }

    // let parse_symbol_type =
    //     |_alerts: &mut Vec<SpannedAlert>, scope: Scope, expr: Expression| -> ExpandedType {};

    let get_type = |_alerts: &mut Vec<SpannedAlert>, scope: &Scope, expr: &Expression| -> String {
        fn get_type_local(
            _alerts: &mut Vec<SpannedAlert>,
            scope: &Scope,
            graph: &SymbolGraph,
            file_name: &str,
            expr: &Expression,
        ) -> String {
            match expr {
                ast::Expression::Value((value, _)) => match value {
                    ast::Value::Int(_) => "int".to_string(),
                    ast::Value::Str(_) => "string".to_string(),
                    ast::Value::Bool(_) => "bool".to_string(),
                    ast::Value::Real(_) => "float".to_string(),
                    ast::Value::Null => "null".to_string(),
                },
                ast::Expression::Error(_) => "void".to_string(),
                ast::Expression::Identifier((ident, _)) => {
                    if let Some(local_type) = scope.check(&ident.name) {
                        local_type
                    } else {
                        let file = graph.files.get(file_name).unwrap();
                        let symbol = file.get(&ident.name);
                        if let Some(symbol) = symbol {
                            match &symbol.definition {
                                SymbolDefinition::Type(_) => ident.name.clone(),

                                SymbolDefinition::Function(func) => format!(
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
                                _ => "void".to_string(),
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
                            "void".to_string()
                        }
                    }
                }
                ast::Expression::Call((call, _)) => {
                    let func_type =
                        get_type_local(_alerts, scope, graph, file_name, &call.expression);

                    let func_type = ExpandedType::from_string(&func_type);
                    if func_type.name == "function" {
                        let mut out_type = "void".to_string();
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

                            let arg_type = ExpandedType::from_string(&get_type_local(
                                _alerts,
                                scope,
                                graph,
                                file_name,
                                &call.args[i],
                            ));

                            let real_param_type = if param.name == "__optional" {
                                &param.generics[0]
                            } else {
                                &param
                            };

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

                        out_type
                    } else {
                        _alerts.push(SpannedAlert::error(
                            "Expected something callable".to_string(),
                            format!("got: '{}'", func_type.name),
                            Location::new(
                                file_name.to_string(),
                                USizeTuple(call.span.start, call.span.end),
                            ),
                        ));
                        "void".to_string()
                    }
                }
                ast::Expression::InlineBlock((block, _)) => "void".to_owned(),
                ast::Expression::StructInstance((ident, _, _)) => ident.name.clone(),
                ast::Expression::Op((lhs, op, rhs, span)) => {
                    let lhs_type = get_type_local(_alerts, scope, graph, file_name, lhs);
                    let rhs_type = get_type_local(_alerts, scope, graph, file_name, rhs);

                    let lhs_type = ExpandedType::from_string(&lhs_type);
                    let rhs_type = ExpandedType::from_string(&rhs_type);

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
                            if let ast::Expression::Identifier(rhs_ident) = rhs.as_ref() {
                                let field =
                                    lhs_symbol.fields.iter().find(|x| x.0 == rhs_ident.0.name);
                                let method =
                                    lhs_symbol.methods.iter().find(|x| x.0 == rhs_ident.0.name);
                                if let Some(field) = field {
                                    return field.1.clone();
                                } else if let Some(ref method) = method {
                                    return format!(
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
                                    );
                                } else {
                                    _alerts.push(SpannedAlert::error(
                                        "Field not found".to_string(),
                                        format!(
                                            "Struct '{}' does not have a member named '{}'",
                                            lhs_type.name.clone(),
                                            rhs_ident.0.name.clone()
                                        ),
                                        Location::new(
                                            file_name.to_string(),
                                            USizeTuple(span.start, span.end),
                                        ),
                                    ));
                                    return "void".to_string();
                                }
                            } else {
                                _alerts.push(SpannedAlert::error(
                                    "Expected identifier".to_string(),
                                    format!("got: '{}'", rhs_type.name),
                                    Location::new(
                                        file_name.to_string(),
                                        USizeTuple(span.start, span.end),
                                    ),
                                ));
                                return "void".to_string();
                            }
                        } else {
                            _alerts.push(SpannedAlert::error(
                                "Expected struct".to_string(),
                                format!("got: '{}'", lhs_type.name),
                                Location::new(
                                    file_name.to_string(),
                                    USizeTuple(span.start, span.end),
                                ),
                            ));
                            return "void".to_string();
                        }
                    } else {
                        let method_op = check_method_local(
                            _alerts,
                            graph,
                            file_name,
                            span,
                            lhs_symbol.unwrap(),
                            &lhs_type.generics,
                            &format!("__operator_{}", op.get_code_name()),
                            &vec![rhs_type],
                        );

                        // let rhs_symbol = graph.files.get(file_name).unwrap().get(&op.name);

                        method_op.to_owned()
                    }
                }
                _ => "void".to_owned(),
            }
        }

        get_type_local(_alerts, scope, graph, file_name, &expr)
    };

    let get_optional_type = |_alerts: &mut Vec<SpannedAlert>,
                             scope: &Scope,
                             _type: Option<Identifier>,
                             expr: Expression| {
        if let Some(ref _type) = _type {
            check_type(_alerts, _type);
            return _type.name.clone();
        } else {
            let _type = get_type(_alerts, scope, &expr);
            return _type;
        }
    };

    let check_body = |_alerts: &mut Vec<SpannedAlert>, scope: &mut Scope, body: &Vec<ast::Root>| {
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
                        if _let.to.is_some() {
                            _to_val = get_type(_alerts, scope, _let.to.as_ref().unwrap());
                        }

                        let let_type = if let Some(ref _type) = _let.value_type {
                            _type.name.clone()
                        } else {
                            _to_val
                        };
                        scope
                            .definitions
                            .insert(_let.name.name.clone(), let_type.clone());
                        println!("let {} = {}", _let.name.name.clone(), let_type.clone());
                    }
                }
                _ => _alerts.push(SpannedAlert::error(
                    format!("You can't define this here"),
                    format!("Attempting to define {}", root.get_kind()),
                    Location::new(
                        file_name.to_string(),
                        USizeTuple(root.get_span().start, root.get_span().end),
                    ),
                )),
            }
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

                    check_type(&mut alerts, &param.1);
                }

                let mut scope = Scope::new();
                for param in &function.parameters {
                    scope
                        .definitions
                        .insert(param.0.name.clone(), param.1.name.clone());
                }

                check_body(&mut alerts, &mut scope, &function.body.roots);
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
            }
            _ => {}
        }
    }

    alerts
}
