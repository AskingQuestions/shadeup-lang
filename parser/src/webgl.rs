use crate::graph::SymbolGraph;

use crate::validator::{TypedBody, TypedExpression, TypedIntermediate, TypedStatement, TypedValue};

pub struct ProgramOutput {
    pub webgl: String,
}

fn translate_primitive_calls(func: &str, a: &str, b: &str, normal: String) -> String {
    let i_type = func.split("___operator_");
    let op = i_type.last().unwrap();

    match op {
        "plus" => format!("{} + {}", a, b),
        "minus" => format!("{} - {}", a, b),
        "multiply" => format!("{} * {}", a, b),
        "divide" => format!("{} / {}", a, b),
        "modulo" => format!("{} % {}", a, b),
        "equal" => format!("{} == {}", a, b),
        "not_equal" => format!("{} != {}", a, b),
        "greater_than" => format!("{} > {}", a, b),
        "less_than" => format!("{} < {}", a, b),
        "greater_than_or_equal" => format!("{} >= {}", a, b),
        "less_than_or_equal" => format!("{} <= {}", a, b),
        "and" => format!("{} && {}", a, b),
        "or" => format!("{} || {}", a, b),
        "not" => format!("!{}", a),
        _ => normal,
    }
}

fn gen_expression_local(graph: &SymbolGraph, file_name: &str, expr: &TypedExpression) -> String {
    match expr {
        TypedExpression::Wrap(expr, _) => {
            format!("({})", gen_expression_local(graph, file_name, expr))
        }
        TypedExpression::Value(value, _) => match value {
            TypedValue::Int(ival) => format!("{}", ival),
            TypedValue::String(sval) => format!("\"{}\"", sval.replace('"', "\\\"")),
            TypedValue::Bool(bval) => format!("{}", bval),
            TypedValue::Float(fval) => format!("{}", fval),
            TypedValue::Null => "null".to_string(),
            TypedValue::Error => "/* !error value */".to_string(),
        },
        TypedExpression::Error() => "/* !error */".to_string(),
        TypedExpression::Identifier(ident, _) => ident.clone(),
        TypedExpression::Call(call, exprs, _) => {
            let mut args = vec![];

            for arg in exprs {
                args.push(gen_expression_local(graph, file_name, arg));
            }

            if exprs.len() == 2 {
                let first = &args[0];
                let second = &args[1];
                translate_primitive_calls(
                    call,
                    first,
                    second,
                    format!("{}({})", call, args.join(", ")),
                )
            } else {
                format!("{}({})", call, args.join(", "))
            }
        }
        TypedExpression::KVMap(map, _) => {
            let mut entries = String::new();

            let mut sorted_props = map.clone();
            sorted_props.sort_by(|a, b| a.0.cmp(&b.0));

            for (_, value) in sorted_props {
                entries.push_str(&format!(
                    "{}, ",
                    gen_expression_local(graph, file_name, &value)
                ));
            }

            if entries.len() > 0 {
                entries.pop();
                entries.pop();
            }

            format!("{}", entries)
        }
        TypedExpression::Shader(inst, _) => {
            format!(
                "__SHADERS[{}].instance({})",
                inst.shader,
                inst.closure
                    .iter()
                    .map(|(name, _)| format!("{}", name))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }
}

fn gen_body_local(graph: &SymbolGraph, file_name: &str, body: &TypedBody) -> String {
    let mut out = String::new();

    for root in &body.statements {
        let line: String = match root {
            TypedStatement::Let {
                name,
                value,
                span: _,
            } => {
                format!(
                    "let {} = {};\n",
                    name,
                    gen_expression_local(graph, file_name, &value)
                )
            }
            TypedStatement::Expression(expr, _) => {
                format!("{};\n", gen_expression_local(graph, file_name, &expr))
            }
            TypedStatement::If {
                condition,
                body,
                else_body,
                else_ifs,
                span: _,
            } => {
                let mut out = String::new();

                out.push_str(&format!(
                    "if ({}) {{\n",
                    gen_expression_local(graph, file_name, &condition)
                ));

                out.push_str(&gen_body_local(graph, file_name, &body));

                for elif in else_ifs {
                    out.push_str(&format!(
                        "\n}} else if ({}) {{\n",
                        gen_expression_local(graph, file_name, &elif.0)
                    ));

                    out.push_str(&gen_body_local(graph, file_name, &elif.1));
                }

                if else_body.is_some() {
                    out.push_str("\n} else {\n");

                    out.push_str(&gen_body_local(
                        graph,
                        file_name,
                        else_body.as_ref().unwrap(),
                    ));
                }

                out.push_str("}\n");

                out
            }
            TypedStatement::Return(_return, _) => {
                format!(
                    "return {};\n",
                    gen_expression_local(graph, file_name, &_return)
                )
            }
        };

        out.push_str(&line);
    }

    out
}

pub fn generate(graph: &SymbolGraph, file_name: &str, typed: &TypedIntermediate) -> ProgramOutput {
    let mut webgl = String::new();
    let file = graph.files.get(file_name);

    let _file = file.unwrap();

    for _struct in &typed.structs {
        let mut out = String::new();

        out.push_str(&format!("struct {} {{\n", _struct.0));

        for field in &_struct.1 {
            out.push_str(&format!("{} {};\n", field.1, field.0));
        }

        out.push_str("\n}\n");

        let mut props = _struct.1.clone();
        props.sort_by(|a, b| a.0.cmp(&b.0));

        let sorted_props = props
            .iter()
            .map(|(name, ty)| format!("{} {}", ty, name))
            .collect::<Vec<String>>();

        out.push_str(&format!(
            "{} __make_struct_{}({}) {{\n{} __struct;\n",
            _struct.0,
            _struct.0,
            sorted_props.join(", "),
            _struct.0,
        ));

        for field in &_struct.1 {
            out.push_str(&format!("__struct.{} = {};\n", field.0, field.0));
        }

        out.push_str("\nreturn __struct;\n}\n");

        for field in &_struct.1 {
            out.push_str(&format!(
                "{} __get_struct_{}_{}({} struct) {{\nreturn struct.{};\n}}\n",
                field.1, _struct.0, field.0, _struct.0, field.0
            ));
        }

        webgl.push_str(&out);
    }

    for func in &typed.functions {
        if func.0.starts_with("__make_struct") || func.0.starts_with("__get_struct") {
            continue;
        }

        if func.0 == "main" {
            for param in &func.1.parameters {
                webgl.push_str(&format!(
                    "in {} __in_{};\n",
                    param.type_name.to_string(),
                    param.name
                ));
            }

            webgl.push_str("void main() {\n");
        } else {
            if func.1.tags.len() > 0 {
                webgl.push_str(&format!(
                    "/* {} */",
                    func.1
                        .tags
                        .iter()
                        .map(|t| format!("{}", t.tag.to_string()))
                        .collect::<Vec<String>>()
                        .join(" ")
                ));
            }
            webgl.push_str(&format!(
                "{} {}({}) {{\n",
                func.1.return_type.to_string(),
                func.0,
                func.1
                    .parameters
                    .iter()
                    .map(|arg| format!("{} {}", arg.type_name.to_string(), arg.name.clone()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ));
        }

        if func.1.javascript.is_some() {
            webgl.push_str(&func.1.javascript.as_ref().unwrap());
        } else {
            let idents_temp = func.1.parameters.clone();
            let idents = idents_temp
                .iter()
                .map(|p| p.name.as_str())
                .collect::<Vec<&str>>();

            let mut body_mut = func.1.body.clone();

            rename_in_identifiers(&mut body_mut, &idents);
            webgl.push_str(&gen_body_local(graph, file_name, &body_mut));
        }

        webgl.push_str("\n}\n");
    }

    ProgramOutput { webgl }
}

fn rename_in_identifiers_expression(expr: &mut TypedExpression, idents: &Vec<&str>) {
    match expr {
        TypedExpression::KVMap(map, _) => {
            for (_, sub) in map.iter_mut() {
                rename_in_identifiers_expression(sub, idents);
            }
        }
        TypedExpression::Call(_, exprs, _) => {
            for expr in exprs.iter_mut() {
                rename_in_identifiers_expression(expr, idents);
            }
        }
        TypedExpression::Identifier(ident, _) => {
            if idents.contains(&ident.as_str()) {
                *ident = format!("__in_{}", ident);
            }
        }
        _ => {}
    };
}
fn rename_in_identifiers(body: &mut TypedBody, idents: &Vec<&str>) {
    for statement in body.statements.iter_mut() {
        match statement {
            TypedStatement::Expression(expr, _) => {
                rename_in_identifiers_expression(expr, idents);
            }
            TypedStatement::Return(expr, _) => {
                rename_in_identifiers_expression(expr, idents);
            }
            TypedStatement::Let {
                name: _,
                value,
                span: _,
            } => {
                rename_in_identifiers_expression(value, idents);
            }
            TypedStatement::If {
                condition,
                body: if_body,
                else_ifs,
                else_body,
                span: _,
            } => {
                rename_in_identifiers_expression(condition, idents);
                rename_in_identifiers(if_body, idents);
                for (condition, el_body) in else_ifs {
                    rename_in_identifiers_expression(condition, idents);
                    rename_in_identifiers(el_body, idents);
                }
                if let Some(else_body) = else_body {
                    rename_in_identifiers(else_body, idents);
                }
            }
        };
    }
}
