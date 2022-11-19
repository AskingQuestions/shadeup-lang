use crate::graph::SymbolGraph;

use crate::validator::{TypedBody, TypedExpression, TypedIntermediate, TypedStatement, TypedValue};

pub struct ProgramOutput {
    pub javascript: String,
}

fn gen_expression_local(graph: &SymbolGraph, file_name: &str, expr: &TypedExpression) -> String {
    match expr {
        TypedExpression::Value(value, _) => match value {
            TypedValue::Int(ival) => format!("({} & 0xffffffff)", ival),
            TypedValue::String(sval) => format!("\"{}\"", sval.replace('"', "\\\"")),
            TypedValue::Bool(bval) => format!("{}", bval),
            TypedValue::Float(fval) => format!("{}", fval),
            TypedValue::Null => "null".to_string(),
            TypedValue::Error => "/* !error value */".to_string(),
        },
        TypedExpression::Error() => "/* !error */".to_string(),
        TypedExpression::Identifier(ident, _) => ident.clone(),
        TypedExpression::Call(call, exprs, _) => {
            let mut args = String::new();

            for arg in exprs {
                args.push_str(&gen_expression_local(graph, file_name, arg));
                args.push_str(", ");
            }

            if args.len() > 0 {
                args.pop();
                args.pop();
            }

            format!("{}({})", call, args)
        }
        TypedExpression::KVMap(map, _) => {
            let mut entries = String::new();

            for (key, value) in map {
                entries.push_str(&format!(
                    "{}: {}, ",
                    key,
                    gen_expression_local(graph, file_name, value)
                ));
            }

            if entries.len() > 0 {
                entries.pop();
                entries.pop();
            }

            format!("{{{}}}", entries)
        }
        TypedExpression::Shader(_inst, _) => {
            format!("/* !shader */{{}}")
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
                    "let {} = {};",
                    name,
                    gen_expression_local(graph, file_name, &value)
                )
            }
            TypedStatement::Expression(expr, _) => {
                format!("{};", gen_expression_local(graph, file_name, &expr))
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
                    "if ({}) {{",
                    gen_expression_local(graph, file_name, &condition)
                ));

                out.push_str(&gen_body_local(graph, file_name, &body));

                for elif in else_ifs {
                    out.push_str(&format!(
                        "}} else if ({}) {{",
                        gen_expression_local(graph, file_name, &elif.0)
                    ));

                    out.push_str(&gen_body_local(graph, file_name, &elif.1));
                }

                if else_body.is_some() {
                    out.push_str("} else {");

                    out.push_str(&gen_body_local(
                        graph,
                        file_name,
                        else_body.as_ref().unwrap(),
                    ));
                }

                out.push_str("}");

                out
            }
            TypedStatement::Return(_return, _) => {
                format!(
                    "return {};",
                    gen_expression_local(graph, file_name, &_return)
                )
            }
        };

        out.push_str(&line);
    }

    out
}

pub fn generate(graph: &SymbolGraph, file_name: &str, typed: &TypedIntermediate) -> ProgramOutput {
    let mut javascript = String::new();
    let file = graph.files.get(file_name);

    let _file = file.unwrap();

    for _struct in &typed.structs {
        let mut out = String::new();

        out.push_str(&format!(
            "function __make_struct_{}(fields) {{return {{",
            _struct.0
        ));

        for field in &_struct.1 {
            out.push_str(&format!("{}: fields.{}, ", field, field));
        }

        out.push_str("}");
        out.push_str("}");

        for field in &_struct.1 {
            out.push_str(&format!(
                "function __get_struct_{}_{}(struct) {{return struct.{};}}",
                _struct.0, field, field
            ));
        }

        javascript.push_str(&out);
    }

    for func in &typed.functions {
        javascript.push_str(&format!(
            "function {}({}) {{",
            func.0,
            func.1
                .parameters
                .iter()
                .map(|arg| arg.name.clone())
                .collect::<Vec<String>>()
                .join(", ")
        ));

        if func.1.javascript.is_some() {
            javascript.push_str(&func.1.javascript.as_ref().unwrap());
        } else {
            javascript.push_str(&gen_body_local(graph, file_name, &func.1.body));
        }

        javascript.push_str("}");
    }

    ProgramOutput { javascript }
}
