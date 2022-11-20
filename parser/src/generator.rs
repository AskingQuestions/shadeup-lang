use crate::graph::SymbolGraph;

use crate::validator::{TypedBody, TypedExpression, TypedIntermediate, TypedStatement, TypedValue};
use crate::webgl;

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

                out.push_str("}");

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
    let mut javascript = String::new();
    let file = graph.files.get(file_name);

    let _file = file.unwrap();

    javascript.push_str(&format!("const __SHADERS = [\n"));

    for shader in &typed.shaders {
        let shaken = typed.tree_shake_shader(graph, file_name, shader.clone());
        javascript.push_str(&format!(
            "  __shadeup_gen_shader(`{}`),\n",
            webgl::generate(graph, file_name, &shaken).webgl
        ));
    }

    javascript.push_str(&format!("\n];\n\n"));

    // for _struct in &typed.structs {
    //     let mut out = String::new();

    //     out.push_str(&format!(
    //         "function __make_struct_{}(fields) {{\nreturn {{\n",
    //         _struct.0
    //     ));

    //     for (field, _) in &_struct.1 {
    //         out.push_str(&format!("  {}: fields.{}, ", field, field));
    //     }

    //     out.push_str("\n}");
    //     out.push_str("\n}\n\n");

    //     for (field, _) in &_struct.1 {
    //         out.push_str(&format!(
    //             "function __get_struct_{}_{}(struct) {{\nreturn struct.{};\n}}",
    //             _struct.0, field, field
    //         ));
    //     }

    //     javascript.push_str(&out);
    // }

    for func in &typed.functions {
        if func.1.tags.len() > 0 {
            javascript.push_str(&format!(
                "/* {} */",
                func.1
                    .tags
                    .iter()
                    .map(|t| format!("{}", t.tag.to_string()))
                    .collect::<Vec<String>>()
                    .join(" ")
            ));
        }
        javascript.push_str(&format!(
            "function {}({}) {{\n",
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

        javascript.push_str("\n}\n\n");
    }

    ProgramOutput { javascript }
}
