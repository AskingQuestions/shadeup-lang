use std::collections::HashMap;

use crate::graph::SymbolGraph;

use crate::validator::{
    propagate_tags, tag_function, TypedBody, TypedExpression, TypedIntermediate, TypedStatement,
    TypedTag, TypedTagType, TypedValue,
};
use crate::webgl;

pub struct ProgramOutput {
    pub javascript: String,
}

pub fn translate_identifier(file_name: &str, ident: &str) -> String {
    if ident.starts_with("$") {
        format!("window['_shadeup_file_globals_{}']", ident.replace("$", ""))
    } else {
        ident.to_string()
    }
}

fn gen_expression_local(
    graph: &SymbolGraph,
    file_name: &str,
    typed: &TypedIntermediate,
    expr: &TypedExpression,
) -> String {
    match expr {
        TypedExpression::Wrap(expr, _) => {
            format!("({})", gen_expression_local(graph, file_name, typed, expr))
        }
        TypedExpression::Value(value, _) => match value {
            TypedValue::Int(ival) => format!("({} & 0xffffffff)", ival),
            TypedValue::String(sval) => format!("\"{}\"", sval.replace('"', "\\\"")),
            TypedValue::Bool(bval) => format!("{}", bval),
            TypedValue::Float(fval) => format!("{}", fval),
            TypedValue::Null => "null".to_string(),
            TypedValue::Error => "/* !error value */".to_string(),
        },
        TypedExpression::Error() => "/* !error */".to_string(),
        TypedExpression::List(exprs, _) => {
            let mut args = String::new();

            for arg in exprs {
                args.push_str(&gen_expression_local(graph, file_name, typed, arg));
                args.push_str(", ");
            }

            if args.len() > 0 {
                args.pop();
                args.pop();
            }

            format!("[{}]", args)
        }
        TypedExpression::Identifier(ident, _) => translate_identifier(file_name, &ident),
        TypedExpression::Call(call, exprs, ctx) => {
            let func = typed.get_function(call);

            if let Some(func) = func {
                // Inline prefix math operators
                let ops = vec![("join", "++"), ("minus_minus", "--")];

                for (name, op) in ops {
                    if call.contains(format!("__prefix_operator_{}", name).as_str()) {
                        return format!(
                            "({}{})",
                            op,
                            gen_expression_local(graph, file_name, typed, &exprs[0])
                        );
                    }
                }

                // Inline postfix math operators
                let ops = vec![("join", "++"), ("minus_minus", "--")];

                for (name, op) in ops {
                    if call.contains(format!("__postfix_operator_{}", name).as_str()) {
                        return format!(
                            "({}{})",
                            gen_expression_local(graph, file_name, typed, &exprs[0]),
                            op
                        );
                    }
                }

                // Inline math operations
                if false {
                    let ops = vec![
                        ("plus", "+"),
                        ("minus", "-"),
                        ("multiply", "*"),
                        ("divide", "/"),
                        ("mod", "%"),
                        ("pow", "**"),
                    ];

                    for (name, op) in ops {
                        if call.ends_with(format!("__operator_{}", name).as_str()) {
                            let mut args = vec![];
                            for expr in exprs {
                                args.push(gen_expression_local(graph, file_name, typed, expr));
                            }
                            return format!("({})", args.join(op));
                        }
                    }
                }

                let mut args = String::new();

                for arg in exprs {
                    args.push_str(&gen_expression_local(graph, file_name, typed, arg));
                    args.push_str(", ");
                }

                if func.has_tag(&TypedTagType::Throws) {
                    args = format!(
                        "['{}', {}, {}], {}",
                        call, ctx.span.start, ctx.span.end, args
                    );
                }

                if args.len() > 0 {
                    args.pop();
                    args.pop();
                }

                if func.has_tag(&TypedTagType::Async) {
                    format!("(await {}({}))", call, args)
                } else {
                    format!("{}({})", call, args)
                }
            } else {
                format!("/* !error function {} */", call)
            }
        }
        TypedExpression::KVMap(map, _) => {
            let mut entries = String::new();

            for (key, value) in map {
                entries.push_str(&format!(
                    "{}: {}, ",
                    key,
                    gen_expression_local(graph, file_name, typed, value)
                ));
            }

            if entries.len() > 0 {
                entries.pop();
                entries.pop();
            }

            format!("{{{}}}", entries)
        }
        TypedExpression::Shader(inst, _) => {
            let shader_globals = typed.shaders.get(inst.shader).unwrap();
            let mut params = inst
                .closure
                .iter()
                .map(|(name, _)| {
                    format!(
                        "{}: {}",
                        name.replace("$", ""),
                        translate_identifier(file_name, name)
                    )
                })
                .collect::<Vec<String>>();
            for (name, value) in &shader_globals.globals {
                params.push(format!(
                    "global_{}: window['_shadeup_file_globals_{}']",
                    name.replace("$", ""),
                    name.replace("$", "")
                ));
            }

            format!(
                "__SHADERS[{}].instance({{{}}})",
                inst.shader,
                params.join(", ")
            )
        }
        TypedExpression::Assign(left, right, _) => {
            format!(
                "{} = {}",
                gen_expression_local(graph, file_name, typed, left),
                gen_expression_local(graph, file_name, typed, right)
            )
        }
        TypedExpression::Access(expr, prop_name, _) => {
            format!(
                "{}.{}",
                gen_expression_local(graph, file_name, typed, expr),
                prop_name
            )
        }
        TypedExpression::Index(expr, index_expr, _) => {
            format!(
                "{}[{}]",
                gen_expression_local(graph, file_name, typed, expr),
                gen_expression_local(graph, file_name, typed, index_expr)
            )
        }
    }
}

fn gen_statement_local(
    graph: &SymbolGraph,
    file_name: &str,
    typed: &TypedIntermediate,
    root: &TypedStatement,
) -> String {
    match root {
        TypedStatement::Set(id, expr, _) => {
            format!(
                "{} = {};\n",
                translate_identifier(file_name, id),
                gen_expression_local(graph, file_name, typed, &expr)
            )
        }
        TypedStatement::Let {
            name,
            type_name,
            value,
            span: _,
        } => {
            format!(
                "let {} = {};\n",
                name,
                gen_expression_local(graph, file_name, typed, &value)
            )
        }
        TypedStatement::Expression(expr, _) => {
            format!(
                "{};\n",
                gen_expression_local(graph, file_name, typed, &expr)
            )
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
                gen_expression_local(graph, file_name, typed, &condition)
            ));

            out.push_str(&gen_body_local(graph, file_name, typed, &body));

            for elif in else_ifs {
                out.push_str(&format!(
                    "\n}} else if ({}) {{\n",
                    gen_expression_local(graph, file_name, typed, &elif.0)
                ));

                out.push_str(&gen_body_local(graph, file_name, typed, &elif.1));
            }

            if else_body.is_some() {
                out.push_str("\n} else {\n");

                out.push_str(&gen_body_local(
                    graph,
                    file_name,
                    typed,
                    else_body.as_ref().unwrap(),
                ));
            }

            out.push_str("}");

            out
        }
        TypedStatement::Return(_return, _) => {
            format!(
                "return {};\n",
                gen_expression_local(graph, file_name, typed, &_return)
            )
        }
        TypedStatement::For {
            init,
            condition,
            update,
            body,
            span,
        } => {
            let mut out = String::new();

            out.push_str(&format!(
                "for ({} {}; {}) {{\n",
                gen_statement_local(graph, file_name, typed, init),
                gen_expression_local(graph, file_name, typed, condition),
                gen_expression_local(graph, file_name, typed, update)
            ));

            out.push_str(&gen_body_local(graph, file_name, typed, &body));

            out.push_str("}");

            out
        }
        TypedStatement::ForEach {
            value,
            key,
            iterator,
            body,
            span,
        } => {
            let mut out = String::new();

            out.push_str(&format!(
                "for (let {} of {}) {{\n",
                if let Some(key) = key {
                    format!("[{}, {}]", key, value)
                } else {
                    format!("{}", value)
                },
                gen_expression_local(graph, file_name, typed, iterator)
            ));

            if let Some(key) = key {
                out.push_str(&format!("let {} = {};\n", key, value));
            }

            out.push_str(&gen_body_local(graph, file_name, typed, &body));

            out.push_str("}");

            out
        }
        TypedStatement::While {
            condition,
            body,
            span,
        } => {
            let mut out = String::new();

            out.push_str(&format!(
                "while ({}) {{\n",
                gen_expression_local(graph, file_name, typed, condition)
            ));

            out.push_str(&gen_body_local(graph, file_name, typed, &body));

            out.push_str("}");

            out
        }
        TypedStatement::Break(span) => "break;\n".to_string(),
        TypedStatement::Continue(span) => "continue;\n".to_string(),
    }
}

fn gen_body_local(
    graph: &SymbolGraph,
    file_name: &str,
    typed: &TypedIntermediate,
    body: &TypedBody,
) -> String {
    let mut out = String::new();

    for root in &body.statements {
        let line: String = gen_statement_local(graph, file_name, typed, root);
        out.push_str(&line);
    }

    out
}

pub fn generate(
    graph: &SymbolGraph,
    file_name: &str,
    full_typed: &TypedIntermediate,
    typed_source: &TypedIntermediate,
) -> ProgramOutput {
    let mut javascript = String::new();
    let file = graph.files.get(file_name);

    let _file = file.unwrap();

    javascript.push_str(&format!("const __SHADERS = [\n"));

    let mut typed = typed_source.clone();

    for (i, shader) in typed_source.shaders.iter().enumerate() {
        let mut shaken = full_typed.tree_shake_shader(graph, file_name, shader.clone());
        tag_function(&mut shaken, "main");

        let mut params = shader
            .parameters
            .iter()
            .map(|p| format!("{}: '{}'", p.0, p.1))
            .collect::<Vec<String>>();

        let mut globals = HashMap::new();

        for tag in shaken.get_function("main").unwrap().tags.iter() {
            if tag.tag == TypedTagType::GlobalVariableAccess {
                if let Some(type_name) = full_typed.globals.get(&tag.type_name) {
                    globals.insert(tag.type_name.clone(), type_name.clone());
                    params.push(format!("global_{}: '{}'", tag.type_name, type_name));
                } else {
                    dbg!(&tag.type_name);
                }
            }
        }

        let mut shade = typed.shaders.get_mut(i).unwrap();
        shade.globals = globals;

        javascript.push_str(&format!(
            "  __shadeup_gen_shader({{{}}}, `{}`),\n",
            params.join(", "),
            webgl::generate(graph, file_name, shade, &shaken).webgl
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

    for _struct in &typed.structs {
        javascript.push_str(&format!(
            "__shadeup_register_struct('{}', {{{}}});\n",
            _struct.0,
            _struct
                .1
                .iter()
                .map(|(field, type_name)| format!("{}: '{}'", field, type_name))
                .collect::<Vec<String>>()
                .join(", ")
        ));
    }

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
            "{}function {}({}{}) {{\n",
            if func.1.has_tag(&TypedTagType::Async) {
                "async "
            } else {
                ""
            },
            func.0,
            if func.1.has_tag(&TypedTagType::Throws) {
                format!(
                    "__stack_info{}",
                    if func.1.parameters.len() > 0 {
                        ", "
                    } else {
                        ""
                    }
                )
            } else {
                "".to_string()
            },
            func.1
                .parameters
                .iter()
                .map(|arg| arg.name.clone())
                .collect::<Vec<String>>()
                .join(", ")
        ));

        if func.1.has_tag(&TypedTagType::Throws) {
            javascript.push_str(&format!("try {{\n"));
        }

        if func.1.javascript.is_some() {
            javascript.push_str(&func.1.javascript.as_ref().unwrap());
        } else {
            javascript.push_str(&gen_body_local(graph, file_name, &typed, &func.1.body));
        }

        if func.1.has_tag(&TypedTagType::Throws) {
            javascript.push_str(&format!(
                "}} catch (e) {{\n  throw __shadeup_error(e, __stack_info);\n}}\n"
            ));
        }

        javascript.push_str("\n}\n\n");
        javascript.push_str(
            format!(
                "window['{}'] = {};\n",
                crate::graph::translate_path_to_safe_name(func.0),
                func.0
            )
            .as_str(),
        );
    }

    ProgramOutput { javascript }
}
