use std::cell::RefCell;
use std::rc::Rc;

use crate::graph::SymbolGraph;

use crate::dep_graph::Graph;
use crate::validator::{
    TypedBody, TypedExpression, TypedIntermediate, TypedShaderDefinition, TypedStatement,
    TypedValue,
};

pub struct ProgramOutput {
    pub webgl: String,
}

fn translate_primitive_calls(func: &str, a: &str, b: &str, normal: String) -> String {
    let x = vec![
        ("___operator_plus", format!("{} + {}", a, b)),
        ("___operator_minus", format!("{} - {}", a, b)),
        ("___operator_multiply", format!("{} * {}", a, b)),
        ("___operator_divide", format!("{} / {}", a, b)),
        ("___operator_modulo", format!("{} % {}", a, b)),
        ("___operator_equal", format!("{} == {}", a, b)),
        ("___operator_not_equal", format!("{} != {}", a, b)),
        ("___operator_greater_than", format!("{} > {}", a, b)),
        ("___operator_less_than", format!("{} < {}", a, b)),
        (
            "___operator_greater_than_or_equal",
            format!("{} >= {}", a, b),
        ),
        ("___operator_less_than_or_equal", format!("{} <= {}", a, b)),
        ("___operator_and", format!("{} && {}", a, b)),
        ("___operator_or", format!("{} || {}", a, b)),
        ("___prefix_operator_not", format!("!{}", a)),
        ("___prefix_operator_minus", format!("-{}", a)),
    ];

    for (op, result) in x {
        if func.contains(&op) {
            return result;
        }
    }

    normal
}

fn is_primitive_op_func(func: &str) -> bool {
    let x = vec![
        "___operator_plus",
        "___operator_minus",
        "___operator_multiply",
        "___operator_divide",
        "___operator_modulo",
        "___operator_equal",
        "___operator_not_equal",
        "___operator_greater_than",
        "___operator_less_than",
        "___operator_greater_than_or_equal",
        "___operator_less_than_or_equal",
        "___operator_and",
        "___operator_or",
        "___prefix_operator_not",
        "___prefix_operator_minus",
    ];

    for op in x {
        if func.contains(&op) {
            return true;
        }
    }
    false
}

pub fn translate_type(ty: &str) -> String {
    match ty {
        "float4" => "vec4".to_string(),
        "float3" => "vec3".to_string(),
        "float2" => "vec2".to_string(),
        "float" => "float".to_string(),
        "int4" => "ivec4".to_string(),
        "int3" => "ivec3".to_string(),
        "int2" => "ivec2".to_string(),
        "int" => "int".to_string(),
        "uint4" => "uvec4".to_string(),
        "uint3" => "uvec3".to_string(),
        "uint2" => "uvec2".to_string(),
        "uint" => "uint".to_string(),
        _ => ty.to_string(),
    }
}

fn translate_identifier(id: &str) -> String {
    if id.starts_with("__") {
        format!("_i_{}", id.strip_prefix("__").unwrap_or(id))
            .replace("___", "_ii_")
            .replace("__", "_i_")
    } else {
        id.to_owned().replace("___", "_ii_").replace("__", "_i_")
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
            TypedValue::Int(ival) => format!("{}", ival),
            TypedValue::String(sval) => format!("\"{}\"", sval.replace('"', "\\\"")),
            TypedValue::Bool(bval) => format!("{}", bval),
            TypedValue::Float(fval) => {
                if fval.fract() == 0.0 {
                    format!("{}.f", fval)
                } else {
                    format!("{}f", fval)
                }
            }
            TypedValue::Null => "null".to_string(),
            TypedValue::Error => "/* !error value */".to_string(),
        },
        TypedExpression::Error() => "/* !error */".to_string(),
        TypedExpression::List(exprs, _) => "/* !error */".to_string(),
        TypedExpression::Identifier(ident, _) => {
            let id_clone = ident.clone().replace("$", "");
            if ident.starts_with("$") {
                format!("_i_in_global_{}", translate_identifier(&id_clone))
            } else {
                translate_identifier(&ident)
            }
        }
        TypedExpression::Call(call, exprs, _) => {
            let mut args = vec![];

            if call.contains("cast_from_scalar") || call.contains("cast_from_vec") {
                let ty = &exprs[0];
                let func = typed.get_function(call);

                if let Some(func) = func {
                    return format!(
                        "{}({})",
                        translate_type(&func.return_type.name),
                        gen_expression_local(graph, file_name, typed, &exprs[0])
                    );
                }
            }

            if call.ends_with("make_vec") {
                let ty = &exprs[0];
                let func = typed.get_function(call);

                if let Some(func) = func {
                    return format!(
                        "{}({})",
                        translate_type(&func.return_type.name),
                        exprs
                            .iter()
                            .map(|e| { gen_expression_local(graph, file_name, typed, &e) })
                            .collect::<Vec<String>>()
                            .join(", ")
                    );
                }
            }

            for arg in exprs {
                args.push(gen_expression_local(graph, file_name, typed, arg));
            }

            if exprs.len() == 2 {
                let first = &args[0];
                let second = &args[1];
                translate_primitive_calls(
                    call,
                    first,
                    second,
                    format!("{}({})", translate_identifier(call), args.join(", ")),
                )
            } else {
                translate_primitive_calls(
                    call,
                    &args[0],
                    &"",
                    format!("{}({})", translate_identifier(call), args.join(", ")),
                )
            }
        }
        TypedExpression::KVMap(map, _) => {
            let mut entries = String::new();

            let mut sorted_props = map.clone();
            sorted_props.sort_by(|a, b| a.0.cmp(&b.0));

            for (_, value) in sorted_props {
                entries.push_str(&format!(
                    "{}, ",
                    gen_expression_local(graph, file_name, typed, &value)
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
                translate_identifier(id),
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
                "{} {} = {};\n",
                translate_type(type_name.to_string().as_str()),
                translate_identifier(name),
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

            out.push_str("}\n");

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
                "for ({}; {}; {};) {{\n",
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
    shader: &TypedShaderDefinition,
    typed: &TypedIntermediate,
) -> ProgramOutput {
    let mut webgl = String::new();
    // webgl.push_str("#version 300 es\n"); // We now do this on the js side
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
                translate_type(&field.1),
                _struct.0,
                field.0,
                _struct.0,
                field.0
            ));
        }

        webgl.push_str(&out);
    }

    // Sort functions by dependence (In theory we should never have any recursive or co-dependent functions)
    let depg = Rc::new(RefCell::new(Graph::new()));
    for func in &typed.functions {
        let cloned_depg = depg.clone();
        cloned_depg.borrow_mut().add_node(func.0.to_string());
        let cloned_depg2 = depg.clone();
        func.1.scan_calls(&move |name: &str| {
            cloned_depg2
                .borrow_mut()
                .add_dependency(name.to_string(), func.0.to_string());
        });
    }

    let mut sorted_functions = depg.borrow_mut().topo_sort();
    for sorted_func in sorted_functions.iter_mut() {
        for func in &typed.functions {
            if sorted_func != func.0 {
                continue;
            }

            if func.0.starts_with("__make_struct")
                || func.0.starts_with("__get_struct")
                || is_primitive_op_func(&func.0)
            {
                continue;
            }

            if func.0 == "main" {
                for param in &func.1.parameters {
                    webgl = format!(
                        "uniform {} {};\n",
                        param.type_name.to_string(),
                        translate_identifier(&format!("__in_{}", param.name.replace("$", "")))
                    ) + &webgl;
                }

                for param in &shader.globals {
                    webgl = format!(
                        "uniform {} {};\n",
                        param.1.to_string(),
                        translate_identifier(&format!("__in_global_{}", param.0.replace("$", "")))
                    ) + &webgl;
                }

                webgl.push_str("/*__SHADEUP_TEMPLATE_INSERT_MAIN_BEFORE__*/\nvoid main() {\n/*__SHADEUP_TEMPLATE_INSERT_MAIN_START__*/\n");
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
                    translate_type(func.1.return_type.to_string().as_str()),
                    translate_identifier(func.0),
                    func.1
                        .parameters
                        .iter()
                        .map(|arg| format!(
                            "{} {}",
                            translate_type(arg.type_name.to_string().as_str()),
                            translate_identifier(&arg.name)
                        ))
                        .collect::<Vec<String>>()
                        .join(", ")
                ));
            }

            if func.1.webgl.is_some() {
                webgl.push_str(&func.1.webgl.as_ref().unwrap());
            } else {
                let idents_temp = func.1.parameters.clone();
                let idents = idents_temp
                    .into_iter()
                    .map(|p| p.name.replace("$", ""))
                    .collect::<Vec<String>>();

                let idents_refs = idents.iter().map(|s| s.as_str()).collect::<Vec<&str>>();

                let mut body_mut = func.1.body.clone();
                if func.0 == "main" {
                    rename_in_identifiers(&mut body_mut, &idents_refs);
                }
                webgl.push_str(&gen_body_local(graph, file_name, typed, &body_mut));
            }

            if func.0 == "main" {
                webgl.push_str("\n/*__SHADEUP_TEMPLATE_INSERT_MAIN_END__*/\n}\n");
            } else {
                webgl.push_str("}\n");
            }
        }
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
        TypedExpression::Wrap(expr, _) => {
            rename_in_identifiers_expression(expr, idents);
        }
        _ => {}
    };
}
fn rename_in_identifiers_statement(statement: &mut TypedStatement, idents: &Vec<&str>) {
    match statement {
        TypedStatement::Set(_, expr, _) => {
            rename_in_identifiers_expression(expr, idents);
        }
        TypedStatement::Expression(expr, _) => {
            rename_in_identifiers_expression(expr, idents);
        }
        TypedStatement::Return(expr, _) => {
            rename_in_identifiers_expression(expr, idents);
        }
        TypedStatement::Let {
            name: _,
            type_name,
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
        TypedStatement::While {
            condition,
            body: while_body,
            span: _,
        } => {
            rename_in_identifiers_expression(condition, idents);
            rename_in_identifiers(while_body, idents);
        }
        TypedStatement::For {
            init,
            condition,
            update,
            body,
            span,
        } => {
            rename_in_identifiers_statement(init, idents);
            rename_in_identifiers_expression(condition, idents);
            rename_in_identifiers_expression(update, idents);
            rename_in_identifiers(body, idents);
        }
        TypedStatement::Break(span) => {}
        TypedStatement::Continue(span) => {}
        TypedStatement::ForEach {
            value,
            key,
            iterator,
            body,
            span,
        } => {
            rename_in_identifiers_expression(iterator, idents);
            rename_in_identifiers(body, idents);
        }
    };
}

fn rename_in_identifiers(body: &mut TypedBody, idents: &Vec<&str>) {
    for statement in body.statements.iter_mut() {
        rename_in_identifiers_statement(statement, idents)
    }
}
