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

fn is_primitive_op_func(func: &str) -> bool {
    let i_type = func.split("___operator_");
    let op = i_type.last().unwrap();

    match op {
        "plus" => true,
        "minus" => true,
        "multiply" => true,
        "divide" => true,
        "modulo" => true,
        "equal" => true,
        "not_equal" => true,
        "greater_than" => true,
        "less_than" => true,
        "greater_than_or_equal" => true,
        "less_than_or_equal" => true,
        "and" => true,
        "or" => true,
        "not" => true,
        _ => false,
    }
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
        format!("_i_{}", id.strip_prefix("__").unwrap_or(id)).replace("___", "_ii_")
    } else {
        id.to_owned().replace("___", "_ii_")
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
        TypedExpression::Identifier(ident, _) => translate_identifier(&ident),
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
                    format!("{}({})", translate_identifier(call), args.join(", ")),
                )
            } else {
                format!("{}({})", translate_identifier(call), args.join(", "))
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

fn gen_statement_local(graph: &SymbolGraph, file_name: &str, root: &TypedStatement) -> String {
    match root {
        TypedStatement::Set(id, expr, _) => {
            format!(
                "{} = {};\n",
                translate_identifier(id),
                gen_expression_local(graph, file_name, &expr)
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
                gen_statement_local(graph, file_name, init),
                gen_expression_local(graph, file_name, condition),
                gen_expression_local(graph, file_name, update)
            ));

            out.push_str(&gen_body_local(graph, file_name, &body));

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
                gen_expression_local(graph, file_name, iterator)
            ));

            if let Some(key) = key {
                out.push_str(&format!("let {} = {};\n", key, value));
            }

            out.push_str(&gen_body_local(graph, file_name, &body));

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
                gen_expression_local(graph, file_name, condition)
            ));

            out.push_str(&gen_body_local(graph, file_name, &body));

            out.push_str("}");

            out
        }
        TypedStatement::Break(span) => "break;\n".to_string(),
        TypedStatement::Continue(span) => "continue;\n".to_string(),
    }
}

fn gen_body_local(graph: &SymbolGraph, file_name: &str, body: &TypedBody) -> String {
    let mut out = String::new();

    for root in &body.statements {
        let line: String = gen_statement_local(graph, file_name, root);

        out.push_str(&line);
    }

    out
}

pub fn generate(graph: &SymbolGraph, file_name: &str, typed: &TypedIntermediate) -> ProgramOutput {
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

    for func in &typed.functions {
        if func.0.starts_with("__make_struct")
            || func.0.starts_with("__get_struct")
            || is_primitive_op_func(&func.0)
        {
            continue;
        }

        if func.0 == "main" {
            for param in &func.1.parameters {
                webgl.push_str(&format!(
                    "uniform {} {};\n",
                    param.type_name.to_string(),
                    translate_identifier(&format!("__in_{}", param.name))
                ));
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
                .iter()
                .map(|p| p.name.as_str())
                .collect::<Vec<&str>>();

            let mut body_mut = func.1.body.clone();

            rename_in_identifiers(&mut body_mut, &idents);
            webgl.push_str(&gen_body_local(graph, file_name, &body_mut));
        }

        webgl.push_str("\n/*__SHADEUP_TEMPLATE_INSERT_MAIN_END__*/\n}\n");
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
