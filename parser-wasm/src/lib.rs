mod utils;
use js_sys::Array;

use wasm_bindgen::prelude::*;

use serde::{Deserialize, Serialize};

use parser::{
    self, ast, environment,
    graph::{self, SymbolDefinition, SymbolNode},
};
extern crate console_error_panic_hook;
use std::panic;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(getter_with_clone)]
pub struct ParserParseResult {
    pub alerts: Array,
}

// #[wasm_bindgen]
// pub fn parse(s: &str) -> ParserParseResult {
//     let tokens = parser::lexer::lex(s);

//     let result = parser::parser::parse(tokens);

//     let mut output = ParserParseResult {
//         alerts: Array::new(),
//     };

//     for alert in result.alerts {
//         // output.push_str(&format!("{}\n\n", &alert.pretty_printed));
//         output.alerts.push(&JsValue::from(alert));
//     }

//     return output;
// }

#[wasm_bindgen]
pub fn make_environment() -> environment::Environment {
    return environment::Environment::new();
}

#[wasm_bindgen]
pub fn set_file(e: &mut environment::Environment, name: String, source: String) {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    e.set_file(name, source);
}

#[wasm_bindgen]
pub fn parse_file(e: &mut environment::Environment, name: &str) -> bool {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    let has_ast = e.parse_file(name).unwrap();
    e.process_file(name);
    has_ast
}

#[wasm_bindgen]
pub fn get_file_alerts(e: &mut environment::Environment, name: &str) -> Array {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
    let arr = Array::new();
    let f = e.get_file(name).unwrap();
    for alert in f.clone().alerts {
        arr.push(&JsValue::from(alert));
    }
    return arr;
}

#[wasm_bindgen]
pub fn generate_file(e: &mut environment::Environment, name: &str) -> String {
    return e.generate_file(name);
}

#[wasm_bindgen]
pub fn get_ast(e: &mut environment::Environment, name: &str) -> String {
    return e.get_ast(name);
}

#[wasm_bindgen]
pub fn get_intellisense(e: &mut environment::Environment, name: &str) -> JsValue {
    let hints = e.get_intellisense(name);

    serde_wasm_bindgen::to_value(&hints).unwrap()
}

#[wasm_bindgen(getter_with_clone)]
struct ExternalSymbol {
    pub name: String,
    pub aliased: bool,
    pub imported: bool,
    pub file: String,
    pub kind: String,
}

#[wasm_bindgen]
pub fn get_symbols(e: &mut environment::Environment) -> Array {
    let symbols = Array::new();

    e.get_graph().files.iter().for_each(|(file_name, file)| {
        file.iter().for_each(|(_name, symbol)| {
            symbols.push(&JsValue::from(ExternalSymbol {
                name: symbol.name.clone(),
                file: file_name.clone(),
                aliased: symbol.aliased,
                imported: symbol.imported,
                kind: match symbol.definition {
                    SymbolDefinition::Function(_) => "function",
                    SymbolDefinition::Type(_) => "type",
                    _ => "unknown",
                }
                .to_string(),
            }));
        });
    });

    symbols
}

#[derive(Serialize, Deserialize)]
struct NativeFunctionSymbol {
    pub name: String,
    pub parameters: Vec<(String, String, bool)>,
    pub return_type: String,
    pub js: String,
}
#[wasm_bindgen]
pub fn add_native_function(e: &mut environment::Environment, val: JsValue) {
    let sym: NativeFunctionSymbol = serde_wasm_bindgen::from_value(val).unwrap();

    let node = SymbolNode {
        aliased: false,
        imported: false,
        file: "external".to_string(),
        name: sym.name.clone(),
        real_name: sym.name.clone(),
        span: 0..0,
        definition: SymbolDefinition::Function(graph::SymbolFunction {
            javascript: Some(sym.js),
            webgl: None,
            return_type: Some(sym.return_type),
            parameters: sym.parameters.clone(),
            span: 0..0,
            tags: vec![],
            method_of: None,
            ..Default::default()
        }),
        root: ast::Root::Error,
    };

    e.add_native_symbol(node);
}

#[derive(Clone)]
#[wasm_bindgen(getter_with_clone)]
struct Span {
    pub start: usize,
    pub end: usize,
}

#[wasm_bindgen(getter_with_clone)]
struct ExternalImportName {
    pub name: String,
    pub alias: Option<String>,
    pub span: Span,
}

#[wasm_bindgen(getter_with_clone)]
struct ExternalImport {
    pub imports: Array,
    pub path: String,
    pub span: Span,
}

#[wasm_bindgen]
pub fn get_imports(e: &mut environment::Environment, name: &str) -> Array {
    let symbols = Array::new();

    let imps = e.get_imports(name);

    for imp in imps {
        let exnames = Array::new();
        for iname in imp.name {
            exnames.push(&JsValue::from(ExternalImportName {
                name: iname.name.name,
                alias: match iname.alias {
                    Some(a) => Some(a.name),
                    None => None,
                },
                span: Span {
                    start: iname.name.span.start,
                    end: iname.name.span.end,
                },
            }));
        }

        symbols.push(&JsValue::from(ExternalImport {
            imports: exnames,
            path: imp.path.value,
            span: Span {
                start: imp.path.span.start,
                end: imp.path.span.end,
            },
        }));
    }

    symbols
}
