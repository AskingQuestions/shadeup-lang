mod utils;
use js_sys::Array;

use wasm_bindgen::prelude::*;

use parser;

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
pub fn make_environment() -> parser::Environment {
    return parser::Environment::new();
}

#[wasm_bindgen]
pub fn set_file(e: &mut parser::Environment, name: String, source: String) {
    e.set_file(name, source);
}

#[wasm_bindgen]
pub fn parse_file(e: &mut parser::Environment, name: &str) {
    let _ = e.parse_file(name);
}

#[wasm_bindgen]
pub fn get_file_alerts(e: &mut parser::Environment, name: &str) -> Array {
    let arr = Array::new();
    let f = e.get_file(name).unwrap();
    for alert in f.clone().alerts {
        arr.push(&JsValue::from(alert));
    }
    return arr;
}
