use parser::Environment;

mod ast;
mod graph;
mod parser2;
mod printer;
mod validator;

fn main() {
    let mut env = Environment::new();

    env.set_file(
        "test.shadeup".to_owned(),
        "fn mainf(a: inta) {}".to_string(),
    );

    let _ = env.parse_file("test.shadeup");

    let file = env.get_file("test.shadeup").unwrap();

    let alerts = file.clone().alerts;

    for alert in alerts {
        println!("{}", alert.message());
    }
}
