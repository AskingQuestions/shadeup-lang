use parser::Environment;

mod ast;
mod generator;
mod graph;
mod parser2;
mod printer;
mod validator;

fn main() {
    let mut env = Environment::new();

    env.set_file(
        "other.shadeup".to_owned(),
        "
    fn add() -> int {
        let a = 1;
        let b = 2;
        let c = a + b;
        return c;
    }
    "
        .to_string(),
    );

    env.set_file(
        "test.shadeup".to_owned(),
        "
        import add from 'other.shadeup';
    struct Test {
        a: int
        b: uint
    }

    fn test() -> int {
        let a = 1;
        let b = Test {a: 2, b: 2};
        let c = b.a + 2.0;

        if (a >= 1) {
            let d = 2;
            return 3;
        } else if (true) {
            return 1;
        }else{
return 2;
        }
    }
    "
        .to_string(),
    );

    let _ = env.parse_file("test.shadeup");
    let _ = env.parse_file("other.shadeup");

    let _ = env.process_file("other.shadeup");
    let _ = env.process_file("test.shadeup");

    let file = env.get_file("test.shadeup").unwrap();

    let alerts = file.clone().alerts;

    println!("{}", file.generated);

    for alert in alerts {
        println!("{}", alert.message());
    }
}
