use crate::environment::Environment;

mod ast;
mod environment;
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
        fn test_add() -> int {
            let a = 1;
            let b = 2;
            let c = a + b;
            return c;
        }

    fn add() -> int {
        let a = 1;
        let b = 2;
        let c = a + b + test_add();
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
        print('test');
        let a = add();

        test();

        let x = shader {
            let b = 2;
            let c = a + b;
            let d = c + add();
        };

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

    let generated = env.generate_file("test.shadeup");

    let file = env.get_file("test.shadeup").unwrap();

    let alerts = file.clone().alerts;

    println!("{}", generated);

    for alert in alerts {
        println!("{}", alert.message());
    }
}
