use std::{fs, time::Instant};

use crate::environment::Environment;

mod ast;
mod dep_graph;
mod environment;
mod generator;
mod graph;
mod parser2;
mod printer;
mod validator;
mod webgl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut env = Environment::new();

    env.set_file(
        "other.shadeup".to_owned(),
        "
    "
        .to_string(),
    );

    let path = std::env::current_dir()?;

    let start = Instant::now();

    let source = fs::read_to_string(path.to_str().unwrap().to_owned() + "/src/test.shadeup")
        .expect("Something went wrong reading the file");

    env.set_file("test.shadeup".to_owned(), source);

    let _ = env.parse_file("test.shadeup");
    let _ = env.parse_file("other.shadeup");

    let duration = start.elapsed();

    let proc = Instant::now();

    let _ = env.process_file("other.shadeup");
    let _ = env.process_file("test.shadeup");

    let proc_duration = proc.elapsed();

    let gen = Instant::now();

    let generated = env.generate_file("test.shadeup");

    let file = env.get_file("test.shadeup").unwrap();

    let gen_duration = gen.elapsed();

    let alerts = file.clone().alerts;

    println!("{}", generated);

    for alert in alerts {
        println!("{}", alert.message());
    }

    let total = start.elapsed();

    println!("Parse: {:?}", duration);
    println!("Process: {:?}", proc_duration);
    println!("Generate: {:?}", gen_duration);
    println!("Total: {:?}", total);

    Ok(())
}
