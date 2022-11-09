mod ast;
mod parser2;

fn main() {
    parser2::parse("x+=f;")
        .iter()
        .for_each(|err| println!("{:?}", err.msg));
}
