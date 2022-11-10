mod ast;
mod graph;
mod parser2;

fn main() {
    parser2::parse("x+=f;g")
        .iter()
        .for_each(|err| println!("{:?}", err.msg));
}
