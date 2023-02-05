use std::io::{self, stdout, Write};

fn main() {
    let env = &mut risp::default_env();
    loop {
        print!("risp > ");
        stdout().flush().unwrap();
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("=> {}", res),
            Err(e) => match e {
                risp::RispErr::Reason(msg) => println!("ERROR: {}", msg),
            },
        }
    }
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("failed to read line");
    expr
}

fn parse_eval(expr: String, env: &mut risp::RispEnv) -> risp::Result<risp::RispExp> {
    let tokens = risp::tokenize(expr);
    let (parsed, _) = risp::parse(&tokens)?;
    let evaled = risp::eval(&parsed, env)?;
    Ok(evaled)
}
