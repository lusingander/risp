use core::fmt;
use std::{collections::HashMap, io, num::ParseFloatError};

#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(f64),
    Bool(bool),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp>),
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::Bool(b) => b.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Func(_) => "Function {}".to_string(),
        };
        write!(f, "{}", str)
    }
}

enum RispErr {
    Reason(String),
}

struct RispEnv {
    data: HashMap<String, RispExp>,
}

type Result<T> = core::result::Result<T, RispErr>;

macro_rules! risp_err {
    ($msg:expr) => {{
        RispErr::Reason($msg.to_string())
    }};
    ($msg:expr, $($args:expr)*) => {{
        RispErr::Reason(format!($msg, $($args)*))
    }};
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

fn parse(tokens: &[String]) -> Result<(RispExp, &[String])> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(risp_err!("could not get token"))?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(risp_err!("unexpected `)`")),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExp, &[String])> {
    let mut res: Vec<RispExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(risp_err!("could not find closing `)`"))?;
        if next_token == ")" {
            return Ok((RispExp::List(res), rest));
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> RispExp {
    match token.as_ref() {
        "true" => RispExp::Bool(true),
        "false" => RispExp::Bool(false),
        _ => {
            let potential_float: core::result::Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => RispExp::Number(v),
                Err(_) => RispExp::Symbol(token.to_string().clone()),
            }
        }
    }
}

macro_rules! ensure_tonicity {
    ($check_fn: expr) => {{
        |args: &[RispExp]| -> Result<RispExp> {
            let floats = parse_list_of_floats(args)?;
            let (first, rest) = floats
                .split_first()
                .ok_or(risp_err!("expected at least one number"))?;
            fn f(prev: &f64, xs: &[f64]) -> bool {
                match xs.split_first() {
                    Some((y, ys)) => $check_fn(prev, y) && f(y, ys),
                    None => true,
                }
            }
            Ok(RispExp::Bool(f(first, rest)))
        }
    }};
}

fn default_env() -> RispEnv {
    let mut data: HashMap<String, RispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExp::Func(|args| {
            let sum = parse_list_of_floats(args)?
                .iter()
                .fold(0.0, |sum, a| sum + a);
            Ok(RispExp::Number(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        RispExp::Func(|args| {
            let floats = parse_list_of_floats(args)?;
            let (first, rest) = floats
                .split_first()
                .ok_or(risp_err!("expected at least one number"))?;
            let sum_of_rest = rest.iter().fold(0.0, |sum, a| sum + a);
            Ok(RispExp::Number(first - sum_of_rest))
        }),
    );
    data.insert(
        "=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a == b)),
    );
    data.insert(
        ">".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a > b)),
    );
    data.insert(
        ">=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a >= b)),
    );
    data.insert(
        "<".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a < b)),
    );
    data.insert(
        "<=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a <= b)),
    );
    RispEnv { data }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>> {
    args.iter().map(parse_single_float).collect()
}

fn parse_single_float(exp: &RispExp) -> Result<f64> {
    match exp {
        RispExp::Number(v) => Ok(*v),
        _ => Err(risp_err!("expected a number")),
    }
}

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp> {
    match exp {
        RispExp::Symbol(k) => eval_symbol(k, env),
        RispExp::Number(_) => Ok(exp.clone()),
        RispExp::Bool(_) => Ok(exp.clone()),
        RispExp::List(list) => eval_list(list, env),
        RispExp::Func(_) => Err(risp_err!("unexpected form")),
    }
}

fn eval_symbol(key: &String, env: &mut RispEnv) -> Result<RispExp> {
    env.data
        .get(key)
        .ok_or(risp_err!("unexpected symbol key = '{}'", key))
        .map(|x| x.clone())
}

fn eval_list(list: &[RispExp], env: &mut RispEnv) -> Result<RispExp> {
    let (first_form, arg_forms) = list
        .split_first()
        .ok_or(risp_err!("expected a non-empty list"))?;
    match eval_built_in_form(first_form, arg_forms, env) {
        Some(result) => result,
        None => {
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExp>>>()?;
                    f(&args_eval)
                }
                _ => Err(risp_err!("first form must be a function")),
            }
        }
    }
}

fn eval_built_in_form(
    exp: &RispExp,
    arg_forms: &[RispExp],
    env: &mut RispEnv,
) -> Option<Result<RispExp>> {
    match exp {
        RispExp::Symbol(s) => match s.as_ref() {
            "if" => Some(eval_if_args(arg_forms, env)),
            "def" => Some(eval_def_args(arg_forms, env)),
            _ => None,
        },
        _ => None,
    }
}

fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp> {
    let test_form = arg_forms.first().ok_or(risp_err!("expected test form"))?;
    let test_eval = eval(test_form, env)?;
    match test_eval {
        RispExp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms
                .get(form_idx)
                .ok_or(risp_err!("expected form idx = {}", form_idx))?;
            eval(res_form, env)
        }
        _ => Err(risp_err!(
            "unexpected test form = {}",
            test_form.to_string()
        )),
    }
}

fn eval_def_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp> {
    if arg_forms.len() != 2 {
        return Err(risp_err!("def can only have two forms"));
    }
    let first_form = arg_forms.get(0).unwrap();
    let second_form = arg_forms.get(1).unwrap();

    let first_str = match first_form {
        RispExp::Symbol(s) => Ok(s.clone()),
        _ => Err(risp_err!("expected first form to be a symbol")),
    }?;
    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp> {
    let (parsed, _) = parse(&tokenize(expr))?;
    let evaled = eval(&parsed, env)?;
    Ok(evaled)
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin()
        .read_line(&mut expr)
        .expect("failed to read line");
    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp > ");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("=> {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("ERROR: {}", msg),
            },
        }
    }
}
