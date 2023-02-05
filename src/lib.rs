use core::fmt;
use std::{collections::HashMap, num::ParseFloatError, rc::Rc};

#[derive(Clone)]
pub enum RispExp {
    Symbol(String),
    Number(f64),
    Bool(bool),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp>),
    Lambda(RispLambda),
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
            RispExp::Lambda(_) => "Lambda {}".to_string(),
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone)]
pub struct RispLambda {
    params_exp: Rc<RispExp>,
    body_exp: Rc<RispExp>,
}

pub enum RispErr {
    Reason(String),
}

pub struct RispEnv<'a> {
    data: HashMap<String, RispExp>,
    outer: Option<&'a RispEnv<'a>>,
}

pub type Result<T> = core::result::Result<T, RispErr>;

macro_rules! risp_err {
    ($msg:expr) => {{
        RispErr::Reason($msg.to_string())
    }};
    ($msg:expr, $($args:expr),*) => {{
        RispErr::Reason(format!($msg, $($args),*))
    }};
}

pub fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|s| s.to_string())
        .collect()
}

pub fn parse(tokens: &[String]) -> Result<(RispExp, &[String])> {
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

pub fn default_env<'a>() -> RispEnv<'a> {
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
    RispEnv { data, outer: None }
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

pub fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp> {
    match exp {
        RispExp::Symbol(k) => eval_symbol(k, env),
        RispExp::Number(_) => Ok(exp.clone()),
        RispExp::Bool(_) => Ok(exp.clone()),
        RispExp::List(list) => eval_list(list, env),
        RispExp::Func(_) => Err(risp_err!("unexpected form")),
        RispExp::Lambda(_) => Err(risp_err!("unexpected form")),
    }
}

fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(k, outer_env),
            None => None,
        },
    }
}

fn eval_symbol(k: &String, env: &mut RispEnv) -> Result<RispExp> {
    env_get(k, env).ok_or(risp_err!("unexpected symbol k = '{}'", k))
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
                RispExp::Func(f) => f(&eval_forms(arg_forms, env)?),
                RispExp::Lambda(lambda) => {
                    let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                    eval(&lambda.body_exp, new_env)
                }
                _ => Err(risp_err!("first form must be a function")),
            }
        }
    }
}

fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

fn env_for_lambda<'a>(
    params: Rc<RispExp>,
    arg_forms: &[RispExp],
    outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>> {
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(risp_err!(
            "expected {} arguments, but got {}",
            ks.len(),
            arg_forms.len()
        ));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, RispExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(RispEnv {
        data,
        outer: Some(outer_env),
    })
}

fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>> {
    let list = match form.as_ref() {
        RispExp::List(list) => Ok(list.clone()),
        _ => Err(risp_err!("expected args form to be a list")),
    }?;
    list.iter()
        .map(|x| match x {
            RispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(risp_err!("expected symbols in the argument list")),
        })
        .collect()
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
            "fn" => Some(eval_lambda_args(arg_forms)),
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

fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp> {
    if arg_forms.len() != 2 {
        return Err(risp_err!("fn definition can only have two forms"));
    }
    let params_exp = arg_forms.get(0).unwrap();
    let body_exp = arg_forms.get(1).unwrap();
    Ok(RispExp::Lambda(RispLambda {
        params_exp: Rc::new(params_exp.clone()),
        body_exp: Rc::new(body_exp.clone()),
    }))
}
