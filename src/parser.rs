use std::collections::HashSet;

use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use crate::common::*;
use crate::types::*;


pub fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if !(*n >= -2_i64.pow(62) && *n <= 2_i64.pow(62) - 1) {
                panic!("not a valid number must be an integer between -2^62 and 2^62-1");
            }
            Expr::Number(tag_number(*n))
        }
        Sexp::Atom(S(name)) => {
            /* Check is boolean */
            if (name == "true") {
                return Expr::Boolean(true);
            } else if (name == "false") {
                return Expr::Boolean(false);
            }
            Expr::Id(name.to_string())
        }
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isnum" => {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isbool" => {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(type_name)), e] if op == "cast" => {
                    let target_type = match type_name.as_str() {
                        "Num" => TypeInfo::Num,
                        "Bool" => TypeInfo::Bool,
                        "Nothing" => TypeInfo::Nothing,
                        "Any" => TypeInfo::Any,
                        _ => panic!("Invalid type: {}", type_name),
                    };
                    Expr::Cast(target_type, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.is_empty() {
                        panic!("Invalid: block needs at least one expression");
                    }
                    Expr::Block(exprs.iter().map(parse_expr).collect())
                }

                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    // Map each binding to (var_name, parsed_expr) tuple
                    let parsed_bindings: Vec<(String, Expr)> = bindings
                        .iter()
                        .map(|binding| match binding {
                            Sexp::List(pair) => match &pair[..] {
                                [Sexp::Atom(S(var)), val] => (var.to_string(), parse_expr(val)),
                                _ => panic!("Invalid binding: expected (variable value)"),
                            },
                            _ => panic!("Invalid binding: expected a list"),
                        })
                        .collect();

                    Expr::Let(parsed_bindings, Box::new(parse_expr(body)))
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "define" => {
                    Expr::Define(name.to_string(), Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                }

                [Sexp::Atom(S(op)), cond, then_expr, else_expr] if op == "if" => Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(then_expr)),
                    Box::new(parse_expr(else_expr)),
                ),

                // Function call: (<name> <expr>*)
                [Sexp::Atom(S(name)), args @ ..] => {
                    let parsed_args = args.iter().map(|arg| parse_expr(arg)).collect();
                    Expr::FunCall(name.to_string(), parsed_args)
                }

                _ => panic!("parse error!"),
            }
        }
        _ => panic!("parse error"),
    }
}

pub fn parse_defn(s: &Sexp) -> Defn {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(signature), body] if keyword == "fun" => {
                match &signature[..] {
                    [Sexp::Atom(S(name)), params @ ..] => {
                        let param_names: Vec<String> = params
                            .iter()
                            .map(|p| match p {
                                Sexp::Atom(S(param_name)) => param_name.to_string(),
                                _ => panic!("Invalid: function parameter must be an identifier"),
                            })
                            .collect();
                        let mut seen = HashSet::new();
                        
                        for p in &param_names {
                            if !seen.insert(p) {
                                panic!("Duplicate Argument");
                            }
                        }

                        Defn {
                            name: name.to_string(),
                            params: param_names,
                            body: Box::new(parse_expr(body)),
                        }
                    }
                    _ => panic!("Invalid: function definition must have a name"),
                }
            }
            _ => panic!("Invalid: expected function definition (fun ...)"),
        },
        _ => panic!("Invalid: function definition must be a list"),
    }
}

pub fn parse_prog(s: &Sexp) -> Prog {
    match s {
        Sexp::List(items) => {
            let mut defns: Vec<Defn> = Vec::new();
            let mut main_expr = None;

            for item in items {
                match item {
                    Sexp::List(inner) => {
                        if let Some(Sexp::Atom(S(keyword))) = inner.first() {
                            if keyword == "fun" {
                                let defn = parse_defn(item);
                                if defns.iter().any(|d| d.name == defn.name) {
                                    panic!("Multiple functions are defined with the same name");
                                }
                                defns.push(defn);
                                continue;
                            }
                        }
                        // If we get here, it's not a function definition, so it's the main expr
                        if main_expr.is_none() {
                            main_expr = Some(parse_expr(item));
                        } else {
                            panic!("Invalid: only one main expression allowed");
                        }
                    }
                    _ => {
                        // Any other expression (atoms, etc.) is the main expression
                        if main_expr.is_none() {
                            main_expr = Some(parse_expr(item));
                        } else {
                            panic!("Invalid: only one main expression allowed");
                        }
                    }
                }
            }

            Prog {
                defns,
                main: Box::new(
                    main_expr
                        .unwrap_or_else(|| Expr::Boolean(false)),
                ),
            }
        }
        _ => {
            // Single expression, no function definitions
            Prog {
                defns: Vec::new(),
                main: Box::new(parse_expr(s)),
            }
        }
    }
}
