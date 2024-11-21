use std::{collections::HashMap, fs};

fn main() {
    let input = fs::read_to_string("./input.txt").unwrap();
    let mut circuit = Circuit::parse(&input).unwrap();
    let part1 = circuit.clone().eval(Expr::Var("a")).unwrap();
    println!("part 1: {:?}", part1);
    circuit.set("b", part1);
    let part2 = circuit.eval(Expr::Var("a")).unwrap();
    println!("part 2: {:?}", part2);
}

#[derive(Debug, Clone)]
pub struct Circuit<'a>(HashMap<&'a str, Expr<'a>>);

impl<'a> Circuit<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        let wires = input.lines().map(|line| Wire::parse(line)).collect::<Option<Vec<_>>>()?;
        let circuit = wires.into_iter().map(|wire| (wire.var, wire.expr)).collect::<HashMap<_, _>>();
        Some(Self(circuit))
    }

    pub fn set(&mut self, var: &'a str, value: u16) {
        self.0.insert(var, Expr::Num(value));
    }

    pub fn eval(&mut self, expr: Expr<'a>) -> Option<u16> {
        let result = match expr {
            Expr::Num(value) => value,
            Expr::Var(var) => {
                let (var, expr) = self.0.remove_entry(var)?;
                let result = self.eval(expr)?;
                self.set(var, result);
                result
            }
            Expr::Not(expr) => !self.eval(*expr)?,
            Expr::Or(lhs, rhs) => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                lhs | rhs
            }
            Expr::And(lhs, rhs) => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                lhs & rhs
            }
            Expr::LShift(lhs, rhs) => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                lhs << rhs
            }
            Expr::RShift(lhs, rhs) => {
                let lhs = self.eval(*lhs)?;
                let rhs = self.eval(*rhs)?;
                lhs >> rhs
            }
        };

        Some(result)
    }
}

#[derive(Debug, Clone)]
pub struct Wire<'a> {
    var: &'a str,
    expr: Expr<'a>,
}

impl<'a> Wire<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        let (expr, var) = input.split_once(" -> ")?;
        let expr = Expr::parse(expr)?;
        Some(Self { var, expr })
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Num(u16),
    Var(&'a str),
    Not(Box<Expr<'a>>),
    Or(Box<Expr<'a>>, Box<Expr<'a>>),
    And(Box<Expr<'a>>, Box<Expr<'a>>),
    LShift(Box<Expr<'a>>, Box<Expr<'a>>),
    RShift(Box<Expr<'a>>, Box<Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        let words = input.split(' ').collect::<Vec<_>>();
        let res = match words.as_slice() {
            [lit] => parse_lit(lit),
            ["NOT", lit] => Expr::Not(Box::new(parse_lit(lit))),
            [lhs, "OR", rhs] => Expr::Or(Box::new(parse_lit(lhs)), Box::new(parse_lit(rhs))),
            [lhs, "AND", rhs] => Expr::And(Box::new(parse_lit(lhs)), Box::new(parse_lit(rhs))),
            [lhs, "LSHIFT", rhs] => Expr::LShift(Box::new(parse_lit(lhs)), Box::new(parse_lit(rhs))),
            [lhs, "RSHIFT", rhs] => Expr::RShift(Box::new(parse_lit(lhs)), Box::new(parse_lit(rhs))),
            _ => return None,
        };

        Some(res)
    }
}

fn parse_lit<'a>(input: &'a str) -> Expr<'a> {
    input.parse().map(Expr::Num).unwrap_or(Expr::Var(input))
}
