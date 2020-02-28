extern crate nom;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag},
    combinator::{map, map_res},
    character::complete::{space0 as space, char},
    multi::fold_many0, 
    sequence::{delimited, pair}
};


fn parens(input: &str) -> IResult<&str, Proposition> {
    delimited(
        space,
        delimited(
            tag("("),
            expr,
            tag(")")
        ),
        space
    )(input)
}

fn term(input: &str) -> IResult<&str, Proposition> {
  alt((
      map(tag("T"), |_| Proposition::Const(true)),
      map(tag("F"), |_| Proposition::Const(false)),
      parens
  ))(input)
}

fn expr(input: &str) -> IResult<&str, Proposition> {
    let (input, init) = term(input)?;
    fold_many0(
        pair(alt((
            char('^'),
            char('v'))), term),
        init,
        |acc, (op, val): (char, Proposition)| {
            match op {
                '^' => Proposition::And(Box::new(acc), Box::new(val)),
                'v' => Proposition::Or(Box::new(acc), Box::new(val)),
                _ => acc
            }
        }
    )(input)
}


#[test]
fn test_parse_proposition() {
    assert_eq!(term("T"), Ok(("", Proposition::Const(true))));
    assert_eq!(term("F"), Ok(("", Proposition::Const(false))));
    assert_eq!(
        expr("T^T"), 
        Ok(("", Proposition::And(Box::new(Proposition::Const(true)), Box::new(Proposition::Const(true))))));
    assert_eq!(
        expr("TvT"), 
        Ok(("", Proposition::Or(Box::new(Proposition::Const(true)), Box::new(Proposition::Const(true))))));
}

#[derive(Debug, PartialEq, Clone, PartialOrd)]
enum Proposition {
    Const(bool),
    And(Box<Proposition>, Box<Proposition>),
    Or(Box<Proposition>, Box<Proposition>),
    Not(Box<Proposition>),
    Imply(Box<Proposition>, Box<Proposition>)
}


fn eval_proposition(proposition: Proposition) -> bool {
    match proposition {
        Proposition::Const(p) => p,
        Proposition::And(p, q) => eval_proposition(*p) && eval_proposition(*q),
        Proposition::Or(p, q) => eval_proposition(*p) || eval_proposition(*q),
        Proposition::Not(p) => !eval_proposition(*p),
        Proposition::Imply(p, q) => eval_proposition(*p) <= eval_proposition(*q)
    }
}

#[test]
fn test_propositions() {
    const T: Proposition = Proposition::Const(true);
    const F: Proposition = Proposition::Const(false);
    assert!(eval_proposition(T));
    assert_eq!(eval_proposition(F), false);
    assert_eq!(eval_proposition(Proposition::Not(Box::new(T))), false);
    assert!(eval_proposition(Proposition::And(Box::new(T), Box::new(T))));
    assert_eq!(eval_proposition(Proposition::And(Box::new(T), Box::new(F))), false);
    assert!(eval_proposition(Proposition::Or(Box::new(T), Box::new(T))));
    assert!(eval_proposition(Proposition::Or(Box::new(T), Box::new(F))));
    assert!(eval_proposition(Proposition::Imply(Box::new(T), Box::new(T))));
}

fn main() {
    println!("Hello, world!");
}
