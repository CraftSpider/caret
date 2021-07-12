
//! Custom implementation of a prec_climber similar to [`pest::prec_climber::PrecClimber`],
//! with the added ability to handle prefix/suffix operators

use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::BitOr;

use pest::iterators::Pair;
use pest::RuleType;

/// Associativity of an [`Operator`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Assoc {
    /// Left `Operator` associativity
    Left,
    /// Right `Operator` associativity
    Right,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Position {
    /// Infix operator
    Infix,
    /// Prefix operator
    Prefix,
    /// Postfix operator
    Suffix,
}

#[derive(Debug)]
pub struct Operator<R: RuleType> {
    rule: R,
    pos: Position,
    assoc: Assoc,
    next: Option<Box<Operator<R>>>,
}

impl<R: RuleType> Operator<R> {
    pub fn new(rule: R) -> Operator<R> {
        Operator {
            rule,
            pos: Position::Infix,
            assoc: Assoc::Left,
            next: None,
        }
    }

    pub fn new_pos(rule: R, pos: Position) -> Operator<R> {
        Operator {
            rule,
            pos,
            assoc: Assoc::Left,
            next: None,
        }
    }
}

impl<R: RuleType> BitOr for Operator<R> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self {
        fn assign_next<R: RuleType>(op: &mut Operator<R>, next: Operator<R>) {
            if let Some(ref mut child) = op.next {
                assign_next(child, next);
            } else {
                op.next = Some(Box::new(next));
            }
        }

        assign_next(&mut self, rhs);
        self
    }
}

#[derive(Debug)]
pub struct PrecClimber<R: RuleType> {
    ops: HashMap<R, (u32, Position, Assoc)>,
}

impl<R: RuleType> PrecClimber<R> {
    pub fn new(ops: Vec<Operator<R>>) -> PrecClimber<R> {
        let ops = ops
            .into_iter()
            .zip(1..)
            .fold(HashMap::new(), |mut map, (op, prec)| {
                let mut next = Some(op);

                while let Some(op) = next.take() {
                    match op {
                        Operator {
                            rule,
                            pos,
                            assoc,
                            next: op_next,
                        } => {
                            map.insert(rule, (prec, pos, assoc));
                            next = op_next.map(|op| *op);
                        }
                    }
                }

                map
            });

        PrecClimber { ops }
    }

    pub fn climb<'i, P, F, G, T, Pre, Post>(
        &self,
        pairs: P,
        mut primary: F,
        mut infix: G,
        mut prefix: Pre,
        mut postfix: Post,
    ) -> T
        where
            P: Iterator<Item = Pair<'i, R>>,
            F: FnMut(Pair<'i, R>) -> T,
            G: FnMut(T, Pair<'i, R>, T) -> T,
            Pre: FnMut(Pair<'i, R>, T) -> T,
            Post: FnMut(T, Pair<'i, R>) -> T,
    {
        self.climb_rec(
            None,
            0,
            &mut pairs.peekable(),
            &mut primary,
            &mut infix,
            &mut prefix,
            &mut postfix
        )
    }

    fn climb_rec<'i, P, F, G, T, Pre, Post>(
        &self,
        mut lhs: Option<T>,
        min_prec: u32,
        pairs: &mut Peekable<P>,
        primary: &mut F,
        infix: &mut G,
        prefix: &mut Pre,
        postfix: &mut Post,
    ) -> T
        where
            P: Iterator<Item = Pair<'i, R>>,
            F: FnMut(Pair<'i, R>) -> T,
            G: FnMut(T, Pair<'i, R>, T) -> T,
            Pre: FnMut(Pair<'i, R>, T) -> T,
            Post: FnMut(T, Pair<'i, R>) -> T,
    {
        while pairs.peek().is_some() {
            let rule = pairs.peek().unwrap().as_rule();
            if let Some(&(prec, pos, assoc)) = self.ops.get(&rule) {
                if prec >= min_prec {
                    let op = pairs.next().unwrap();
                    match pos {
                        Position::Infix => {
                            let mut rhs = self.climb_rec(None, prec, pairs, primary, infix, prefix, postfix);

                            while pairs.peek().is_some() {
                                let rule = pairs.peek().unwrap().as_rule();
                                if let Some(&(new_prec, _, assoc)) = self.ops.get(&rule) {
                                    if new_prec > prec || assoc == Assoc::Right && new_prec == prec {
                                        rhs = self.climb_rec(Some(rhs), new_prec, pairs, primary, infix, prefix, postfix);
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }

                            lhs = Some(infix(lhs.expect("Infix operator with no left hand side"), op, rhs))
                        }
                        Position::Prefix => {
                            let mut rhs = self.climb_rec(None, prec, pairs, primary, infix, prefix, postfix);

                            while pairs.peek().is_some() {
                                let rule = pairs.peek().unwrap().as_rule();
                                if let Some(&(new_prec, _, assoc)) = self.ops.get(&rule) {
                                    if new_prec > prec || assoc == Assoc::Right && new_prec == prec {
                                        rhs = self.climb_rec(Some(rhs), new_prec, pairs, primary, infix, prefix, postfix);
                                    } else {
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }

                            lhs = Some(prefix(op, rhs))
                        }
                        Position::Suffix => {
                            if assoc == Assoc::Right {
                                todo!("Right associativity for suffix operators")
                            }
                            lhs = Some(postfix(lhs.expect("Postfix operator with no left hand side"), op))
                        }
                    }
                } else {
                    break;
                }
            } else if let None = lhs {
                lhs = Some(primary(pairs.next().unwrap()));
            } else {
                break;
            }
        }

        lhs.unwrap()
    }
}


