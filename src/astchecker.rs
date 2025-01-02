use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    rc::{Rc, Weak},
};
use std::cell::Ref;

use crate::{lexer::Token::*, tree::Node};

#[derive(PartialEq, Clone)]
enum Value {
    Bool(bool),
    Int(u32),
    Float(f32),
    String(String),
    Nothing,
}

#[derive(PartialEq, Clone)]
enum Type {
    Bool,
    Int,
    Float,
    String,
    None,
    List(Box<Type>),
    Tuple(Vec<Type>),
    Shard(Vec<Type>, Vec<Type>),
    Nullable(Box<Type>),
}

#[derive(PartialEq, Clone)]
enum Effect {
    Pure,
    Func,
    Obs,
    Proc,
}

#[derive(Clone)]
struct Meaning {
    t: Type,
    v: Value,
    e: Effect,
}

impl PartialOrd for Effect {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else if let (Self::Pure, _) | (Self::Func, Self::Obs) | (_, Self::Proc) = (self, other) {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    }
}

struct Table {
    parent: Option<Weak<RefCell<Table>>>,
    map: HashMap<String, Symbol>,
}

trait TableRc {
    fn get(&self, key: String) -> Option<Symbol>;
    fn set(&mut self, key: String, val: Symbol);
    fn add_table(&mut self, key: Option<String>) -> Rc<RefCell<Table>>;
}

impl TableRc for Rc<RefCell<Table>> {
    fn get(&self, key: String) -> Option<Symbol> {
        if key == "_".to_owned() {
            match self.borrow().parent {
                Some(parent) => Some(Symbol::Table(parent.upgrade().unwrap())),
                None => None
            }

        } else {
            self.borrow().map.get(&key).cloned()
        }
    }

    fn set(&mut self, key: String, val: Symbol) {
        self.borrow_mut().map.insert(key, val);
    }

    fn add_table(&mut self, key: Option<String>) -> Rc<RefCell<Table>> {
        let table = Table {
            parent: Some(Rc::downgrade(self)),
            map: HashMap::new(),
        };

        let rc = Rc::new(RefCell::new(table));

        if let Some(k) = key {
            self.set(k, Symbol::Table(Rc::clone(&rc)));
        }

        rc
    }
}

#[derive(Clone)]
enum Symbol {
    Meaning(Meaning),
    Table(Rc<RefCell<Table>>),
}

pub struct ASTChecker {
    global: Rc<RefCell<Table>>,
    local: Rc<RefCell<Table>>,
}

impl ASTChecker {
    pub fn check(&mut self, node_rc: Rc<RefCell<Node>>) -> Meaning {
        let node = node_rc.borrow();
        match node.token.0 {
            _SOF | _Block => {
                let mut meaning = Meaning {
                    t: Type::None,
                    v: Value::Nothing,
                    e: Effect::Pure,
                };

                for child in node.children.clone() {
                    let child_meaning = self.check(child);

                    if child_meaning.e > meaning.e {
                        meaning.e = child_meaning.e;
                    }
                }

                meaning
            }
            _List => {
                let mut inner_t = Type::None;
                let mut e = Effect::Pure;

                for child in node.children.clone() {
                    let child_meaning = self.check(child);

                    if child_meaning.e > e {
                        e = child_meaning.e;
                    }

                    if inner_t == Type::None {
                        inner_t = child_meaning.t;
                    } else if inner_t != child_meaning.t {
                        panic!("Syntax Error")
                    }
                }

                Meaning {
                    t: Type::List(Box::new(inner_t)),
                    v: Value::Nothing,
                    e,
                }
            }
            _Tuple => {
                let mut inner_t_vec: Vec<Type> = vec![];
                let mut e = Effect::Pure;

                for child in node.children.clone() {
                    let child_meaning = self.check(child);

                    if child_meaning.e > e {
                        e = child_meaning.e;
                    }

                    inner_t_vec.push(child_meaning.t);
                }

                Meaning {
                    t: Type::Tuple(inner_t_vec),
                    v: Value::Nothing,
                    e,
                }
            }
            _Lambda => {
                let mut args: Vec<Type> = vec![];

                self.local = self.local.add_table(None);

                for arg in node.children[0].borrow().children.clone() {
                    let arg_meaning = self.check(arg);

                    if let Type::Shard(arg, _) = arg_meaning.t {
                        args.extend(arg);
                    }
                }

                let expr_meaning = self.check(node.children[1].clone());

                if let Some(Symbol::Table(table)) = self.local.get("_".to_owned()) {
                    self.local = Rc::clone(&table);
                } else {
                    panic!("Bug")
                }

                Meaning {
                    t: Type::Shard(args, vec![expr_meaning.t]),
                    v: Value::Nothing,
                    e: expr_meaning.e,
                }
            }
            _Path => {
                let mut meaning: Option<Meaning> = None;
                let mut current_table = Rc::clone(&self.local);

                for child in node.children.clone() {
                    match current_table.get(child.borrow().token.1.clone()) {
                        Some(Symbol::Table(table)) if meaning.is_none() => {
                            current_table = table;
                        }
                        Some(Symbol::Meaning(inner_meaning)) if meaning.is_none() => {
                            meaning = Some(inner_meaning);
                        }
                        _ => panic!("Path Error"),
                    }
                }

                match meaning {
                    Some(inner_meaning) => inner_meaning,
                    None => panic!("Name Error"),
                }
            }
            If => {
                let mut e = Effect::Pure;

                for child in node.children {
                    let child_meaning = self.check(child);

                    if let (_Block, _) | (_, Type::Bool) = (child.borrow().token.0, child_meaning.t) {
                        if child_meaning.e > e {
                            e = child_meaning.e;
                        }
                    }
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e
                }
            },
            For => {
                let mut e = Effect::Pure;

                let iter = node.children.iter().map(|x| self.check(x.clone()));

                for _ in 0..3 {
                    if let Meaning { e: cond_effect, v: _, t: Type::Shard(_, _) } = iter.next().unwrap() {
                        if cond_effect < e {
                            e = cond_effect;
                        }
                    } else {
                        panic!("Type Error")
                    }
                }

                let block_effect = iter.next().unwrap().e;
                if block_effect < e {
                    e = block_effect;
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e
                }
            }
            ForEach => {
                let mut e = Effect::Pure;

                let iter = node.children.iter().map(|x| self.check(x.clone()));

                if let Meaning { e: cond_effect, v: _, t: Type::List(_) | Type::Tuple(_) | Type::String } = iter.next().unwrap() {
                    if cond_effect < e {
                        e = cond_effect;
                    }
                } else {
                    panic!("Type Error")
                }

                let block_effect = iter.next().unwrap().e;
                if block_effect < e {
                    e = block_effect;
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e
                }
            }
            Forever => {
                let mut e = Effect::Pure;

                let iter = node.children.iter().map(|x| self.check(x.clone()));

                let block_effect = iter.next().unwrap().e;
                if block_effect < e {
                    e = block_effect;
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e
                }
            }
            While => {
                let mut e = Effect::Pure;

                let iter = node.children.iter().map(|x| self.check(x.clone()));

                if let Meaning { e: cond_effect, v: _, t: Type::Shard(_, _) } = iter.next().unwrap() {
                    if cond_effect < e {
                        e = cond_effect;
                    }
                } else {
                    panic!("Type Error")
                }

                let block_effect = iter.next().unwrap().e;
                if block_effect < e {
                    e = block_effect;
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e
                }
            },
            Match => {
                let mut e = Effect::Pure;
                let mut match_t: Option<Type> = None;

                for child in node.children {
                    let child_meaning = self.check(child);

                    if !matches!(child.borrow().token.0, _Block) {
                        let case_t = self.check(child).t;

                        match match_t {
                            None => {
                                match_t = Some(case_t);
                            }
                            Some(match_t) if case_t != match_t => panic!("Type Error"),
                            _ => ()
                        };
                    }

                    if child_meaning.e > e {
                        e = child_meaning.e;
                    }
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e
                }
            },
            Shard => {
                let pr_local = Rc::clone(&self.local);
                self.local = Rc::new(RefCell::new(Table {
                    parent: None,
                    map: HashMap::new()
                }));
            }
            _ => todo!()
        }
    }
}
