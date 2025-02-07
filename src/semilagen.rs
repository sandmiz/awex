/*
Semantic Checking & Intermediate Language Generator
*/

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::{lexer::Token::*, tree::Node};

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Outcome(Vec<Type>),
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    Nothing,
    Address(u32),
    Table,
    Anno(char, String),
    Mailbox(Type),
    Continue,
    Break,
}

#[derive(Debug, PartialEq, Clone)]
enum Type {
    Bool,
    Int,
    Float,
    String,
    None,
    List(Box<Type>),
    Tuple(Vec<Type>),
    Shard(Vec<Type>, Vec<Type>),
    Box(Box<Type>),
}

#[derive(PartialEq, Clone)]
enum Effect {
    Pure,
    Func,
    Obs,
    Proc,
}

#[derive(Clone)]
pub struct Meaning {
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

enum Instruction {
    Mov(u8, u32),

}

impl TableRc for Rc<RefCell<Table>> {
    fn get(&self, key: String) -> Option<Symbol> {
        if key == "_".to_owned() {
            match self.borrow().parent.clone() {
                Some(parent) => Some(Symbol::Table(parent.upgrade().unwrap())),
                None => None,
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
    can_init: bool,
    unique_id_count: u32,
}

impl ASTChecker {
    pub fn new() -> Self {
        let global = Rc::new(RefCell::new(Table {
            parent: None,
            map: HashMap::new(),
        }));

        ASTChecker {
            global: Rc::clone(&global),
            local: Rc::clone(&global),
            can_init: true,
            unique_id_count: 0,
        }
    }

    pub fn check(&mut self, node_rc: Rc<RefCell<Node>>) -> Meaning {
        let mut node = node_rc.borrow_mut();
        println!("Check {:?}", node.token.0);
        match node.token.0 {
            _SOF | _Block => {
                let mut e = Effect::Pure;
                let mut returns: Vec<Type> = vec![];

                for child in node.children.clone() {
                    let child_meaning = self.check(child);

                    match child_meaning.v {
                        Value::Mailbox(t) => {
                            returns.push(t);
                        }
                        Value::Outcome(t_vec) => {
                            returns.extend(t_vec);
                        }
                        Value::Break => {
                            break;
                        }
                        _ => (),
                    }

                    if child_meaning.e > e {
                        e = child_meaning.e;
                    }
                }

                println!("{:?}", returns);

                Meaning {
                    t: Type::None,
                    v: Value::Outcome(returns),
                    e,
                }
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
                        panic!("Type Error")
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
                let pr_local = Rc::clone(&self.local);
                let mut parent_meaning: Option<Meaning> = None;
                let iter = node.children.iter().map(|x| self.check(x.clone()));

                for child_meaning in iter {
                    match (parent_meaning, child_meaning.clone()) {
                        (
                            None,
                            Meaning {
                                t: _,
                                v: Value::Address(_) | Value::Table,
                                e: _,
                            }
                            | Meaning {
                                t: Type::List(_) | Type::Tuple(_),
                                v: _,
                                e: _,
                            },
                        ) => {
                            parent_meaning = Some(child_meaning);
                        }
                        (
                            Some(Meaning {
                                t: Type::List(t),
                                v: _,
                                e,
                            }),
                            Meaning {
                                t: Type::Int,
                                v: _,
                                e: _,
                            },
                        ) => {
                            parent_meaning = Some(Meaning {
                                t: *t,
                                v: Value::Nothing,
                                e,
                            });
                        }
                        _ => panic!("Type Error"),
                    }
                }

                self.local = pr_local;
                println!("{:?}", parent_meaning.clone().unwrap().v);
                parent_meaning.unwrap()
            }
            ID => {
                fn search(ast: &mut ASTChecker, node: std::cell::RefMut<'_, Node>) -> Meaning {
                    match ast.local.get(node.token.1.clone()) {
                        Some(Symbol::Table(table)) => {
                            ast.local = table;

                            Meaning {
                                t: Type::None,
                                v: Value::Table,
                                e: Effect::Pure,
                            }
                        }
                        Some(Symbol::Meaning(meaning)) => meaning,
                        None => {
                            if let Some(Symbol::Table(table)) = ast.local.get("_".to_owned()) {
                                ast.local = table;

                                search(ast, node)
                            } else if !Rc::ptr_eq(&ast.local, &ast.global) {
                                ast.local = Rc::clone(&ast.global);

                                search(ast, node)
                            } else {
                                panic!("Path Error");
                            }
                        }
                    }
                }
                search(self, node)
            }
            If => {
                let mut e = Effect::Pure;
                let mut state = 0;
                let mut i = 0;
                let mut outcome: Option<Value> = None;
                let mut has_else = false;

                for child in node.children.clone() {
                    let child_meaning = self.check(Rc::clone(&child));

                    if let (_Block, _) | (_, Type::Bool) = (child.borrow().token.0, child_meaning.t)
                    {
                        match state {
                            0 => {
                                if let Value::Bool(b) = child_meaning.v {
                                    self.can_init = true;
                                    state = if b { 1 } else { 2 };
                                } else {
                                    self.can_init = false;
                                    state = 3;
                                }
                                i += 1;
                                println!("{}", i);
                            }
                            1 => {
                                break;
                            }
                            2 => {
                                node.children =
                                    [&node.children[..(i - 1)], &node.children[(i + 1)..]].concat();
                                i -= 1;
                                state = 3;
                            }
                            3 | 5 => {
                                if let Value::Bool(b) = child_meaning.v {
                                    self.can_init = true;
                                    state = if b { 4 } else { 2 }
                                } else {
                                    if let Value::Outcome(_) = child_meaning.v {
                                        if state == 5 {
                                            has_else = true;
                                        }
                                        match outcome {
                                            None => {
                                                outcome = Some(child_meaning.v);
                                            }
                                            Some(block_out) if block_out == child_meaning.v => {
                                                outcome = Some(child_meaning.v);
                                            }
                                            _ => panic!("Inconsistent Outcome Error"),
                                        }
                                    }
                                    self.can_init = false;
                                    state = 5;
                                }

                                if node.children.len() == 1 {}
                                i += 1;
                            }
                            4 => {
                                self.can_init = true;
                                node.children = node.children[..i].to_vec();
                                if node.children.len() <= 2 {}
                                break;
                            }
                            _ => unimplemented!(),
                        }

                        if child_meaning.e > e {
                            e = child_meaning.e;
                        }
                    } else {
                        panic!("Type Error")
                    }
                }

                self.can_init = true;

                if outcome != Some(Value::Outcome(vec![])) && !has_else {
                    panic!("Inconsistent Outcome Error")
                }

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            For => {
                self.can_init = false;

                let mut e = Effect::Pure;
                let mut iter = node.children.iter().map(|x| self.check(x.clone()));

                for _ in 0..3 {
                    if let Meaning {
                        e: cond_effect,
                        v: _,
                        t: Type::Shard(_, _),
                    } = iter.next().unwrap()
                    {
                        if cond_effect < e {
                            e = cond_effect;
                        }
                    } else {
                        panic!("Type Error")
                    }
                }

                let block = iter.next().unwrap();
                if block.v != Value::Outcome(vec![]) {
                    panic!("Inconsistent Outcome Error")
                }
                if block.e < e {
                    e = block.e;
                }

                self.can_init = true;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            ForEach => {
                self.can_init = false;

                let mut e = Effect::Pure;
                let mut iter = node.children.iter().map(|x| self.check(x.clone()));

                if let Meaning {
                    e: cond_effect,
                    v: _,
                    t: Type::List(_) | Type::Tuple(_) | Type::String,
                } = iter.next().unwrap()
                {
                    if cond_effect < e {
                        e = cond_effect;
                    }
                } else {
                    panic!("Type Error")
                }

                let block = iter.next().unwrap();
                if block.v != Value::Outcome(vec![]) {
                    panic!("Inconsistent Outcome Error")
                }
                if block.e < e {
                    e = block.e;
                }

                self.can_init = true;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            Forever => {
                self.can_init = false;

                let mut e = Effect::Pure;
                let mut iter = node.children.iter().map(|x| self.check(x.clone()));

                let block = iter.next().unwrap();
                if block.v != Value::Outcome(vec![]) {
                    panic!("Inconsistent Outcome Error")
                }
                if block.e < e {
                    e = block.e;
                }

                self.can_init = true;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            While => {
                self.can_init = false;

                let mut e = Effect::Pure;
                let mut iter = node.children.iter().map(|x| self.check(x.clone()));

                if let Meaning {
                    e: cond_effect,
                    v: _,
                    t: Type::Shard(_, _),
                } = iter.next().unwrap()
                {
                    if cond_effect < e {
                        e = cond_effect;
                    }
                } else {
                    panic!("Type Error")
                }

                let block = iter.next().unwrap();
                if block.v != Value::Outcome(vec![]) {
                    panic!("Inconsistent Outcome Error")
                }
                if block.e < e {
                    e = block.e;
                }

                self.can_init = true;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            Match => {
                let mut e = Effect::Pure;
                let mut match_t: Option<Type> = None;

                self.can_init = false;

                for child in node.children.clone() {
                    let child_meaning = self.check(Rc::clone(&child));

                    if !matches!(child.borrow().token.0, _Block) {
                        let case_t = self.check(child).t;

                        match match_t {
                            None => {
                                match_t = Some(case_t);
                            }
                            Some(match_t) if case_t != match_t => panic!("Type Error"),
                            _ => (),
                        };
                    }

                    if child_meaning.e > e {
                        e = child_meaning.e;
                    }
                }

                self.can_init = true;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            Shard => {
                self.check(Rc::clone(&node.children[3]));

                let pr_local = Rc::clone(&self.local);
                self.local = Rc::new(RefCell::new(Table {
                    parent: None,
                    map: HashMap::new(),
                }));

                let mut args: Vec<Type> = vec![];
                let mut rets: Vec<Type> = vec![];
                let mut e = Effect::Pure;

                for arg in node.children[0].borrow().children.clone() {
                    let arg_meaning = self.check(arg);

                    if let Type::Shard(arg, _) = arg_meaning.t {
                        args.extend(arg);
                    }
                }

                for ret in node.children[1].borrow().children.clone() {
                    let ret_meaning = self.check(ret);

                    if let Type::Shard(ret, _) = ret_meaning.t {
                        rets.extend(ret);
                    }
                }

                let block_effect = self.check(Rc::clone(&node.children[2])).e;
                if block_effect > e {
                    e = block_effect;
                }

                self.local = pr_local;

                Meaning {
                    t: Type::None,
                    v: Value::Nothing,
                    e,
                }
            }
            Mailbox => {
                let ret_meaning = self.check(Rc::clone(&node.children[0]));
                Meaning {
                    t: Type::None,
                    v: Value::Mailbox(ret_meaning.t),
                    e: ret_meaning.e,
                }
            }
            FatRArrow => Meaning {
                t: Type::None,
                v: Value::Continue,
                e: Effect::Pure,
            },
            Break => Meaning {
                t: Type::None,
                v: Value::Break,
                e: Effect::Pure,
            },
            Mod => {
                let mut e = Effect::Pure;

                let pr_local = Rc::clone(&self.local);
                self.local = self
                    .local
                    .add_table(Some(node.children[0].borrow().token.1.clone()));

                let block_effect = self.check(Rc::clone(&node.children[1])).e;
                if block_effect > e {
                    e = block_effect;
                }

                self.local = pr_local;

                Meaning {
                    t: Type::None,
                    v: Value::Nothing,
                    e,
                }
            }
            Annotation => {
                let mut chars = node.token.1.chars();

                let key = chars.next().unwrap();

                let val: String = chars.skip(1).collect();

                Meaning {
                    t: Type::None,
                    v: Value::Anno(key, val),
                    e: Effect::Pure,
                }
            }
            Nothing => Meaning {
                t: Type::None,
                v: Value::Nothing,
                e: Effect::Pure,
            },
            Bool => Meaning {
                t: Type::Bool,
                v: Value::Bool(node.token.1.parse().unwrap()),
                e: Effect::Pure,
            },
            Int => Meaning {
                t: Type::Int,
                v: Value::Int(node.token.1.parse().unwrap()),
                e: Effect::Pure,
            },
            Float => Meaning {
                t: Type::Float,
                v: Value::Float(node.token.1.parse().unwrap()),
                e: Effect::Pure,
            },
            Str => Meaning {
                t: Type::String,
                v: Value::String(node.token.1.clone()),
                e: Effect::Pure,
            },
            HashExclam => {
                if !self.can_init {
                    panic!("Illegal Init Error")
                }
                let id = node.children[0].borrow().token.1.clone();
                let mut e: Option<Effect> = None;
                let mut t: Option<Type> = None;

                for anno in node
                    .children
                    .iter()
                    .skip(1)
                    .map(|x| self.check(Rc::clone(x)))
                {
                    println!("{:?}", anno.v);
                    match anno.v {
                        Value::Anno('e', effect) if e.is_none() => match effect.as_str() {
                            "pure" => e = Some(Effect::Pure),
                            "func" => e = Some(Effect::Func),
                            "obs" => e = Some(Effect::Obs),
                            "proc" => e = Some(Effect::Proc),
                            _ => panic!("Annotation Error"),
                        },
                        Value::Anno('t', typ) if t.is_none() => match typ.as_str() {
                            "bool" => t = Some(Type::Bool),
                            "int" => t = Some(Type::Int),
                            "float" => t = Some(Type::Float),
                            "string" => t = Some(Type::String),
                            _ => panic!("Annotation Error"),
                        },
                        _ => panic!("Annotation Error"),
                    }
                }

                if t.is_none() || e.is_none() {
                    panic!("Annotation Error")
                }

                self.local.set(
                    id,
                    Symbol::Meaning(Meaning {
                        t: t.clone().unwrap(),
                        v: Value::Address(self.unique_id_count),
                        e: e.unwrap(),
                    }),
                );

                self.unique_id_count += 1;

                Meaning {
                    t: Type::Shard(vec![t.clone().unwrap()], vec![t.unwrap()]),
                    v: Value::Nothing,
                    e: Effect::Func,
                }
            }
            Hash => {
                let var = self.check(Rc::clone(&node.children[0]));

                Meaning {
                    t: Type::Shard(vec![var.t.clone()], vec![var.t.clone()]),
                    v: Value::Nothing,
                    e: Effect::Func,
                }
            }
            RArrow => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                let r: Vec<Type>;

                match (op1.t, op2.t) {
                    (Type::Tuple(o), Type::Shard(i, ir)) if o == i => {
                        r = ir;
                    }
                    (o, Type::Shard(i, ir)) if vec![o.clone()] == i => {
                        r = ir;
                    }
                    _ => panic!("Type Error"),
                }

                Meaning {
                    t: Type::Tuple(r),
                    v: Value::Nothing,
                    e: if op1.e > op2.e { op1.e } else { op2.e },
                }
            }
            Plus => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                match (op1.t.clone(), op2.t.clone()) {
                    (Type::String, Type::String) => {
                        if let (Value::String(s1), Value::String(s2)) = (op1.v, op2.v) {
                            Meaning {
                                t: Type::String,
                                v: Value::String(s1 + s2.as_str()),
                                e: Effect::Pure,
                            }
                        } else {
                            Meaning {
                                t: Type::String,
                                v: Value::Nothing,
                                e: Effect::Pure,
                            }
                        }
                    }
                    (Type::Int | Type::Float, Type::Int | Type::Float) => match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Int,
                            v: Value::Int(n1 + n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) | (Value::Float(n2), Value::Int(n1)) => {
                            Meaning {
                                t: Type::Float,
                                v: Value::Float(n1 as f32 + n2),
                                e: Effect::Pure,
                            }
                        }
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Float,
                            v: Value::Float(n1 + n2),
                            e: Effect::Pure,
                        },
                        (v, Value::Int(0)) => Meaning {
                            t: op1.t.clone(),
                            v,
                            e: op1.e,
                        },
                        (Value::Int(0), v) => Meaning {
                            t: op2.t.clone(),
                            v,
                            e: op2.e,
                        },
                        (v, Value::Float(0.)) => Meaning {
                            t: Type::Float,
                            v,
                            e: op1.e,
                        },
                        (Value::Float(0.), v) => Meaning {
                            t: Type::Float,
                            v,
                            e: op2.e,
                        },

                        _ => Meaning {
                            t: Type::Float,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    },
                    (Type::List(t1), Type::List(t2)) if t1 == t2 => Meaning {
                        t: Type::List(t1),
                        v: Value::Nothing,
                        e: Effect::Pure,
                    },
                    _ => panic!("Type Error"),
                }
            }
            Minus => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                match (op1.t.clone(), op2.t.clone()) {
                    (Type::Int | Type::Float, Type::Int | Type::Float) => match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Int,
                            v: Value::Int(n1 - n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) | (Value::Float(n2), Value::Int(n1)) => {
                            Meaning {
                                t: Type::Float,
                                v: Value::Float(n1 as f32 - n2),
                                e: Effect::Pure,
                            }
                        }
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Float,
                            v: Value::Float(n1 - n2),
                            e: Effect::Pure,
                        },
                        (v1, v2) if v1 == v2 => Meaning {
                            t: Type::Int,
                            v: Value::Int(0),
                            e: Effect::Pure,
                        },
                        (v, Value::Int(0)) => Meaning {
                            t: op1.t,
                            v,
                            e: op1.e,
                        },
                        (v, Value::Float(0.)) => Meaning {
                            t: Type::Float,
                            v,
                            e: op1.e,
                        },
                        _ => Meaning {
                            t: Type::Float,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    },
                    _ => panic!("Type Error"),
                }
            }
            Asterisk => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                match (op1.t.clone(), op2.t.clone()) {
                    (Type::Int | Type::Float, Type::Int | Type::Float) => match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Int,
                            v: Value::Int(n1 * n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) | (Value::Float(n2), Value::Int(n1)) => {
                            Meaning {
                                t: Type::Float,
                                v: Value::Float(n1 as f32 * n2),
                                e: Effect::Pure,
                            }
                        }
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Float,
                            v: Value::Float(n1 * n2),
                            e: Effect::Pure,
                        },
                        (_, Value::Int(0)) if op1.t == Type::Int => Meaning {
                            t: Type::Int,
                            v: Value::Int(0),
                            e: Effect::Pure,
                        },
                        (Value::Int(0), _) if op2.t == Type::Int => Meaning {
                            t: Type::Int,
                            v: Value::Int(0),
                            e: Effect::Pure,
                        },
                        (_, Value::Int(0) | Value::Float(0.))
                        | (Value::Int(0) | Value::Float(0.), _) => Meaning {
                            t: Type::Int,
                            v: Value::Int(0),
                            e: Effect::Pure,
                        },
                        (v, Value::Int(1)) => Meaning {
                            t: op1.t,
                            v,
                            e: op1.e,
                        },
                        (Value::Int(1), v) => Meaning {
                            t: op2.t,
                            v,
                            e: op2.e,
                        },
                        (v, Value::Float(1.)) => Meaning {
                            t: Type::Float,
                            v,
                            e: op1.e,
                        },
                        (Value::Float(1.), v) => Meaning {
                            t: Type::Float,
                            v,
                            e: op2.e,
                        },

                        _ => Meaning {
                            t: Type::Float,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    },
                    _ => panic!("Type Error"),
                }
            }
            TiltBar => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                match (op1.t.clone(), op2.t.clone()) {
                    (Type::Int | Type::Float, Type::Int | Type::Float) => match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Int,
                            v: Value::Int(n1 / n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) | (Value::Float(n2), Value::Int(n1)) => {
                            Meaning {
                                t: Type::Float,
                                v: Value::Float(n1 as f32 / n2),
                                e: Effect::Pure,
                            }
                        }
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Float,
                            v: Value::Float(n1 / n2),
                            e: Effect::Pure,
                        },
                        (v1, v2) if v1 == v2 => Meaning {
                            t: op1.t,
                            v: Value::Float(1.),
                            e: Effect::Pure,
                        },
                        (Value::Int(0), _) => Meaning {
                            t: Type::Int,
                            v: Value::Int(0),
                            e: Effect::Pure,
                        },
                        (Value::Float(0.), _) => Meaning {
                            t: Type::Float,
                            v: Value::Float(0.),
                            e: Effect::Pure,
                        },
                        (_, Value::Int(0) | Value::Float(0.)) => panic!("Zero Division Error"),
                        (v, Value::Int(1)) => Meaning {
                            t: op1.t,
                            v,
                            e: op1.e,
                        },
                        (v, Value::Float(1.)) => Meaning {
                            t: Type::Float,
                            v,
                            e: op1.e,
                        },
                        _ => Meaning {
                            t: Type::Float,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    },
                    _ => panic!("Type Error"),
                }
            }
            Power => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                match (op1.t, op2.t) {
                    (Type::Int | Type::Float, Type::Int | Type::Float) => match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Int,
                            v: Value::Float((n1 as f32).powi(n2)),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) | (Value::Float(n2), Value::Int(n1)) => {
                            Meaning {
                                t: Type::Float,
                                v: Value::Float((n1 as f32).powf(n2)),
                                e: Effect::Pure,
                            }
                        }
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Float,
                            v: Value::Float(n1.powf(n2)),
                            e: Effect::Pure,
                        },
                        _ => Meaning {
                            t: Type::Float,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    },
                    _ => panic!("Type Error"),
                }
            }
            Equal => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == op2.t {
                    if op1.v != Value::Nothing && op2.v != Value::Nothing {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Bool(op1.v == op1.v),
                            e: Effect::Pure,
                        }
                    } else {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        }
                    }
                } else {
                    panic!("Type Error");
                }
            }
            NEqual => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == op2.t {
                    if op1.v != Value::Nothing && op2.v != Value::Nothing {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Bool(op1.v != op1.v),
                            e: Effect::Pure,
                        }
                    } else {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        }
                    }
                } else {
                    panic!("Type Error");
                }
            }
            GreatThan => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == Type::Int
                    || op1.t == Type::Float && op2.t == Type::Int
                    || op2.t == Type::Float
                {
                    match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 > n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 as f32 > n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 > n2 as f32),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 > n2),
                            e: Effect::Pure,
                        },
                        _ => Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    }
                } else {
                    panic!("Type Error");
                }
            }
            GreatEq => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == Type::Int
                    || op1.t == Type::Float && op2.t == Type::Int
                    || op2.t == Type::Float
                {
                    match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 >= n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 as f32 >= n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 >= n2 as f32),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 >= n2),
                            e: Effect::Pure,
                        },
                        _ => Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    }
                } else {
                    panic!("Type Error");
                }
            }
            SmallThan => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == Type::Int
                    || op1.t == Type::Float && op2.t == Type::Int
                    || op2.t == Type::Float
                {
                    match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 < n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool((n1 as f32) < n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 < n2 as f32),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 < n2),
                            e: Effect::Pure,
                        },
                        _ => Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    }
                } else {
                    panic!("Type Error");
                }
            }
            SmallEq => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == Type::Int
                    || op1.t == Type::Float && op2.t == Type::Int
                    || op2.t == Type::Float
                {
                    match (op1.v, op2.v) {
                        (Value::Int(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 <= n2),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 as f32 <= n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 <= n2 as f32),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Float(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 <= n2),
                            e: Effect::Pure,
                        },
                        _ => Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        },
                    }
                } else {
                    panic!("Type Error");
                }
            }
            Or => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == Type::Bool && op2.t == Type::Bool {
                    if let (Value::Bool(b1), Value::Bool(b2)) = (op1.v, op2.v) {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Bool(b1 || b2),
                            e: Effect::Pure,
                        }
                    } else {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        }
                    }
                } else {
                    panic!("Type Error");
                }
            }
            And => {
                let op1 = self.check(Rc::clone(&node.children[0]));
                let op2 = self.check(Rc::clone(&node.children[1]));

                if op1.t == op2.t {
                    if let (Value::Bool(b1), Value::Bool(b2)) = (op1.v, op2.v) {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Bool(b1 && b2),
                            e: Effect::Pure,
                        }
                    } else {
                        Meaning {
                            t: Type::Bool,
                            v: Value::Nothing,
                            e: Effect::Pure,
                        }
                    }
                } else {
                    panic!("Type Error");
                }
            }
            Exclam => {
                let op1 = self.check(Rc::clone(&node.children[0]));

                if let Value::Bool(b) = op1.v {
                    Meaning {
                        t: Type::Bool,
                        v: Value::Bool(!b),
                        e: Effect::Pure,
                    }
                } else {
                    Meaning {
                        t: Type::Bool,
                        v: Value::Nothing,
                        e: Effect::Pure,
                    }
                }
            }
            _ => todo!(),
        }
    }
}
