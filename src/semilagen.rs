/*
Semantic Checking & Intermediate Language Generator
*/

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    default,
    rc::{Rc, Weak},
    vec,
};

use crate::{lexer::Token::*, tree::Node};

#[derive(Debug, PartialEq, Clone)]
enum Value {
    // Compiler guide
    Nothing,
    Outcome(Vec<Type>, Vec<Instruction>),
    Table,
    Anno(char, String),
    // State modifier
    Mailbox(Type, Box<Value>),
    Continue,
    Break,
    // Fixed size value
    Bool(bool),
    Int(i32),
    Float(f64),
    // Buffer
    String(String),
    // Memory
    Heap(String),
    Stack(u32),
}

#[derive(Debug, PartialEq, Clone)]
enum Type {
    // Invalid
    None,
    // Fixed size value
    Bool,
    Int,
    Float,
    // Dynamic size value
    Tuple(Vec<Type>),
    // Pointer
    Shard(Vec<Type>, Vec<Type>),
    // Buffer
    List(Box<Type>),
    String,
    Jack,
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

impl Type {
    fn bytesize(&self) -> u8 {
        match self {
            // Fixed size value
            Type::Bool => 1,
            Type::Int => 4,
            Type::Float => 8,
            // Dynamic size value
            Type::Tuple(types) => {
                let mut size = 0;
                for t in types {
                    size += t.bytesize();
                }
                size
            }
            // Pointer
            Type::Shard(_, _) => 4,
            // Buffer
            Type::List(_) => 16,
            Type::String => 16,
            Type::Jack => 16,
            _ => panic!("Unsizeable"),
        }
    }
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

#[derive(Debug, PartialEq, Clone)]
enum Instruction {
    Label(String),
    Alloc(u8),
    Mov(u8, i32),
    MovLabel(u8, String),
    LdrStack(u8, u32),
    StrStack(u32, u8),
    AllocStack(u32),
    PurgeStack(u32),
    Add(u8, u8),
    AddVal(u8, i32),
    Sub(u8, u8),
    SubVal(u8, i32),
    Mul(u8, u8),
    MulVal(u8, i32),
    Div(u8, u8),
    DivVal(u8, i32),
    Cp(u8, i32),
    Jp(u8, String), // 0 = No flags, 1 = EQ, 2 = GE, 3 = SE, 4 = GT, 5 = ST
    Ret,
}

pub struct Interpreter {
    global: Rc<RefCell<Table>>,
    local: Rc<RefCell<Table>>,
    alloc_heap: bool,
    heap: u32,
    stack: u32,
    registers: u8,
    instructions: Vec<Instruction>,
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Rc::new(RefCell::new(Table {
            parent: None,
            map: HashMap::new(),
        }));

        Interpreter {
            global: Rc::clone(&global),
            local: Rc::clone(&global),
            alloc_heap: true,
            heap: 0,
            stack: 0,
            registers: 0,
            instructions: Default::default(),
        }
    }

    pub fn check(&mut self, node_rc: Rc<RefCell<Node>>) -> Meaning {
        let mut node = node_rc.borrow_mut();
        println!("Check {:?}", node.token.0);
        match node.token.0 {
            _SOF | _Block => {
                let mut e = Effect::Pure;
                let mut returns: Vec<Type> = vec![];
                let mut instructions: Vec<Instruction> = vec![];

                for child in node.children.clone() {
                    let child_meaning = self.check(child);

                    match child_meaning.v {
                        Value::Mailbox(t, v) => {
                            returns.push(t);
                            match *v {
                                Value::Stack(addr) => {
                                    self.registers += 1;
                                    let start = self.stack + 1;
                                    self.stack += t.bytesize() as u32;
                                    
                                    for i in (start..self.stack).step_by(4) {
                                        self.instructions.push(Instruction::LdrStack(self.registers, addr));
                                        self.instructions.push(Instruction::StrStack(i, self.registers));
                                    }

                                    self.registers -= 1;
                                }
                                Value::Heap(addr) => {
                                    self.registers += 1;
                                    let start = self.stack + 1;
                                    self.stack += t.bytesize() as u32;
                                    
                                    for i in (start..self.stack).step_by(4) {
                                        self.instructions.push(Instruction::MovLabel(self.registers, addr));
                                        self.instructions.push(Instruction::StrStack(i, self.registers));
                                    }

                                    self.registers -= 1;
                                }
                                Value::Int(val) => {
                                    self.registers += 1;
                                    self.stack += 4;

                                    self.instructions.push(Instruction::Mov(self.registers, val));
                                    self.instructions.push(Instruction::StrStack(self.stack - 4, self.registers));

                                    self.registers -= 1;
                                }
                                Value::Bool(val) => {
                                    self.registers += 1;
                                    self.stack += 4;

                                    self.instructions.push(Instruction::Mov(self.registers, val as i32));
                                    self.instructions.push(Instruction::StrStack(self.stack - 4, self.registers));

                                    self.registers -= 1;
                                }
                                Value::Float(val) => {
                                    self.registers += 1;
                                    self.stack += 8;

                                    for i in (self.stack-8..self.stack).step_by(4) {
                                        self.instructions.push(Instruction::Mov(self.registers, i32::from_be_bytes(val.to_be_bytes()[i as usize..i as usize+4].try_into().unwrap())));
                                        self.instructions.push(Instruction::StrStack(self.stack - 4, self.registers));
                                    }

                                    self.registers -= 1;
                                }
                            }
                        }
                        Value::Outcome(t_vec, i_vec) => {
                            returns.extend(t_vec);
                            instructions.extend(i_vec);
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
                Meaning {
                    t: Type::None,
                    v: Value::Outcome(returns, instructions),
                    e,
                }
            }
            _List => {
                let mut inner_t = Type::None;
                let mut e = Effect::Pure;

                println!("whaaar {:?}", node.children.clone());

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
                                v: Value::Heap(_) | Value::Stack(_) | Value::Table,
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
                fn search(ast: &mut Interpreter, node: std::cell::RefMut<'_, Node>) -> Meaning {
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

                let pr_local = self.local.clone();
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.local = self.local.add_table(None);

                for child in node.children.clone() {
                    let child_meaning = self.check(Rc::clone(&child));

                    if let (_Block, _) | (_, Type::Bool) = (child.borrow().token.0, child_meaning.t)
                    {
                        match state {
                            0 => {
                                if let Value::Bool(b) = child_meaning.v {
                                    self.alloc_heap = pr_alloc_heap;
                                    self.stack = pr_stack;
                                    state = if b { 1 } else { 2 };
                                } else {
                                    self.alloc_heap = false;
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
                                    self.alloc_heap = pr_alloc_heap;
                                    self.stack = pr_stack;
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
                                    self.alloc_heap = false;
                                    state = 5;
                                }

                                if node.children.len() == 1 {}
                                i += 1;
                            }
                            4 => {
                                self.alloc_heap = pr_alloc_heap;
                                self.stack = pr_stack;
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

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

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
                let pr_local = self.local.clone();
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.local = self.local.add_table(None);
                self.alloc_heap = false;

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

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            ForEach => {
                let pr_local = self.local.clone();
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.local = self.local.add_table(None);
                self.alloc_heap = false;

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

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            Forever => {
                let pr_local = self.local.clone();
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.alloc_heap = false;
                self.local = self.local.add_table(None);

                let mut e = Effect::Pure;
                let mut iter = node.children.iter().map(|x| self.check(x.clone()));

                let block = iter.next().unwrap();
                if block.v != Value::Outcome(vec![]) {
                    panic!("Inconsistent Outcome Error")
                }
                if block.e < e {
                    e = block.e;
                }

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            While => {
                let pr_local = self.local.clone();
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.alloc_heap = false;
                self.local = self.local.add_table(None);

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

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            Match => {
                let mut e = Effect::Pure;
                let mut match_t: Option<Type> = None;

                let pr_local = self.local.clone();
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.local = self.local.add_table(None);
                self.alloc_heap = false;

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

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

                Meaning {
                    v: Value::Nothing,
                    t: Type::None,
                    e,
                }
            }
            Shard => {
                let def = self.check(Rc::clone(&node.children[2]));

                let pr_local = Rc::clone(&self.local);
                self.local = Rc::new(RefCell::new(Table {
                    parent: None,
                    map: HashMap::new(),
                }));
                let pr_alloc_heap = self.alloc_heap;
                let pr_stack = self.stack;

                self.local = self.local.add_table(None);
                self.alloc_heap = false;

                let mut args: Vec<Type> = vec![];
                let mut e = Effect::Pure;

                for arg in node.children[0].borrow().children.clone() {
                    let arg_meaning = self.check(arg);

                    if let Type::Shard(arg, _) = arg_meaning.t {
                        args.extend(arg);
                    }
                }

                let block_effect = self.check(Rc::clone(&node.children[2])).e;
                if block_effect > e {
                    e = block_effect;
                }

                self.local = pr_local;
                self.alloc_heap = pr_alloc_heap;
                self.stack = pr_stack;

                match def.t {
                    Type::Shard(sig, _) => match &sig[0] {
                        Type::Shard(args_sig, _) if args_sig != &args => panic!("Type Error"),
                        _ => (),
                    },
                    _ => (),
                }

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
                        Value::Anno('t', typ) if t.is_none() => {
                            fn parse_type(typ: &str) -> Type {
                                match typ {
                                    "bool" => Type::Bool,
                                    "int" => Type::Int,
                                    "float" => Type::Float,
                                    "string" => Type::String,
                                    "jack" => Type::Jack,
                                    a if a.starts_with("tuple") => {
                                        let mut inner_types: Vec<Type> = vec![];
                                        let mut buf = String::new();
                                        let mut unclosed_brackets = 0;
                                        for ch in a[6..(a.len() - 1)].chars() {
                                            match ch {
                                                ',' | ')' if unclosed_brackets == 0 => {
                                                    inner_types.push(parse_type(buf.as_str()));
                                                    buf = String::new();
                                                }
                                                '(' => {
                                                    unclosed_brackets += 1;
                                                    buf.push(ch);
                                                }
                                                ')' => {
                                                    unclosed_brackets -= 1;
                                                    buf.push(ch);
                                                }
                                                _ => buf.push(ch),
                                            };
                                        }
                                        Type::Tuple(inner_types)
                                    }
                                    a if a.starts_with("list") => {
                                        println!("whatevs {} {}", a.len(), typ);
                                        Type::List(Box::new(parse_type(&a[5..(a.len() - 1)])))
                                    }
                                    a if a.starts_with("shard") => {
                                        let mut inner_types: [Vec<Type>; 2] = Default::default();
                                        let mut pool = 0;
                                        let mut buf = String::new();
                                        let mut unclosed_brackets = 0;
                                        for ch in a[5..].chars() {
                                            match ch {
                                                ',' | ')' if unclosed_brackets == 1 => {
                                                    inner_types[pool]
                                                        .push(parse_type(buf.as_str()));
                                                    buf = String::new();
                                                    if ch == ')' {
                                                        unclosed_brackets = 0;
                                                        pool = 1;
                                                    }
                                                }
                                                '(' => {
                                                    unclosed_brackets += 1;
                                                    if unclosed_brackets != 1 {
                                                        buf.push(ch);
                                                    }
                                                }
                                                ')' => {
                                                    unclosed_brackets -= 1;
                                                    buf.push(ch);
                                                }
                                                _ if unclosed_brackets == 0 => {
                                                    panic!("Annotation Error")
                                                }
                                                _ => buf.push(ch),
                                            };
                                        }
                                        Type::Shard(inner_types[0].clone(), inner_types[1].clone())
                                    }
                                    _ => panic!("Annotation Error {:?}", typ),
                                }
                            }
                            t = Some(parse_type(typ.as_str()));
                            println!("{:?}", t);
                        }
                        _ => panic!("Annotation Error"),
                    }
                }

                if t.is_none() || e.is_none() {
                    panic!("Annotation Error")
                }

                let v: Value;

                if self.alloc_heap {
                    v = Value::Heap(format!("L_{}", self.heap));
                    self.heap += 1;
                } else {
                    v = Value::Stack(self.stack);
                    self.stack += t.clone().unwrap().bytesize() as u32;
                }

                self.local.set(
                    id,
                    Symbol::Meaning(Meaning {
                        t: t.clone().unwrap(),
                        v,
                        e: e.unwrap(),
                    }),
                );

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
                                v: Value::Float(n1 as f64 + n2),
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
                                v: Value::Float(n1 as f64 - n2),
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
                                v: Value::Float(n1 as f64 * n2),
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
                                v: Value::Float(n1 as f64 / n2),
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
                            v: Value::Float((n1 as f64).powi(n2)),
                            e: Effect::Pure,
                        },
                        (Value::Int(n1), Value::Float(n2)) | (Value::Float(n2), Value::Int(n1)) => {
                            Meaning {
                                t: Type::Float,
                                v: Value::Float((n1 as f64).powf(n2)),
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
                            v: Value::Bool(n1 as f64 > n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 > n2 as f64),
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
                            v: Value::Bool(n1 as f64 >= n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 >= n2 as f64),
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
                            v: Value::Bool((n1 as f64) < n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 < n2 as f64),
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
                            v: Value::Bool(n1 as f64 <= n2),
                            e: Effect::Pure,
                        },
                        (Value::Float(n1), Value::Int(n2)) => Meaning {
                            t: Type::Bool,
                            v: Value::Bool(n1 <= n2 as f64),
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
