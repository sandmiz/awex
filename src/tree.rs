use crate::lexer::Token;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct Node {
    pub token: (Token, String),
    pub children: Vec<Rc<RefCell<Node>>>,
    pub parent: Option<Weak<RefCell<Node>>>,
}

pub trait NodeRc {
    fn insert(&self, token: Token, value: String) -> Rc<RefCell<Node>>;
    fn extend(&self, nodes: Vec<Rc<RefCell<Node>>>) -> Rc<RefCell<Node>>;
}

impl std::fmt::Debug for Node {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        let res = write!(formatter, "{:?}", self.token);

        if &self.children.len() > &0 {
            let children: Vec<String> = self
                .children
                .iter()
                .map(|s| format!("{:?}", s.borrow()))
                .collect();
            write!(
                formatter,
                "\n   {}",
                children.join("\n").replace("\n", "\n   ")
            )?;
        }

        res
    }
}

impl NodeRc for Rc<RefCell<Node>> {
    fn insert(&self, token: Token, value: String) -> Rc<RefCell<Node>> {
        let node = Node {
            token: (token, value),
            children: vec![],
            parent: Some(Rc::downgrade(self)),
        };

        let rc = Rc::new(RefCell::new(node));
        self.borrow_mut().children.push(Rc::clone(&rc));

        rc
    }

    fn extend(&self, nodes: Vec<Rc<RefCell<Node>>>) -> Rc<RefCell<Node>> {
        for node in nodes.clone() {
            node.borrow_mut().parent = Some(Rc::downgrade(self));
        }

        self.borrow_mut().children.extend(nodes);

        Rc::clone(self)
    }
}

impl Node {
    pub fn new(token: Token, value: String) -> Rc<RefCell<Self>> {
        let node = Node {
            token: (token, value),
            children: vec![],
            parent: None,
        };

        Rc::new(RefCell::new(node))
    }
}
