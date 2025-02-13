use std::{cell::RefCell, rc::Rc, str::Chars, vec};

use crate::{
    lexer::{
        self, Lexer,
        Token::{self, *},
    },
    tree::{Node, NodeRc},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cursor: Token,
    buf: String,
    pub tree: Rc<RefCell<Node>>,
    tree_return: Vec<Rc<RefCell<Node>>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: Chars<'a>) -> Self {
        let tree = Node {
            token: (_SOF, String::new()),
            children: vec![],
            parent: None,
        };

        Parser {
            lexer: Lexer {
                source,
                state: lexer::State::Initial,
                buf: String::new(),
            },
            cursor: _SOF,
            buf: String::new(),
            tree: Rc::new(RefCell::new(tree)),
            tree_return: vec![],
        }
    }

    pub fn parse(&mut self) {
        while self.adv_cursor() != _EOF {
            if self.statement() {
                self.tree
                    .borrow_mut()
                    .children
                    .extend(self.tree_return.clone());
            } else {
                panic!("Syntax Error")
            }
        }

        println!("{:?}", self.tree);
    }

    fn adv_cursor(&mut self) -> Token {
        (self.cursor, self.buf) = self.lexer.next();

        self.cursor
    }

    fn statement(&mut self) -> bool {
        if self.precedence0() {
            println!("{:?}", self.cursor);
            if self.cursor == Semicolon {
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Mailbox {
            self.adv_cursor();
            let mailbox = Node::new(Mailbox, self.buf.clone());
            if self.precedence0() {
                mailbox.extend(self.tree_return.clone());
                if self.cursor == Semicolon {
                    self.tree_return = vec![mailbox];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Break {
            if self.adv_cursor() == Semicolon {
                self.tree_return = vec![Node::new(Break, self.buf.clone())];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == FatRArrow {
            if self.adv_cursor() == Semicolon {
                self.tree_return = vec![Node::new(FatRArrow, self.buf.clone())];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == If {
            let if_tree = Node::new(If, self.buf.clone());
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    if_tree.extend(self.tree_return.clone());
                    if self.cursor == RRound {
                        println!("h {:?}", self.cursor);
                        self.adv_cursor();
                        println!("g {:?}", self.cursor);
                        if self.block() {
                            println!("f {:?}", self.cursor);
                            if_tree
                                .insert(_Block, String::new())
                                .extend(self.tree_return.clone());
                            println!("c {:?}", self.cursor);
                            if self.elif() {
                                if_tree.extend(self.tree_return.clone());

                                println!("d {:?}", self.cursor);
                                if self.cursor == Semicolon {
                                    self.tree_return = vec![if_tree];
                                    true
                                } else if self.cursor == Else {
                                    self.adv_cursor();
                                    if self.block() {
                                        if_tree
                                            .insert(_Block, String::new())
                                            .extend(self.tree_return.clone());
                                        if self.cursor == Semicolon {
                                            self.tree_return = vec![if_tree];
                                            true
                                        } else {
                                            panic!("Syntax Error")
                                        }
                                    } else {
                                        panic!("Syntax Error")
                                    }
                                } else {
                                    panic!("Syntax Error")
                                }
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == For {
            let for_tree = Node::new(For, self.buf.clone());
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.value() {
                    for_tree.extend(self.tree_return.clone());
                    if self.cursor == Semicolon {
                        self.adv_cursor();
                        if self.precedence0() {
                            for_tree.extend(self.tree_return.clone());
                            if self.cursor == Semicolon {
                                self.adv_cursor();
                                if self.precedence0() {
                                    for_tree.extend(self.tree_return.clone());
                                    if self.cursor == RRound {
                                        self.adv_cursor();
                                        if self.block() {
                                            for_tree
                                                .insert(_Block, String::new())
                                                .extend(self.tree_return.clone());

                                            if self.cursor == Semicolon {
                                                self.tree_return = vec![for_tree];
                                                true
                                            } else {
                                                panic!("Syntax Error")
                                            }
                                        } else {
                                            panic!("Syntax Error")
                                        }
                                    } else {
                                        panic!("Syntax Error")
                                    }
                                } else {
                                    panic!("Syntax Error")
                                }
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == ForEach {
            let foreach_tree = Node::new(ForEach, self.buf.clone());
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    foreach_tree.extend(self.tree_return.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            foreach_tree
                                .insert(_Block, String::new())
                                .extend(self.tree_return.clone());
                            if self.cursor == Semicolon {
                                self.tree_return = vec![foreach_tree];
                                true
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Forever {
            let forever_tree = Node::new(Forever, self.buf.clone());

            self.adv_cursor();
            if self.block() {
                forever_tree
                    .insert(_Block, String::new())
                    .extend(self.tree_return.clone());
                if self.cursor == Semicolon {
                    self.tree_return = vec![forever_tree];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == While {
            let while_tree = Node::new(While, self.buf.clone());
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    while_tree.extend(self.tree_return.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            while_tree
                                .insert(_Block, String::new())
                                .extend(self.tree_return.clone());
                            if self.cursor == Semicolon {
                                self.tree_return = vec![while_tree];
                                true
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Match {
            let match_tree = Node::new(Match, self.buf.clone());
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    match_tree.extend(self.tree_return.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.case() {
                            match_tree.extend(self.tree_return.clone());
                            if self.cursor == Semicolon {
                                self.tree_return = vec![match_tree];
                                true
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Try {
            let try_tree = Node::new(Try, self.buf.clone());
            self.adv_cursor();
            if self.block() {
                try_tree
                    .insert(_Block, String::new())
                    .extend(self.tree_return.clone());
                if self.cursor == Except {
                    self.adv_cursor();
                    if self.block() {
                        try_tree
                            .insert(_Block, String::new())
                            .extend(self.tree_return.clone());
                        if self.cursor == Semicolon {
                            self.tree_return = vec![try_tree];
                            true
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Shard {
            let shard_tree = Node::new(Shard, self.buf.clone());
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.args() {
                    shard_tree
                        .insert(_Block, String::new())
                        .extend(self.tree_return.clone());
                    if self.cursor == RRound {
                        if self.adv_cursor() == FatRArrow {
                            if self.adv_cursor() == LRound {
                                self.adv_cursor();
                                if self.args() {
                                    shard_tree
                                        .insert(_Block, String::new())
                                        .extend(self.tree_return.clone());
                                    if self.cursor == RRound {
                                        self.adv_cursor();
                                        if self.block() {
                                            shard_tree
                                                .insert(_Block, String::new())
                                                .extend(self.tree_return.clone());
                                            if self.cursor == Semicolon {
                                                if self.adv_cursor() == RArrow {
                                                    self.adv_cursor();
                                                    if self.def() {
                                                        shard_tree.extend(self.tree_return.clone());
                                                        if self.cursor == Semicolon {
                                                            self.tree_return = vec![shard_tree];
                                                            true
                                                        } else {
                                                            panic!("Syntax Error")
                                                        }
                                                    } else {
                                                        panic!("Syntax Error")
                                                    }
                                                } else {
                                                    panic!("Syntax Error")
                                                }
                                            } else {
                                                panic!("Syntax Error")
                                            }
                                        } else {
                                            panic!("Syntax Error")
                                        }
                                    } else {
                                        panic!("Syntax Error")
                                    }
                                } else {
                                    panic!("Syntax Error")
                                }
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Mod {
            let mod_tree = Node::new(Mod, self.buf.clone());

            if self.adv_cursor() == ID {
                mod_tree.insert(ID, self.buf.clone());
                self.adv_cursor();
                if self.block() {
                    mod_tree.extend(self.tree_return.clone());
                    if self.cursor == Semicolon {
                        self.tree_return = vec![mod_tree];
                        true
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Get {
            let get_tree = Node::new(Get, self.buf.clone());

            if self.adv_cursor() == Str {
                get_tree.insert(Str, self.buf.clone());
                if self.adv_cursor() == Semicolon {
                    self.tree_return = vec![get_tree];
                    true
                } else if self.cursor == As {
                    if self.adv_cursor() == Str {
                        get_tree.insert(Str, self.buf.clone());
                        if self.adv_cursor() == Semicolon {
                            self.tree_return = vec![get_tree];
                            true
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            false
        }
    }

    fn args(&mut self) -> bool {
        if self.def() {
            let mut args = self.tree_return.clone();
            if self.cursor == Semicolon {
                self.adv_cursor();
                if self.args() {
                    args.extend(self.tree_return.clone());
                    self.tree_return = args;
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            true
        }
    }

    fn case(&mut self) -> bool {
        if self.cursor == Case {
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.value() {
                    let mut cases = self.tree_return.clone();
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            cases.push(
                                Node::new(_Block, String::new()).extend(self.tree_return.clone()),
                            );
                            if self.cursor == Semicolon {
                                self.adv_cursor();
                                if self.case() {
                                    cases.extend(self.tree_return.clone());
                                    self.tree_return = cases;
                                    true
                                } else {
                                    panic!("Syntax Error")
                                }
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            true
        }
    }

    fn elif(&mut self) -> bool {
        if self.cursor == Elif {
            let mut elifs = vec![];
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    elifs.extend(self.tree_return.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            let elif_block = Node::new(_Block, String::new());
                            elif_block.extend(self.tree_return.clone());
                            elifs.push(elif_block);
                            if self.elif() {
                                elifs.extend(self.tree_return.clone());
                                self.tree_return = elifs;
                                true
                            } else {
                                panic!("Syntax Error")
                            }
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            true
        }
    }

    fn block(&mut self) -> bool {
        println!("a {:?}", self.cursor);
        if self.statement() {
            let mut block = self.tree_return.clone();

            self.adv_cursor();
            if self.block() {
                block.extend(self.tree_return.clone());

                self.tree_return = block;
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            println!("e {:?}", self.cursor);
            self.tree_return = vec![];
            true
        }
    }

    fn precedence0(&mut self) -> bool {
        if self.precedence1() {
            if self.precedence0prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            false
        }
    }

    fn precedence0prime(&mut self) -> bool {
        if self.cursor == RArrow {
            let push = Node::new(RArrow, self.buf.clone());

            push.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence1() {
                push.extend(self.tree_return.clone());

                self.tree_return = vec![push];
                if self.precedence0prime() {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if [Dart, PlusDart, MinusDart, MulDart, DivDart, PowDart].contains(&self.cursor)
            && self.tree_return[0].borrow().token.0 == _Path
        {
            let dart = Node::new(self.cursor.clone(), self.buf.clone());

            dart.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence1() {
                dart.extend(self.tree_return.clone());

                self.tree_return = vec![dart];
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence1(&mut self) -> bool {
        if self.precedence2() {
            if self.precedence1prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence1prime(&mut self) -> bool {
        if self.cursor == Or {
            let or = Node::new(Or, self.buf.clone());

            or.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence2() {
                or.extend(self.tree_return.clone());

                self.tree_return = vec![or];
                if self.precedence0prime() {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence2(&mut self) -> bool {
        if self.precedence3() {
            if self.precedence2prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence2prime(&mut self) -> bool {
        if self.cursor == And {
            let and = Node::new(And, self.buf.clone());

            and.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence3() {
                and.extend(self.tree_return.clone());

                self.tree_return = vec![and];
                if self.precedence2prime() {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence3(&mut self) -> bool {
        if self.precedence4() {
            true
        } else if self.cursor == Exclam {
            self.adv_cursor();

            let not = Node::new(Exclam, self.buf.clone());

            if self.precedence4() {
                not.extend(self.tree_return.clone());

                self.tree_return = vec![not];
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence4(&mut self) -> bool {
        if self.precedence5() {
            if self.precedence4prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence4prime(&mut self) -> bool {
        if [Equal, NEqual, GreatThan, GreatEq, SmallThan, SmallEq].contains(&self.cursor) {
            let eq = Node::new(self.cursor, self.buf.clone());

            eq.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence5() {
                eq.extend(self.tree_return.clone());

                self.tree_return = vec![eq];
                if self.precedence4prime() {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence5(&mut self) -> bool {
        if self.precedence6() {
            if self.precedence5prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence5prime(&mut self) -> bool {
        if [Plus, Minus].contains(&self.cursor) {
            let add = Node::new(self.cursor, self.buf.clone());

            add.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence6() {
                add.extend(self.tree_return.clone());

                self.tree_return = vec![add];
                if self.precedence5prime() {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence6(&mut self) -> bool {
        if self.precedence7() {
            if self.precedence6prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence6prime(&mut self) -> bool {
        if [Asterisk, TiltBar].contains(&self.cursor) {
            let mul = Node::new(self.cursor, self.buf.clone());

            mul.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence7() {
                mul.extend(self.tree_return.clone());

                self.tree_return = vec![mul];
                if self.precedence6prime() {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence7(&mut self) -> bool {
        if self.precedence8() {
            if self.precedence7prime() {
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            false
        }
    }

    fn precedence7prime(&mut self) -> bool {
        if self.cursor == Power {
            let pow = Node::new(Power, self.buf.clone());

            pow.extend(self.tree_return.clone());

            self.adv_cursor();
            if self.precedence8() {
                if self.precedence7prime() {
                    pow.extend(self.tree_return.clone());

                    self.tree_return = vec![pow];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            true
        }
    }

    fn precedence8(&mut self) -> bool {
        if self.cursor == LRound {
            self.adv_cursor();
            if self.precedence0() {
                if self.cursor == RRound {
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.value() {
            true
        } else {
            self.tree_return = vec![];
            false
        }
    }

    fn value(&mut self) -> bool {
        if [Int, Float, Str, Bool, Nothing].contains(&self.cursor) {
            self.tree_return = vec![Node::new(self.cursor, self.buf.clone())];
            self.adv_cursor();
            true
        } else if self.cursor == ID {
            let id = Node::new(ID, self.buf.clone());
            self.adv_cursor();
            
            if self.path() {
                let path = Node::new(_Path, self.buf.clone());
                path.extend(vec![id]);
                path.extend(self.tree_return.clone());
                self.tree_return = vec![path];
            } else {
                self.tree_return = vec![id];
            }

            
            true
        } else if self.cursor == Hash {
            let assign = Node::new(Hash, self.buf.clone());
            if self.adv_cursor() == ID {
                let id = Node::new(ID, self.buf.clone());
                self.adv_cursor();
                
                if self.path() {
                    let path = assign.insert(_Path, self.buf.clone());
                    path.extend(vec![id]);
                    path.extend(self.tree_return.clone());
                } else {
                    assign.extend(vec![id]);
                }
                self.tree_return = vec![assign];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Amph {
            let assign = Node::new(Amph, self.buf.clone());
            if self.adv_cursor() == ID {
                let id = Node::new(ID, self.buf.clone());
                self.adv_cursor();

                if self.path() {
                    let path = assign.insert(_Path, self.buf.clone());
                    path.extend(vec![id]);
                    path.extend(self.tree_return.clone());
                } else {
                    assign.extend(vec![id]);
                }
                
                
                self.tree_return = vec![assign];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.def() {
            true
        } else if self.cursor == LSquare {
            if [Int, Float, Str, Bool, Nothing].contains(&self.adv_cursor()) {
                self.adv_cursor();
                if self.item() {
                    let list = Node::new(_List, String::new());

                    list.extend(self.tree_return.clone());

                    if self.cursor == RSquare {
                        self.adv_cursor();
                        if self.path() {
                            let path = Node::new(_Path, self.buf.clone());
                            path.extend(vec![list]);
                            path.extend(self.tree_return.clone());
                            self.tree_return = vec![path];
                        } else {
                            self.tree_return = vec![list];
                        }
                        true
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == LCurly {
            let tuple = Node::new(_Tuple, String::new());
            if [Int, Float, Str, Bool, Nothing].contains(&self.adv_cursor()) {
                tuple.insert(self.cursor, self.buf.clone());
                self.adv_cursor();
                if self.item() {
                    tuple.extend(self.tree_return.clone());

                    if self.cursor == RCurly {
                        self.adv_cursor();
                        if self.path() {
                            let path = Node::new(_Path, self.buf.clone());
                            path.extend(vec![tuple]);
                            path.extend(self.tree_return.clone());
                            self.tree_return = vec![path];
                        } else {
                            self.tree_return = vec![tuple];
                        }

                        true
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Sd {
            let lambda = Node::new(_Lambda, String::new());

            self.adv_cursor();
            if self.args() {
                lambda
                    .insert(_Block, String::new())
                    .extend(self.tree_return.clone());
                if self.cursor == Colon {
                    self.adv_cursor();
                    if self.precedence0() {
                        lambda.extend(self.tree_return.clone());

                        if self.cursor == Semicolon {
                            self.tree_return = vec![lambda];
                            self.adv_cursor();
                            true
                        } else {
                            panic!("Syntax Error")
                        }
                    } else {
                        panic!("Syntax Error")
                    }
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            false
        }
    }

    fn item(&mut self) -> bool {
        if self.cursor == Comma {
            if [Int, Float, Str, Bool, Nothing].contains(&self.adv_cursor()) {
                let mut items = vec![Node::new(self.cursor, self.buf.clone())];

                self.adv_cursor();

                if self.item() {
                    items.extend(self.tree_return.clone());
                    self.tree_return = items;
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            true
        }
    }

    fn path(&mut self) -> bool {
        if self.cursor == Colon {
            if [ID, Int].contains(&self.adv_cursor()) {
                println!("{:?}", self.cursor);
                let mut path = vec![Node::new(self.cursor, self.buf.clone())];

                self.adv_cursor();

                if self.path() {
                    path.extend(self.tree_return.clone());
                } 

                self.tree_return = path;
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            false
        }
    }

    fn def(&mut self) -> bool {
        if self.cursor == HashExclam {
            let def = Node::new(HashExclam, self.buf.clone());
            if self.adv_cursor() == ID {
                def.insert(ID, self.buf.clone());

                self.adv_cursor();
                if self.anno() {
                    def.extend(self.tree_return.clone());
                    self.tree_return = vec![def];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            false
        }
    }

    fn anno(&mut self) -> bool {
        if self.cursor == Annotation {
            let mut annos = vec![Node::new(Annotation, self.buf.clone())];
            self.adv_cursor();
            if self.anno() {
                annos.extend(self.tree_return.clone());
                self.tree_return = annos;
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_return = vec![];
            true
        }
    }
}
