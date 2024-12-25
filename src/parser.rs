use core::panic;
use std::{cell::RefCell, rc::Rc, str::Chars, vec};

use crate::{
    lexer::{
        self, Lexer,
        Token::{self, *},
    },
    tree::{new_node, Node, NodeRc},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cursor: Token,
    tree: Rc<RefCell<Node>>,
    tree_cursor: Vec<Rc<RefCell<Node>>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: Chars<'a>) -> Self {
        let tree = Node {
            token: _SOF,
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
            tree: Rc::new(RefCell::new(tree)),
            tree_cursor: vec![],
        }
    }

    pub fn parse(&mut self) {
        while self.adv_cursor() != _EOF {
            if self.statement() {
                self.tree
                    .borrow_mut()
                    .children
                    .extend(self.tree_cursor.clone());
            } else {
                panic!("Syntax Error")
            }
        }

        println!("{:?}", self.tree);
    }

    fn adv_cursor(&mut self) -> Token {
        self.cursor = self.lexer.next().unwrap();

        self.cursor
    }

    fn statement(&mut self) -> bool {
        if self.precedence0() {
            if self.cursor == Semicolon {
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Mailbox {
            self.adv_cursor();
            let mailbox = new_node(Mailbox);
            if self.precedence0() {
                mailbox.extend(self.tree_cursor.clone());
                if self.cursor == Semicolon {
                    self.tree_cursor = vec![mailbox];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == Break {
            if self.adv_cursor() == Semicolon {
                self.tree_cursor = vec![new_node(Break)];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == FatRArrow {
            if self.adv_cursor() == Semicolon {
                self.tree_cursor = vec![new_node(FatRArrow)];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == If {
            let if_tree = new_node(If);
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    if_tree.extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            if_tree.insert(_Block).extend(self.tree_cursor.clone());
                            if self.elif() {
                                if_tree.extend(self.tree_cursor.clone());
                                if self.cursor == Semicolon {
                                    self.tree_cursor = vec![if_tree];
                                    true
                                } else if self.cursor == Else {
                                    self.adv_cursor();
                                    if self.block() {
                                        if_tree.insert(_Block).extend(self.tree_cursor.clone());
                                        if self.cursor == Semicolon {
                                            self.tree_cursor = vec![if_tree];
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
            let for_tree = new_node(For);
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    for_tree.extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            for_tree.insert(_Block).extend(self.tree_cursor.clone());

                            if self.cursor == Semicolon {
                                self.tree_cursor = vec![for_tree];
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
        } else if self.cursor == ForEach {
            let foreach_tree = new_node(ForEach);
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    foreach_tree.extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            foreach_tree.insert(_Block).extend(self.tree_cursor.clone());
                            if self.cursor == Semicolon {
                                self.tree_cursor = vec![foreach_tree];
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
            let forever_tree = new_node(Forever);

            self.adv_cursor();
            if self.block() {
                forever_tree.insert(_Block).extend(self.tree_cursor.clone());
                if self.cursor == Semicolon {
                    self.tree_cursor = vec![forever_tree];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == While {
            let while_tree = new_node(While);
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    while_tree.extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            while_tree.insert(_Block).extend(self.tree_cursor.clone());
                            if self.cursor == Semicolon {
                                self.tree_cursor = vec![while_tree];
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
            let match_tree = new_node(Match);
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    match_tree.extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.case() {
                            match_tree.extend(self.tree_cursor.clone());
                            if self.cursor == Semicolon {
                                self.tree_cursor = vec![match_tree];
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
            let try_tree = new_node(Try);
            self.adv_cursor();
            if self.block() {
                try_tree.insert(_Block).extend(self.tree_cursor.clone());
                if self.cursor == Except {
                    self.adv_cursor();
                    if self.block() {
                        try_tree.insert(_Block).extend(self.tree_cursor.clone());
                        if self.cursor == Semicolon {
                            self.tree_cursor = vec![try_tree];
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
            let shard_tree = new_node(Shard);
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.args() {
                    shard_tree.insert(_Args).extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            shard_tree.insert(_Block).extend(self.tree_cursor.clone());
                            if self.cursor == Semicolon {
                                if self.adv_cursor() == FatRArrow {
                                    self.adv_cursor();
                                    if self.value() {
                                        self.tree_cursor = vec![shard_tree];
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
        } else if self.cursor == Aggr {
            let aggr_tree = new_node(Aggr);

            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.args() {
                    aggr_tree.extend(self.tree_cursor.clone());

                    if self.cursor == RRound {
                        if self.adv_cursor() == Semicolon {
                            self.tree_cursor = vec![aggr_tree];
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
        } else if self.cursor == Mod {
            let mod_tree = new_node(Mod);

            if self.adv_cursor() == ID {
                self.adv_cursor();
                if self.block() {
                    mod_tree.extend(self.tree_cursor.clone());
                    if self.cursor == Semicolon {
                        self.tree_cursor = vec![mod_tree];
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
            let get_tree = new_node(Get);

            if self.adv_cursor() == Str {
                get_tree.insert(Str);
                if self.adv_cursor() == Semicolon {
                    self.tree_cursor = vec![get_tree];
                    true
                } else if self.cursor == As {
                    if self.adv_cursor() == Str {
                        get_tree.insert(Str);
                        if self.adv_cursor() == Semicolon {
                            self.tree_cursor = vec![get_tree];
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
            self.tree_cursor = vec![];
            false
        }
    }

    fn args(&mut self) -> bool {
        if self.precedence0() {
            let mut args = self.tree_cursor.clone();
            if self.cursor == Semicolon {
                self.adv_cursor();
                if self.args() {
                    args.extend(self.tree_cursor.clone());
                    self.tree_cursor = args;
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_cursor = vec![];
            true
        }
    }

    fn case(&mut self) -> bool {
        if self.cursor == Case {
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.value() {
                    let mut cases = self.tree_cursor.clone();
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            cases.extend(self.tree_cursor.clone());
                            if self.cursor == Semicolon {
                                self.adv_cursor();
                                if self.case() {
                                    cases.extend(self.tree_cursor.clone());
                                    self.tree_cursor = cases;
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
            self.tree_cursor = vec![];
            true
        }
    }

    fn elif(&mut self) -> bool {
        if self.cursor == Elif {
            let mut elifs = vec![];
            if self.adv_cursor() == LRound {
                self.adv_cursor();
                if self.precedence0() {
                    elifs.extend(self.tree_cursor.clone());
                    if self.cursor == RRound {
                        self.adv_cursor();
                        if self.block() {
                            let elif_block = new_node(_Block);
                            elif_block.extend(self.tree_cursor.clone());
                            elifs.push(elif_block);
                            if self.elif() {
                                elifs.extend(self.tree_cursor.clone());
                                self.tree_cursor = elifs;
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
                panic!("Syntax Error")
            }
        } else {
            self.tree_cursor = vec![];
            true
        }
    }

    fn block(&mut self) -> bool {
        if self.statement() {
            let mut block = self.tree_cursor.clone();
            if self.block() {
                block.extend(self.tree_cursor.clone());
                self.tree_cursor = block;
                self.adv_cursor();
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_cursor = vec![];
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
            self.tree_cursor = vec![];
            false
        }
    }

    fn precedence0prime(&mut self) -> bool {
        if self.cursor == RArrow {
            let push = new_node(RArrow);

            push.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence0() {
                push.extend(self.tree_cursor.clone());

                self.tree_cursor = vec![push];
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
            let or = new_node(Or);

            or.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence1() {
                or.extend(self.tree_cursor.clone());

                self.tree_cursor = vec![or];
                true
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
            let and = new_node(And);

            and.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence3() {
                if self.precedence2prime() {
                    and.extend(self.tree_cursor.clone());

                    self.tree_cursor = vec![and];
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

            let not = new_node(Exclam);

            if self.precedence4() {
                not.extend(self.tree_cursor.clone());

                self.tree_cursor = vec![not];
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
            let eq = new_node(self.cursor);

            eq.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence5() {
                if self.precedence4prime() {
                    eq.extend(self.tree_cursor.clone());

                    self.tree_cursor = vec![eq];
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
            let add = new_node(self.cursor);

            add.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence6() {
                if self.precedence5prime() {
                    add.extend(self.tree_cursor.clone());

                    self.tree_cursor = vec![add];
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
            let mul = new_node(self.cursor);

            mul.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence7() {
                if self.precedence6prime() {
                    mul.extend(self.tree_cursor.clone());

                    self.tree_cursor = vec![mul];
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
            let pow = new_node(Power);

            pow.extend(self.tree_cursor.clone());

            self.adv_cursor();
            if self.precedence8() {
                if self.precedence7prime() {
                    pow.extend(self.tree_cursor.clone());

                    self.tree_cursor = vec![pow];
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
            self.tree_cursor = vec![];
            false
        }
    }

    fn value(&mut self) -> bool {
        if [Number, Str, Bool, Nothing].contains(&self.cursor) {
            self.tree_cursor = vec![new_node(self.cursor)];
            self.adv_cursor();
            true
        } else if self.cursor == ID {
            let id_tree = new_node(_Path);
            id_tree.extend(vec![new_node(ID)]);
            self.adv_cursor();
            if self.path() {
                id_tree.extend(self.tree_cursor.clone());
            }
            self.tree_cursor = vec![id_tree];
            true
        } else if self.cursor == Hash {
            let assign = new_node(Hash);
            if self.adv_cursor() == ID {
                let id_tree = assign.insert(_Path);
                id_tree.insert(ID);
                self.adv_cursor();
                if self.path() {
                    id_tree.extend(self.tree_cursor.clone());
                }

                self.tree_cursor = vec![assign];
                true
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == HashExclam {
            let def = new_node(HashExclam);
            if self.adv_cursor() == ID {
                def.insert(ID);

                self.adv_cursor();
                if self.anno() {
                    let anno = def.insert(_Anno);
                    anno.extend(self.tree_cursor.clone());
                    self.tree_cursor = vec![def];
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else if self.cursor == LSquare {
            if [Number, Str, Bool, Nothing].contains(&self.adv_cursor()) {
                self.adv_cursor();
                if self.item() {
                    let list = new_node(_List);

                    list.extend(self.tree_cursor.clone());

                    if self.adv_cursor() == RSquare {
                        self.tree_cursor = vec![list];

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
        } else if self.cursor == LCurly {
            if [Number, Str, Bool, Nothing].contains(&self.adv_cursor()) {
                self.adv_cursor();
                if self.item() {
                    let tuple = new_node(_Tuple);

                    tuple.extend(self.tree_cursor.clone());

                    if self.adv_cursor() == RCurly {
                        self.tree_cursor = vec![tuple];

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
            self.tree_cursor = vec![];
            false
        }
    }

    fn item(&mut self) -> bool {
        if self.cursor == Comma {
            if [Number, Str, Bool, Nothing].contains(&self.adv_cursor()) {
                let mut items = vec![new_node(self.cursor)];

                self.adv_cursor();

                if self.item() {
                    items.extend(self.tree_cursor.clone());
                    self.tree_cursor = items;
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_cursor = vec![];
            true
        }
    }

    fn path(&mut self) -> bool {
        if self.cursor == Colon {
            if self.adv_cursor() == ID {
                let mut path = vec![new_node(self.cursor)];

                self.adv_cursor();

                if self.path() {
                    path.extend(self.tree_cursor.clone());
                    self.tree_cursor = path;
                    true
                } else {
                    panic!("Syntax Error")
                }
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_cursor = vec![];
            true
        }
    }

    fn anno(&mut self) -> bool {
        if self.cursor == Annotation {
            let mut annos = vec![new_node(Annotation)];
            self.adv_cursor();
            if self.anno() {
                annos.extend(self.tree_cursor.clone());
                self.tree_cursor = annos;
                true
            } else {
                panic!("Syntax Error")
            }
        } else {
            self.tree_cursor = vec![];
            true
        }
    }
}
