use itertools::Itertools;
use lurk::field::LurkField;
use std::collections::BTreeMap;

type Name = Vec<String>;

fn hash4<F: LurkField>(preimage: &[F; 4]) -> F {
    todo!()
}

fn hash6<F: LurkField>(preimage: &[F; 6]) -> F {
    todo!()
}

fn hash8<F: LurkField>(preimage: &[F; 8]) -> F {
    todo!()
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprTag {
    Num,
    U64,
    Char,
    Str,
    Comm,
    Fun,
    Cons,
    Name,
    Sym,
    Key,
    Thunk,
}

impl ExprTag {
    pub fn to_field<F: LurkField>(self) -> F {
        todo!()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op1 {
    Car,
    Cdr,
    Atom,
    Emit,
    Open,
    Secret,
    Commit,
    Num,
    Comm,
    Char,
    Eval,
    U64,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op2 {
    Sum,
    Diff,
    Product,
    Quotient,
    Equal,
    NumEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Cons,
    StrCons,
    Begin,
    Hide,
    Modulo,
    Eval,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContTag {
    Outermost,
    Call0,
    Call,
    Call2,
    Tail,
    Error,
    Lookup,
    If,
    Let,
    LetRec,
    Dummy,
    Terminal,
    Emit,
    Unop(Op1),
    Binop(Op2),
    Binop2(Op2),
}

impl ContTag {
    pub fn to_field<F: LurkField>(self) -> F {
        todo!()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprPtr<F: LurkField> {
    tag: ExprTag,
    val: F,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ContPtr<F: LurkField> {
    tag: ContTag,
    val: F,
}

pub enum ExprPtrImg<F: LurkField> {
    Cons(ExprPtr<F>, ExprPtr<F>),
    StrCons(ExprPtr<F>, ExprPtr<F>),
    NameCons(ExprPtr<F>, ExprPtr<F>),
    Fun(ExprPtr<F>, ExprPtr<F>, ExprPtr<F>),
    Thunk(ExprPtr<F>, ContPtr<F>),
}

pub enum ContPtrImg<F: LurkField> {
    Cont1(ExprPtr<F>, ContPtr<F>),
    Cont2(ExprPtr<F>, ExprPtr<F>, ContPtr<F>),
    Cont3(ExprPtr<F>, ExprPtr<F>, ExprPtr<F>, ContPtr<F>),
}

pub struct Store<F: LurkField> {
    exprs: BTreeMap<ExprPtr<F>, ExprPtrImg<F>>,
    conts: BTreeMap<ContPtr<F>, ContPtrImg<F>>,
    comms: BTreeMap<F, ExprPtr<F>>,

    vec_char_cache: BTreeMap<Vec<char>, ExprPtr<F>>,
    vec_str_cache: BTreeMap<Vec<String>, ExprPtr<F>>,

    str_cache: BTreeMap<String, ExprPtr<F>>,
    str_cache_inv: BTreeMap<ExprPtr<F>, String>,

    name_cache: BTreeMap<Name, ExprPtr<F>>,
    name_cache_inv: BTreeMap<ExprPtr<F>, Name>,
}

impl<F: LurkField + std::cmp::Ord> Store<F> {
    pub fn put_expr(&mut self, ptr: ExprPtr<F>, img: ExprPtrImg<F>) {
        self.exprs.insert(ptr, img);
    }

    pub fn put_cont(&mut self, ptr: ContPtr<F>, img: ContPtrImg<F>) {
        self.conts.insert(ptr, img);
    }

    pub fn put_chars(&mut self, chars: Vec<char>) -> ExprPtr<F> {
        let mut ptr: ExprPtr<F>;
        let mut chars_rev = chars;
        chars_rev.reverse();
        let mut heads_acc: Vec<char> = vec![];
        loop {
            if chars_rev.is_empty() {
                ptr = ExprPtr {
                    tag: ExprTag::Str,
                    val: F::zero(),
                };
                break;
            }
            heads_acc.push(chars_rev.pop().unwrap());
            match self.vec_char_cache.get(&chars_rev) {
                Some(ptr_cache) => {
                    ptr = ptr_cache.clone();
                    break;
                }
                None => continue,
            }
        }
        while let Some(c) = heads_acc.pop() {
            let preimage = [
                ExprTag::Char.to_field(),
                F::from_char(c),
                ptr.tag.to_field(),
                ptr.val,
            ];
            let hash = hash4(&preimage);
            ptr = ExprPtr {
                tag: ExprTag::Str,
                val: hash,
            };
            let img = ExprPtrImg::StrCons(
                ExprPtr {
                    tag: ExprTag::Char,
                    val: F::from_char(c),
                },
                ptr.clone(),
            );
            self.put_expr(ptr.clone(), img);
            chars_rev.push(c);
            self.vec_char_cache.insert(chars_rev.clone(), ptr.clone());
        }
        ptr
    }

    pub fn put_strs(&mut self, strs: Vec<String>) -> ExprPtr<F> {
        let mut ptr: ExprPtr<F>;
        let mut strs_rev = strs;
        strs_rev.reverse();
        let mut heads_acc: Vec<String> = vec![];
        loop {
            if strs_rev.is_empty() {
                ptr = ExprPtr {
                    tag: ExprTag::Name,
                    val: F::zero(),
                };
                break;
            }
            heads_acc.push(strs_rev.pop().unwrap());
            match self.vec_str_cache.get(&strs_rev) {
                Some(ptr_cache) => {
                    ptr = ptr_cache.clone();
                    break;
                }
                None => continue,
            }
        }
        while let Some(s) = heads_acc.pop() {
            let name_ptr = self.put_chars(s.chars().collect_vec());
            let name_ptr_ = name_ptr.clone();
            let preimage = [
                name_ptr_.tag.to_field(),
                name_ptr_.val,
                ptr.tag.to_field(),
                ptr.val,
            ];
            let hash = hash4(&preimage);
            ptr = ExprPtr {
                tag: ExprTag::Name,
                val: hash,
            };
            let img = ExprPtrImg::NameCons(name_ptr, ptr.clone());
            self.put_expr(ptr.clone(), img);
            strs_rev.push(s);
            self.vec_str_cache.insert(strs_rev.clone(), ptr.clone());
        }
        ptr
    }

    pub fn put_str(&mut self, str: String) -> ExprPtr<F> {
        match self.str_cache.get(&str) {
            Some(ptr) => ptr.clone(),
            None => {
                let ptr = self.put_chars(str.chars().collect_vec());
                self.str_cache.insert(str.clone(), ptr.clone());
                self.str_cache_inv.insert(ptr.clone(), str);
                ptr
            }
        }
    }

    pub fn put_name(&mut self, name: Name) -> ExprPtr<F> {
        match self.name_cache.get(&name) {
            Some(ptr) => ptr.clone(),
            None => {
                let ptr = self.put_strs(name.clone());
                self.name_cache.insert(name.clone(), ptr.clone());
                self.name_cache_inv.insert(ptr.clone(), name);
                ptr
            }
        }
    }

    pub fn put_sym(&mut self, sym_name: Name) -> ExprPtr<F> {
        ExprPtr {
            tag: ExprTag::Sym,
            val: self.put_name(sym_name).val,
        }
    }

    pub fn put_key(&mut self, key_name: Name) -> ExprPtr<F> {
        ExprPtr {
            tag: ExprTag::Key,
            val: self.put_name(key_name).val,
        }
    }

    pub fn put_cons(&mut self, head: ExprPtr<F>, tail: ExprPtr<F>) -> ExprPtr<F> {
        let (head_, tail_) = (head.clone(), tail.clone());
        let preimage = [head.tag.to_field(), head.val, tail.tag.to_field(), tail.val];
        let hash = hash4(&preimage);
        let img = ExprPtrImg::Cons(head_, tail_);
        let ptr = ExprPtr {
            tag: ExprTag::Cons,
            val: hash,
        };
        self.put_expr(ptr.clone(), img);
        ptr
    }

    pub fn put_fun(&mut self, args: ExprPtr<F>, env: ExprPtr<F>, body: ExprPtr<F>) -> ExprPtr<F> {
        let (args_, env_, body_) = (args.clone(), env.clone(), body.clone());
        let preimage = [
            args.tag.to_field(),
            args.val,
            env.tag.to_field(),
            env.val,
            body.tag.to_field(),
            body.val,
        ];
        let hash = hash6(&preimage);
        let img = ExprPtrImg::Fun(args_, env_, body_);
        let ptr = ExprPtr {
            tag: ExprTag::Fun,
            val: hash,
        };
        self.put_expr(ptr.clone(), img);
        ptr
    }

    pub fn put_thunk(&mut self, expr: ExprPtr<F>, cont: ContPtr<F>) -> ExprPtr<F> {
        let (expr_, cont_) = (expr.clone(), cont.clone());
        let preimage = [expr.tag.to_field(), expr.val, cont.tag.to_field(), cont.val];
        let hash = hash4(&preimage);
        let img = ExprPtrImg::Thunk(expr_, cont_);
        let ptr = ExprPtr {
            tag: ExprTag::Thunk,
            val: hash,
        };
        self.put_expr(ptr.clone(), img);
        ptr
    }

    pub fn put_cont1(&mut self, tag: ContTag, expr: ExprPtr<F>, cont: ContPtr<F>) -> ContPtr<F> {
        let (expr_, cont_) = (expr.clone(), cont.clone());
        let preimage = [expr.tag.to_field(), expr.val, cont.tag.to_field(), cont.val];
        let hash = hash4(&preimage);
        let ptr = ContPtr { tag, val: hash };
        self.put_cont(ptr.clone(), ContPtrImg::Cont1(expr_, cont_));
        ptr
    }

    pub fn put_cont2(
        &mut self,
        tag: ContTag,
        e1: ExprPtr<F>,
        e2: ExprPtr<F>,
        cont: ContPtr<F>,
    ) -> ContPtr<F> {
        let (e1_, e2_, cont_) = (e1.clone(), e2.clone(), cont.clone());
        let preimage = [
            e1.tag.to_field(),
            e1.val,
            e2.tag.to_field(),
            e2.val,
            cont.tag.to_field(),
            cont.val,
        ];
        let hash = hash6(&preimage);
        let ptr = ContPtr { tag, val: hash };
        self.put_cont(ptr.clone(), ContPtrImg::Cont2(e1_, e2_, cont_));
        ptr
    }

    pub fn put_cont3(
        &mut self,
        tag: ContTag,
        e1: ExprPtr<F>,
        e2: ExprPtr<F>,
        e3: ExprPtr<F>,
        cont: ContPtr<F>,
    ) -> ContPtr<F> {
        let (e1_, e2_, e3_, cont_) = (e1.clone(), e2.clone(), e3.clone(), cont.clone());
        let preimage = [
            e1.tag.to_field(),
            e1.val,
            e2.tag.to_field(),
            e2.val,
            e3.tag.to_field(),
            e3.val,
            cont.tag.to_field(),
            cont.val,
        ];
        let hash = hash8(&preimage);
        let ptr = ContPtr { tag, val: hash };
        self.put_cont(ptr.clone(), ContPtrImg::Cont3(e1_, e2_, e3_, cont_));
        ptr
    }
}
