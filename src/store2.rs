use lurk::field::LurkField;
use std::collections::BTreeMap;

type Name = Vec<String>;

fn hash4<F: LurkField> (preimage: &[F; 4]) -> F {
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
    Sym,
    Key,
    Thunk,
}

impl ExprTag {
    pub fn to_field<F: LurkField>(self) -> F {
        todo!()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprPtr<F: LurkField> {
    tag: ExprTag,
    val: F,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct ContPtr<F: LurkField> {
    tag: ContTag,
    val: F,
}

pub enum ExprPtrImg<F: LurkField> {
    Cons(ExprPtr<F>, ExprPtr<F>),
    StrCons(ExprPtr<F>, ExprPtr<F>),
    SymCons(ExprPtr<F>, ExprPtr<F>),
    Fun(ExprPtr<F>, ExprPtr<F>, ExprPtr<F>),
    Thunk(ExprPtr<F>, ContPtr<F>),
}

pub enum ContPtrImg<F: LurkField> {
    Cont0(ContPtr<F>),
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
        let mut chars_rev = chars.clone();
        chars_rev.reverse();
        let mut heads_acc: Vec<char> = vec![];
        loop {
            if chars_rev.is_empty() {
                ptr = ExprPtr { tag: ExprTag::Str, val: F::zero() };
                break;
            }
            heads_acc.push(chars_rev.pop().unwrap());
            match self.vec_char_cache.get(&chars_rev) {
                Some(ptr_cache) => {
                    ptr = ptr_cache.clone();
                    break;
                },
                None => continue,
            }
        }
        while let Some(c) = heads_acc.pop() {
            let preimage = [ExprTag::Char.to_field(), F::from_char(c), ptr.tag.to_field(), ptr.val];
            let hash = hash4(&preimage);
            ptr = ExprPtr { tag: ExprTag::Str, val: hash };
            let img = ExprPtrImg::StrCons(ExprPtr { tag: ExprTag::Char, val: F::from_char(c) }, ptr.clone());
            self.put_expr(ptr.clone(), img);
            chars_rev.push(c);
            self.vec_char_cache.insert(chars_rev.clone(), ptr.clone());
        }
        ptr
    }
}
