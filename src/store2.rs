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
    Sym(ExprPtr<F>),
    Key(ExprPtr<F>),
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
            self.exprs.insert(ptr.clone(), img);
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
            self.exprs.insert(ptr.clone(), img);
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

    pub fn get_str(&mut self, str_ptr: &ExprPtr<F>) -> Result<String, &str> {
        let mut acc_str: String = Default::default();
        let mut ptr = str_ptr;
        loop {
            if *ptr
                == (ExprPtr {
                    tag: ExprTag::Str,
                    val: F::zero(),
                })
            {
                break;
            }
            match self.str_cache_inv.get(ptr) {
                Some(str) => {
                    acc_str.push_str(str.as_str());
                    break;
                }
                None => match self.exprs.get(&ptr) {
                    Some(ExprPtrImg::StrCons(char_ptr, tail_ptr)) => {
                        acc_str.push(char_ptr.val.to_char().unwrap());
                        self.str_cache.insert(acc_str.clone(), ptr.clone());
                        self.str_cache_inv.insert(ptr.clone(), acc_str.clone());
                        ptr = tail_ptr;
                    }
                    _ => return Err("Error when fetching string"),
                },
            }
        }
        Ok(acc_str)
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

    pub fn get_name(&mut self, name_ptr: &ExprPtr<F>) -> Result<Name, &str> {
        let mut acc_name: Name = Default::default();
        let mut ptr = name_ptr;
        loop {
            if *ptr
                == (ExprPtr {
                    tag: ExprTag::Name,
                    val: F::zero(),
                })
            {
                break;
            }
            match self.name_cache_inv.get(&ptr) {
                Some(name) => {
                    // acc_name.append(name);
                    break;
                }
                None => match self.exprs.get(&ptr) {
                    Some(ExprPtrImg::NameCons(str_ptr, tail_ptr)) => {
                        // let str: String = self.get_str(str_ptr)?;
                        // acc_name.push(str);
                        self.name_cache.insert(acc_name.clone(), ptr.clone());
                        self.name_cache_inv.insert(ptr.clone(), acc_name.clone());
                        ptr = tail_ptr;
                    }
                    _ => return Err("Error when fetching name"),
                },
            }
        }
        Ok(acc_name)
    }

    #[inline]
    pub fn put_sym(&mut self, sym_name: Name) -> ExprPtr<F> {
        let name_ptr = self.put_name(sym_name);
        let ptr = ExprPtr { tag: ExprTag::Sym, val: name_ptr.val };
        let img = ExprPtrImg::Sym(name_ptr);
        self.exprs.insert(ptr.clone(), img);
        ptr
    }

    #[inline]
    pub fn put_key(&mut self, key_name: Name) -> ExprPtr<F> {
        let name_ptr = self.put_name(key_name);
        let ptr = ExprPtr { tag: ExprTag::Key, val: name_ptr.val };
        let img = ExprPtrImg::Key(name_ptr);
        self.exprs.insert(ptr.clone(), img);
        ptr
    }

    #[inline]
    pub fn get_sym_key_name(&mut self, ptr: &ExprPtr<F>) -> Result<Name, &str> {
        self.get_name(
            &(ExprPtr {
                tag: ExprTag::Name,
                val: ptr.val,
            }),
        )
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
        self.exprs.insert(ptr.clone(), img);
        ptr
    }

    #[inline]
    pub fn get_cons(&self, cons_ptr: &ExprPtr<F>) -> Result<(&ExprPtr<F>, &ExprPtr<F>), &str> {
        match self.exprs.get(cons_ptr) {
            Some(ExprPtrImg::Cons(x, y)) => Ok((x, y)),
            _ => Err("Error fetching cons"),
        }
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
        self.exprs.insert(ptr.clone(), img);
        ptr
    }

    #[inline]
    pub fn get_fun(
        &self,
        fun_ptr: &ExprPtr<F>,
    ) -> Result<(&ExprPtr<F>, &ExprPtr<F>, &ExprPtr<F>), &str> {
        match self.exprs.get(fun_ptr) {
            Some(ExprPtrImg::Fun(x, y, z)) => Ok((x, y, z)),
            _ => Err("Error fetching function"),
        }
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
        self.exprs.insert(ptr.clone(), img);
        ptr
    }

    #[inline]
    pub fn get_thunk(&self, thunk_ptr: &ExprPtr<F>) -> Result<(&ExprPtr<F>, &ContPtr<F>), &str> {
        match self.exprs.get(thunk_ptr) {
            Some(ExprPtrImg::Thunk(x, y)) => Ok((x, y)),
            _ => Err("Error fetching thunk"),
        }
    }

    pub fn put_cont1(&mut self, tag: ContTag, expr: ExprPtr<F>, cont: ContPtr<F>) -> ContPtr<F> {
        let (expr_, cont_) = (expr.clone(), cont.clone());
        let preimage = [expr.tag.to_field(), expr.val, cont.tag.to_field(), cont.val];
        let hash = hash4(&preimage);
        let ptr = ContPtr { tag, val: hash };
        self.conts
            .insert(ptr.clone(), ContPtrImg::Cont1(expr_, cont_));
        ptr
    }

    #[inline]
    pub fn get_cont1(&self, cont_ptr: &ContPtr<F>) -> Result<(&ExprPtr<F>, &ContPtr<F>), &str> {
        match self.conts.get(cont_ptr) {
            Some(ContPtrImg::Cont1(x, k)) => Ok((x, k)),
            _ => Err("Error fetching cont1"),
        }
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
        self.conts
            .insert(ptr.clone(), ContPtrImg::Cont2(e1_, e2_, cont_));
        ptr
    }

    #[inline]
    pub fn get_cont2(
        &self,
        cont_ptr: &ContPtr<F>,
    ) -> Result<(&ExprPtr<F>, &ExprPtr<F>, &ContPtr<F>), &str> {
        match self.conts.get(cont_ptr) {
            Some(ContPtrImg::Cont2(x, y, k)) => Ok((x, y, k)),
            _ => Err("Error fetching cont2"),
        }
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
        self.conts
            .insert(ptr.clone(), ContPtrImg::Cont3(e1_, e2_, e3_, cont_));
        ptr
    }

    #[inline]
    pub fn get_cont3(
        &self,
        cont_ptr: &ContPtr<F>,
    ) -> Result<(&ExprPtr<F>, &ExprPtr<F>, &ExprPtr<F>, &ContPtr<F>), &str> {
        match self.conts.get(cont_ptr) {
            Some(ContPtrImg::Cont3(x, y, z, k)) => Ok((x, y, z, k)),
            _ => Err("Error fetching cont3"),
        }
    }

    #[inline]
    pub fn put_lurk_sym(&mut self, lurk_sym: &str) -> ExprPtr<F> {
        self.put_sym(vec![lurk_sym.to_string(), "lurk".to_string()])
    }
}