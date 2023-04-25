#![doc = include_str!("../README.md")]

use anyhow::{anyhow, bail, Context, Result};
use clap::{Arg, ArgAction, Command};
use pasta_curves::pallas;

use fcomm::{
    public_params, Claim, Commitment, CommittedExpression, CommittedExpressionMap, Id, LurkCont,
    LurkPtr, NovaProofCache, Opening, Proof, PtrEvaluation,
};
use lurk::coprocessor::Coprocessor;
use lurk::eval::{
    lang::{Coproc, Lang},
    Evaluable, Status, Witness, IO,
};
use lurk::expr::Expression;
use lurk::field::LurkField;
use lurk::package::Package;
use lurk::proof::{nova::NovaProver, Prover};
use lurk::ptr::Ptr;
use lurk::repl::{ReplState, ReplTrait};
use lurk::store::Store;
use lurk::sym::Sym;
use lurk::tag::ExprTag;
use lurk::writer::Write;

use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::thread;

const DEFAULT_REDUCTION_COUNT: usize = 10;

#[derive(Clone, Debug)]
struct Demo {
    inputs: Vec<String>,
    index: usize,
}

impl Demo {
    fn new(inputs: Vec<String>) -> Self {
        Demo { inputs, index: 0 }
    }

    fn new_from_path<P: AsRef<Path>>(p: P, base_prompt_length: usize) -> Self {
        let file = File::open(p.as_ref()).unwrap();
        let lines = io::BufReader::new(file).lines();

        let mut inputs = Vec::new();

        let mut current_input = Vec::new();
        let indent = " ".to_string().repeat(base_prompt_length);

        macro_rules! finalize_current {
            () => {
                if !current_input.is_empty() {
                    let input = current_input.join("\n");
                    inputs.push(input);
                    Vec::new()
                } else {
                    current_input
                }
            };
        }

        for line in lines {
            let line = line.unwrap();
            if line.is_empty() {
                current_input = finalize_current!();
            } else {
                let input_line = if current_input.is_empty() {
                    line
                } else {
                    indent.clone() + &line
                };
                current_input.push(input_line);
            }
        }

        finalize_current!();

        Self::new(inputs)
    }

    fn next_input(&mut self) -> Option<&String> {
        let input = self.inputs.get(self.index);
        if input.is_some() {
            self.index += 1;
        }
        input
    }
    fn peek_input(&self) -> Option<&String> {
        self.inputs.get(self.index)
    }
}

pub struct ClutchState<F: LurkField, C: Coprocessor<F>> {
    repl_state: ReplState<F, C>,
    reduction_count: usize,
    history: Vec<IO<F>>,
    proof_map: NovaProofCache,
    expression_map: CommittedExpressionMap,
    last_claim: Option<Claim<F>>,
    demo: Option<Demo>,
}

type F = pallas::Scalar;

impl<F: LurkField, C: Coprocessor<F>> ClutchState<F, C> {
    fn base_prompt() -> String {
        "\n!> ".into()
    }
}

impl ReplTrait<F, Coproc<F>> for ClutchState<F, Coproc<F>> {
    fn new(
        s: &mut Store<F>,
        limit: usize,
        command: Option<Command>,
        lang: Lang<F, Coproc<F>>,
    ) -> Self {
        let reduction_count = DEFAULT_REDUCTION_COUNT;

        let proof_map = fcomm::nova_proof_cache(reduction_count);
        let expression_map = fcomm::committed_expression_store();

        let demo = command.clone().and_then(|c| {
            let l = Self::base_prompt().trim_start_matches('\n').len();
            let matches = c.get_matches();

            matches
                .get_one::<String>("demo")
                .map(|demo_file| Demo::new_from_path(demo_file, l))
        });

        // Load params from disk cache, or generate them in the background.
        thread::spawn(move || public_params(reduction_count));

        Self {
            repl_state: ReplState::new(s, limit, command, lang),
            reduction_count,
            history: Default::default(),
            proof_map,
            expression_map,
            demo,
            last_claim: None,
        }
    }

    fn name() -> String {
        "Lurk Clutch".into()
    }

    fn prompt(&mut self) -> String {
        if let Some(demo_input) = self.demo.as_ref().and_then(|d: &Demo| d.peek_input()) {
            if demo_input.trim().is_empty() {
                "".to_string()
            } else {
                format!("{}{demo_input}", Self::base_prompt())
            }
        } else {
            Self::base_prompt()
        }
    }

    fn command() -> Command {
        ReplState::<F, Coproc<F>>::command().arg(
            Arg::new("demo")
                .long("demo")
                .value_parser(clap::builder::NonEmptyStringValueParser::new())
                .action(ArgAction::Set)
                .value_name("DEMO")
                .help("Specifies the demo file path"),
        )
    }

    fn process_line(&mut self, line: String) -> String {
        if !line.is_empty() {
            return line;
        };
        // If line is blank, get the next demo input.
        self.demo
            .as_mut()
            .and_then(Demo::next_input)
            .ok_or(&line)
            .unwrap_or(&line)
            .to_string()
    }

    fn handle_meta<P: AsRef<Path> + Copy>(
        &mut self,
        store: &mut Store<F>,
        expr_ptr: Ptr<F>,
        package: &Package,
        p: P,
    ) -> Result<()> {
        let expr = store.fetch(&expr_ptr).unwrap();

        macro_rules! delegate {
            () => {
                self.repl_state.handle_meta(store, expr_ptr, package, p)
            };
        }

        let res: Option<Ptr<F>> = match expr {
            Expression::Cons(car, rest) => match &store.fetch(&car).unwrap() {
                Expression::Sym(Sym::Sym(s)) => match s.name().as_str() {
                    "CALL" => self.call(store, rest)?,
                    "CHAIN" => self.chain(store, rest)?,
                    "COMMIT" => self.commit(store, rest)?,
                    "OPEN" => self.open(store, rest)?,
                    "PROOF-IN-EXPR" => self.proof_in_expr(store, rest)?,
                    "PROOF-OUT-EXPR" => self.proof_out_expr(store, rest)?,
                    "PROOF-CLAIM" => self.proof_claim(store, rest)?,
                    "PROVE" => self.prove(store, rest)?,
                    "VERIFY" => self.verify(store, rest)?,
                    _ => return delegate!(),
                },
                Expression::Comm(_, c) => {
                    // NOTE: this cannot happen from a text-based REPL, since there is not currrently a literal Comm syntax.
                    self.apply_comm(store, *c, rest)?
                }
                Expression::Num(c) => {
                    let comm = store.intern_num(*c);
                    self.apply_comm(store, comm, rest)?
                }
                _ => return delegate!(),
            },
            Expression::Num(n) => {
                let i = n.into_scalar().to_u64_unchecked();
                self.hist(i as usize).map(|io| io.expr)
            }
            _ => return delegate!(),
        };

        if let Some(expr) = res {
            let mut handle = io::stdout().lock();
            expr.fmt(store, &mut handle)?;
            println!();
        };
        Ok(())
    }

    fn handle_non_meta(
        &mut self,
        store: &mut Store<F>,
        expr_ptr: Ptr<F>,
    ) -> Result<(IO<F>, IO<F>, usize)> {
        let (input, output, iterations) = self.repl_state.handle_non_meta(store, expr_ptr)?;

        self.history.push(output);

        let claim = Claim::PtrEvaluation::<F>(PtrEvaluation {
            expr: LurkPtr::from_ptr(store, &input.expr),
            env: LurkPtr::from_ptr(store, &input.env),
            cont: LurkCont::from_cont_ptr(store, &input.cont),
            expr_out: LurkPtr::from_ptr(store, &output.expr),
            env_out: LurkPtr::from_ptr(store, &output.env),
            cont_out: LurkCont::from_cont_ptr(store, &output.cont),
            status: <lurk::eval::IO<F> as Evaluable<F, Witness<F>, Coproc<F>>>::status(&output),

            iterations: None,
        });

        self.last_claim = Some(claim);

        Ok((input, output, iterations))
    }
}

impl<F: LurkField, C: Coprocessor<F>> ClutchState<F, C> {
    fn hist(&self, n: usize) -> Option<&IO<F>> {
        self.history.get(n)
    }
}

impl ClutchState<F, Coproc<F>> {
    fn lang(&self) -> &Lang<F, Coproc<F>> {
        &self.repl_state.lang
    }
    fn commit(&mut self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let (first, rest) = store.car_cdr(&rest)?;
        let (second, _) = store.car_cdr(&rest)?;

        let (expr, secret) = if rest.is_nil() {
            // TODO: also support Commitment::from_ptr_with_hiding (randomized secret at runtime).
            (first, F::zero())
        } else if let Expression::Num(n) = store
            .fetch(&second)
            .ok_or_else(|| anyhow!("second arg to !:COMMIT must be a number."))?
        {
            (first, n.into_scalar())
        } else {
            bail!("Secret must be a Num")
        };

        let (evaled, _, _, _) = self.repl_state.eval_expr(expr, store)?;

        let commitment = Commitment::from_ptr_and_secret(store, &evaled, secret);

        let committed_expression = CommittedExpression {
            expr: LurkPtr::from_ptr(store, &evaled),
            secret: Some(secret),
            commitment: Some(commitment),
        };

        self.expression_map.set(commitment, &committed_expression)?;
        Ok(Some(store.intern_maybe_opaque_comm(commitment.comm)))
    }

    fn open(&self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        Ok(self.open_aux(store, rest)?.1)
    }

    fn apply_comm(
        &mut self,
        store: &mut Store<F>,
        comm: Ptr<F>,
        rest: Ptr<F>,
    ) -> Result<Option<Ptr<F>>> {
        let call_form = store.cons(comm, rest);
        self.apply_comm_aux(store, call_form, false)
    }
    fn call(&mut self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        self.apply_comm_aux(store, rest, false)
    }
    fn chain(&mut self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let (result_expr, new_comm) = self
            .apply_comm_aux(store, rest, true)?
            .and_then(|cons| store.car_cdr(&cons).ok()) // unpack pair
            .ok_or_else(|| {
                anyhow!(
                    "chained functional commitment output must be a pair (result, new-commitment)"
                )
            })?;

        let new_secret = store
            .secret(new_comm)
            .ok_or_else(|| anyhow!("secret missing"))
            .and_then(|hash| {
                store
                    .get_expr_hash(&hash)
                    .ok_or_else(|| anyhow!("hash missing"))
            })
            .map(|hash| *hash.value())?;

        let (_, new_fun) = store
            .open(new_comm)
            .ok_or_else(|| anyhow!("opening missing"))?;

        let new_commitment = Commitment::from_comm(store, &new_comm);

        let expr = LurkPtr::from_ptr(store, &new_fun);

        let new_function = CommittedExpression::<F> {
            expr,
            secret: Some(new_secret),
            commitment: Some(new_commitment),
        };

        self.expression_map.set(new_commitment, &new_function)?;

        let interned_commitment = store.intern_maybe_opaque_comm(new_commitment.comm);
        let mut handle = io::stdout().lock();
        interned_commitment.fmt(store, &mut handle)?;
        println!();

        Ok(Some(result_expr))
    }
    fn apply_comm_aux(
        &mut self,
        store: &mut Store<F>,
        rest: Ptr<F>,
        chain: bool,
    ) -> Result<Option<Ptr<F>>> {
        let args = store.cdr(&rest)?;
        let (commitment, Some(e)) = self.open_aux(store, rest)? else { bail!("failed to open") };
        let call = store.cons(e, args);
        let (arg, _) = store.car_cdr(&args)?;

        let (result, _iterations, cont, _) = self
            .repl_state
            .eval_expr(call, store)
            .with_context(|| "Evaluating call")?;

        let (output, new_commitment) = if chain {
            let (output, new_comm) = store.car_cdr(&result)?;
            (output, Some(Commitment::from_comm(store, &new_comm)))
        } else {
            (result, None)
        };

        let claim = Claim::Opening::<F>(Opening {
            input: arg.fmt_to_string(store),
            output: output.fmt_to_string(store),
            status: Status::from(cont),
            commitment,
            new_commitment,
        });

        self.last_claim = Some(claim);

        Ok(Some(result))
    }
    fn open_aux(
        &self,
        store: &mut Store<F>,
        rest: Ptr<F>,
    ) -> Result<(Commitment<F>, Option<Ptr<F>>)> {
        let maybe_comm = store.car(&rest)?;

        let comm = match maybe_comm.tag {
            ExprTag::Comm => maybe_comm,
            ExprTag::Num => {
                // See Store::open_mut().
                let scalar = store
                    .fetch_num(&maybe_comm)
                    .map(|x| x.into_scalar())
                    .ok_or_else(|| anyhow!("failed to fetch comm"))?;

                store.intern_maybe_opaque_comm(scalar)
            }
            _ => {
                bail!("not a commitment");
            }
        };

        let commitment = Commitment::from_comm(store, &comm);

        if let Ok((_secret, value)) = store.open_mut(maybe_comm) {
            return Ok((commitment, Some(value)));
        };

        let commitment_expr = self.expression_map.get(&commitment);
        let commitment_ptr = commitment_expr.map(|c| {
            let ptr = c.expr.ptr(store, self.repl_state.limit, self.lang());

            if let Some(secret) = c.secret {
                store.intern_comm(secret, ptr);
            };

            ptr
        });

        store.hydrate_scalar_cache();

        Ok((commitment, commitment_ptr))
    }

    fn proof_in_expr(&self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let proof = self.get_proof(store, rest)?;
        let (input, _output) = proof.io(store, self.lang())?;

        let mut handle = io::stdout().lock();
        input.expr.fmt(store, &mut handle)?;
        println!();
        Ok(None)
    }
    fn proof_out_expr(&self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let proof = self.get_proof(store, rest)?;
        let (_input, output) = proof.io(store, self.lang())?;

        let mut handle = io::stdout().lock();
        output.expr.fmt(store, &mut handle)?;
        println!();
        Ok(None)
    }
    fn proof_claim(&self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let proof = self.get_proof(store, rest)?;

        println!("{0:#?}", proof.claim);
        Ok(None)
    }

    fn get_proof(&self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Proof<F>> {
        let (proof_cid, _rest1) = store.car_cdr(&rest)?;
        let cid_string = if let Expression::Str(p) = store
            .fetch(&proof_cid)
            .ok_or_else(|| anyhow!("failed to fetch cid string"))?
        {
            p.to_string()
        } else {
            bail!("proof cid must be a string");
        };

        let cid = fcomm::cid_from_string(&cid_string)?;
        self.proof_map
            .get(&cid)
            .ok_or_else(|| anyhow!("proof not found: {cid}"))
    }

    fn prove(&mut self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let (proof_in_expr, _rest1) = store.car_cdr(&rest)?;

        let prover = NovaProver::<F, Coproc<F>>::new(self.reduction_count, self.lang().clone());
        let pp = public_params(self.reduction_count)?;

        let proof = if rest.is_nil() {
            self.last_claim
                .as_ref()
                .map(|claim| {
                    Proof::prove_claim(
                        store,
                        claim,
                        self.repl_state.limit,
                        false,
                        &prover,
                        &pp,
                        self.lang(),
                    )
                })
                .ok_or_else(|| anyhow!("no last claim"))?
        } else {
            Proof::<pallas::Scalar>::eval_and_prove(
                store,
                proof_in_expr,
                Some(self.repl_state.env),
                self.repl_state.limit,
                false,
                &prover,
                &pp,
                self.lang(),
            )
        }?;

        if proof.verify(&pp, self.lang())?.verified {
            let cid_str = proof.claim.cid().to_string();
            match proof.claim {
                Claim::Evaluation(_) | Claim::Opening(_) => println!("{0:#?}", proof.claim),
                Claim::PtrEvaluation(_) => println!("Claim::PtrEvaluation elided."),
            }

            let cid = store.str(cid_str);
            Ok(Some(cid))
        } else {
            bail!("verification of new proof failed");
        }
    }
    fn verify(&mut self, store: &mut Store<F>, rest: Ptr<F>) -> Result<Option<Ptr<F>>> {
        let (proof_cid, _) = store.car_cdr(&rest)?;

        let cid_string = if let Expression::Str(p) = store
            .fetch(&proof_cid)
            .ok_or_else(|| anyhow!("failed to fetch cid string"))?
        {
            p.to_string()
        } else {
            bail!("proof cid must be a string");
        };

        let cid = fcomm::cid_from_string(&cid_string)?;
        let proof = self
            .proof_map
            .get(&cid)
            .ok_or_else(|| anyhow!("proof not found: {cid}"))?;

        let pp = public_params(self.reduction_count)?;
        let result = proof.verify(&pp, self.lang()).unwrap();

        if result.verified {
            Ok(Some(store.get_t()))
        } else {
            Ok(Some(store.get_nil()))
        }
    }
}