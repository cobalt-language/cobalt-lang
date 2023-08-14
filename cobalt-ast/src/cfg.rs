// I'm so sorry to whoever has to decipher what happens in here
use crate::*;
use either::{for_both, Either};
use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, InstructionValue, IntValue};
use std::cell::{Cell, Ref};
use std::cmp::{Ordering, PartialOrd};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
static UNIT: () = ();
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Location<'ctx> {
    Block(BasicBlock<'ctx>),
    Inst(InstructionValue<'ctx>, usize),
    AfterInst(InstructionValue<'ctx>),
}
impl<'ctx> Location<'ctx> {
    pub fn block(self) -> BasicBlock<'ctx> {
        match self {
            Self::Block(b) => b,
            Self::Inst(i, _) | Self::AfterInst(i) => i.get_parent().unwrap(),
        }
    }
    pub fn current(ctx: &CompCtx<'ctx>) -> Option<Self> {
        let b = ctx.builder.get_insert_block()?;
        Some(
            b.get_last_instruction()
                .map(Self::AfterInst)
                .unwrap_or(Self::Block(b)),
        )
    }
}
impl<'ctx> From<InstructionValue<'ctx>> for Location<'ctx> {
    fn from(value: InstructionValue<'ctx>) -> Self {
        Self::Inst(value, 0)
    }
}
impl<'ctx> From<Result<InstructionValue<'ctx>, BasicBlock<'ctx>>> for Location<'ctx> {
    fn from(value: Result<InstructionValue<'ctx>, BasicBlock<'ctx>>) -> Self {
        match value {
            Ok(i) => i.into(),
            Err(b) => Self::Block(b),
        }
    }
}
impl PartialOrd for Location<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.block() != other.block() {
            return None;
        }
        use Location::*;
        use Ordering::*;
        match (*self, *other) {
            (Block(_), Block(_)) => Some(Equal),
            (Inst(lb, ln), Inst(rb, rn)) => {
                let c = cmp_insts(lb, rb)?;
                Some(if c == Equal { ln.cmp(&rn) } else { c })
            }
            (AfterInst(lb), AfterInst(rb)) => cmp_insts(lb, rb),
            (Block(_), _) => Some(Less),
            (_, Block(_)) => Some(Greater),
            (AfterInst(_), _) => Some(Greater),
            (_, AfterInst(_)) => Some(Less),
        }
    }
}
impl<'ctx> PartialEq<InstructionValue<'ctx>> for Location<'ctx> {
    fn eq(&self, other: &InstructionValue<'ctx>) -> bool {
        *self == Self::Inst(*other, 0)
    }
}
impl<'ctx> PartialOrd<InstructionValue<'ctx>> for Location<'ctx> {
    fn partial_cmp(&self, other: &InstructionValue<'ctx>) -> Option<Ordering> {
        self.partial_cmp(&Self::Inst(*other, 0))
    }
}
pub fn cmp_insts(lhs: InstructionValue, rhs: InstructionValue) -> Option<Ordering> {
    if lhs.get_parent() != rhs.get_parent() {
        return None;
    }
    use Ordering::*;
    if lhs == rhs {
        return Some(Equal);
    }
    let mut i = lhs.get_next_instruction();
    while let Some(inst) = i {
        if inst == rhs {
            return Some(Less);
        }
        i = inst.get_next_instruction();
    }
    i = rhs.get_next_instruction();
    while let Some(inst) = i {
        if inst == lhs {
            return Some(Greater);
        }
        i = inst.get_next_instruction();
    }
    None
}
#[derive(Debug, Clone, Copy)]
enum Terminator<'ctx> {
    /// this block does not branch to another block
    Ret,
    /// unconditional branch to a block
    UBr(usize),
    /// conditional branch to one of two other blocks
    CBr(BasicValueEnum<'ctx>, usize, usize),
    // lazy forms require linear time lookup, but can only be initialized after all blocks are parsed
    UBrLazy(BasicBlock<'ctx>),
    CBrLazy(BasicValueEnum<'ctx>, BasicBlock<'ctx>, BasicBlock<'ctx>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use<'ctx> {
    pub inst: Location<'ctx>,
    pub loc: SourceSpan,
    pub name: (String, usize),
    pub is_move: bool,
    /// whether this is tracked or just for debugging
    pub real: bool,
}
/// compare the order in which moves (or their underlying instructions) occur.
impl PartialOrd for Use<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inst.partial_cmp(&other.inst)
    }
}
impl<'ctx> PartialEq<InstructionValue<'ctx>> for Use<'ctx> {
    fn eq(&self, other: &InstructionValue<'ctx>) -> bool {
        self.inst == *other
    }
}
/// compare the order in which moves (or their underlying instructions) occur.
impl<'ctx> PartialOrd<InstructionValue<'ctx>> for Use<'ctx> {
    fn partial_cmp(&self, other: &InstructionValue<'ctx>) -> Option<std::cmp::Ordering> {
        self.inst.partial_cmp(other)
    }
}
impl<'ctx> PartialOrd<Store<'ctx>> for Use<'ctx> {
    fn partial_cmp(&self, other: &Store<'ctx>) -> Option<Ordering> {
        self.inst.partial_cmp(&other.inst)
    }
}
impl<'ctx> PartialEq<Store<'ctx>> for Use<'ctx> {
    fn eq(&self, _other: &Store<'ctx>) -> bool {
        false
    }
}
impl Hash for Use<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inst.hash(state);
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Store<'ctx> {
    pub inst: Location<'ctx>,
    pub name: (String, usize),
    pub real: bool,
}
/// compare the order in which moves (or their underlying instructions) occur.
impl PartialOrd for Store<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inst.partial_cmp(&other.inst)
    }
}
impl<'ctx> PartialEq<InstructionValue<'ctx>> for Store<'ctx> {
    fn eq(&self, other: &InstructionValue<'ctx>) -> bool {
        self.inst == *other
    }
}
/// compare the order in which moves (or their underlying instructions) occur.
impl<'ctx> PartialOrd<InstructionValue<'ctx>> for Store<'ctx> {
    fn partial_cmp(&self, other: &InstructionValue<'ctx>) -> Option<Ordering> {
        self.inst.partial_cmp(other)
    }
}
impl<'ctx> PartialOrd<Use<'ctx>> for Store<'ctx> {
    fn partial_cmp(&self, other: &Use<'ctx>) -> Option<Ordering> {
        use Ordering::*;
        other.partial_cmp(self).map(|v| match v {
            Less => Greater,
            Greater => Less,
            Equal => Equal,
        })
    }
}
impl<'ctx> PartialEq<Use<'ctx>> for Store<'ctx> {
    fn eq(&self, _other: &Use<'ctx>) -> bool {
        false
    }
}
impl Hash for Store<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inst.hash(state);
    }
}
fn cmp_ops(l: &Either<*const Use, *const Store>, r: &Either<*const Use, *const Store>) -> Ordering {
    unsafe { for_both!(l, l => for_both!(r, r => (**l).partial_cmp(&**r))) }
        .unwrap_or(Ordering::Equal)
}
/// structure of a block
struct Block<'a, 'ctx> {
    block: BasicBlock<'ctx>,
    // these pointers are safe because they're borrowed from a container in a RefCell, which cannot be mutated because of the Ref
    moves: Vec<Either<*const Use<'ctx>, *const Store<'ctx>>>,
    term: Terminator<'ctx>,

    // These are used to prevent extra allocations.
    //
    // During analysis we analyze each variable one at a time, and reuse these fields during that
    // analysis.
    //
    /// - `true`: we need the variable to be initialized
    /// - `false`: it doesn't need to be initialized (this doesn't mean it's not used, but if it
    /// is, there's a store before it)
    input: Cell<bool>,
    /// - `Some(true)`: after this block, the variable is initialized (no moves after the last store)
    /// - `Some(false)`: after this block, the variable is uninitialized (ends in a move)
    /// - `None`: after this block, the variable is in the same state as it was before
    output: Cell<Option<bool>>,
    reached: IntValue<'ctx>,
    // marker to for safety
    _ref: Ref<'a, ()>,
}
impl std::fmt::Debug for Block<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            f.debug_struct("Block")
                .field("block", &self.block)
                .field(
                    "moves",
                    &self
                        .moves
                        .iter()
                        .map(|e| match e {
                            Either::Left(u) => Either::Left(&**u),
                            Either::Right(u) => Either::Right(&**u),
                        })
                        .collect::<Vec<_>>(),
                )
                .field("term", &self.term)
                .field("reached", &self.reached)
                .field("input", &self.input.get())
                .field("output", &self.output.get())
                .finish()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoubleMove {
    pub name: String,
    pub loc: SourceSpan,
    pub prev: Option<SourceSpan>,
    pub guaranteed: bool,
}
impl PartialOrd for DoubleMove {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.loc.offset() == other.loc.offset() {
            if self.loc.len() == other.loc.len() {
                let lprev = self.prev.unwrap_or(self.loc);
                let rprev = other.prev.unwrap_or(other.loc);
                if lprev.offset() == rprev.offset() {
                    Some(lprev.len().cmp(&rprev.len()))
                } else {
                    Some(lprev.offset().cmp(&rprev.offset()))
                }
            } else {
                Some(self.loc.len().cmp(&other.loc.len()))
            }
        } else {
            Some(self.loc.offset().cmp(&other.loc.offset()))
        }
    }
}
impl Ord for DoubleMove {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.loc.offset() == other.loc.offset() {
            if self.loc.len() == other.loc.len() {
                let lprev = self.prev.unwrap_or(self.loc);
                let rprev = other.prev.unwrap_or(other.loc);
                if lprev.offset() == rprev.offset() {
                    lprev.len().cmp(&rprev.len())
                } else {
                    lprev.offset().cmp(&rprev.offset())
                }
            } else {
                self.loc.len().cmp(&other.loc.len())
            }
        } else {
            self.loc.offset().cmp(&other.loc.offset())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinearTypeNotUsed {
    pub name: String,
    pub loc: SourceSpan,
}

/// control flow graph
#[derive(Debug)]
pub struct Cfg<'a, 'ctx: 'a> {
    blocks: Vec<Block<'a, 'ctx>>,
    last: Location<'ctx>,
    preds: Vec<HashSet<usize>>,
}
impl<'a, 'ctx> Cfg<'a, 'ctx> {
    /// create a CFG tracking the moves between `start` and `end`
    /// `start` and `end` are inclusive
    pub fn new(start: Location<'ctx>, end: Location<'ctx>, ctx: &'a CompCtx<'ctx>) -> Self {
        let start_block = start.block();
        let end_block = end.block();
        let false_ = ctx.context.bool_type().const_zero();
        let true_ = ctx.context.bool_type().const_all_ones();
        if start_block == end_block {
            if end < start {
                return Self {
                    blocks: vec![Block {
                        block: start_block,
                        term: Terminator::Ret,
                        moves: vec![],
                        input: Cell::default(),
                        output: Cell::default(),
                        reached: ctx.context.bool_type().const_all_ones(),
                        _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT),
                    }],
                    preds: vec![HashSet::new()],
                    last: end,
                };
            }
            let borrow = ctx.moves.borrow();
            let (moves, stores) = &*borrow;
            let mut moves = moves.iter().map(Either::Left).chain(stores.iter().map(Either::Right)).filter(|m| for_both!(m, m => m.inst.block() == start_block && m.inst >= start && m.inst <= end)).map(|e| match e {
                Either::Left(l) => Either::Left(l as _),
                Either::Right(r) => Either::Right(r as _)
            }).collect::<Vec<_>>();
            moves.sort_unstable_by(cmp_ops);
            let term = start_block.get_terminator();
            Self {
                blocks: vec![Block {
                    block: start_block,
                    term: term.map_or(Terminator::Ret, |term| {
                        if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                            match term.get_num_operands() {
                                1 => {
                                    if let Some(Either::Right(b)) = term.get_operand(0) {
                                        Terminator::UBrLazy(b)
                                    } else {
                                        Terminator::Ret
                                    }
                                }
                                3 => {
                                    if let (
                                        Some(Either::Left(c)),
                                        Some(Either::Right(t)),
                                        Some(Either::Right(f)),
                                    ) = (
                                        term.get_operand(0),
                                        term.get_operand(2),
                                        term.get_operand(1),
                                    ) {
                                        Terminator::CBrLazy(c, t, f)
                                    } else {
                                        Terminator::Ret
                                    }
                                }
                                _ => Terminator::Ret,
                            }
                        } else {
                            Terminator::Ret
                        }
                    }),
                    moves,
                    input: Cell::default(),
                    output: Cell::default(),
                    reached: true_,
                    _ref: Ref::map(borrow, |_| &UNIT),
                }],
                preds: vec![HashSet::new()],
                last: end,
            }
        } else {
            let borrow = ctx.moves.borrow();
            let (mvs, sts) = &*borrow;
            let mut moves = HashMap::new();
            for m in mvs {
                moves
                    .entry(m.inst.block())
                    .or_insert_with(Vec::new)
                    .push(Either::Left(std::ptr::addr_of!(*m)));
            }
            for s in sts {
                moves
                    .entry(s.inst.block())
                    .or_insert_with(Vec::new)
                    .push(Either::Right(std::ptr::addr_of!(*s)));
            }
            moves
                .entry(start_block)
                .or_insert_with(Vec::new)
                .retain(|e| for_both!(e, m => unsafe {(**m).inst}) >= start);
            let mut seen = HashSet::from([end_block]);
            let mut queue = vec![start_block]; // depth-first traversal, with the queue being in reverse order (last element first)
            let mut blocks = vec![];
            while let Some(block) = queue.pop() {
                seen.insert(block);
                let term = block.get_terminator().map_or(Terminator::Ret, |term| {
                    if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                        match term.get_num_operands() {
                            1 => {
                                if let Some(Either::Right(b)) = term.get_operand(0) {
                                    if !seen.contains(&b) {
                                        queue.push(b)
                                    }
                                    Terminator::UBrLazy(b)
                                } else {
                                    Terminator::Ret
                                }
                            }
                            3 => {
                                if let (
                                    Some(Either::Left(c)),
                                    Some(Either::Right(t)),
                                    Some(Either::Right(f)),
                                ) = (
                                    term.get_operand(0),
                                    term.get_operand(2),
                                    term.get_operand(1),
                                ) {
                                    if !seen.contains(&t) {
                                        queue.push(t)
                                    }
                                    if !seen.contains(&f) {
                                        queue.push(f)
                                    }
                                    Terminator::CBrLazy(c, t, f)
                                } else {
                                    Terminator::Ret
                                }
                            }
                            _ => Terminator::Ret,
                        }
                    } else {
                        Terminator::Ret
                    }
                });
                let mut moves = moves.remove(&block).unwrap_or_default();
                moves.sort_unstable_by(cmp_ops);
                blocks.push(Block {
                    block,
                    term,
                    moves,
                    input: Cell::default(),
                    output: Cell::default(),
                    reached: false_,
                    _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT),
                });
            }
            {
                // end block
                let term = end_block.get_terminator().map_or(Terminator::Ret, |term| {
                    if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                        match term.get_num_operands() {
                            1 => {
                                if let Some(Either::Right(b)) = term.get_operand(0) {
                                    Terminator::UBrLazy(b)
                                } else {
                                    Terminator::Ret
                                }
                            }
                            3 => {
                                if let (
                                    Some(Either::Left(c)),
                                    Some(Either::Right(t)),
                                    Some(Either::Right(f)),
                                ) = (
                                    term.get_operand(0),
                                    term.get_operand(2),
                                    term.get_operand(1),
                                ) {
                                    Terminator::CBrLazy(c, t, f)
                                } else {
                                    Terminator::Ret
                                }
                            }
                            _ => Terminator::Ret,
                        }
                    } else {
                        Terminator::Ret
                    }
                });
                let mut moves = moves.remove(&end_block).unwrap_or_default();
                moves.retain(|e| for_both!(e, m => unsafe {(**m).inst}) <= end);
                moves.sort_unstable_by(cmp_ops);
                blocks.push(Block {
                    block: end_block,
                    term,
                    moves,
                    input: Cell::default(),
                    output: Cell::default(),
                    reached: false_,
                    _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT),
                });
            }
            let map = blocks
                .iter()
                .enumerate()
                .map(|(n, b)| (b.block, n))
                .collect::<HashMap<_, _>>();
            for block in &mut blocks {
                match block.term {
                    Terminator::UBrLazy(b) => {
                        block.term = if let Some(&b) = map.get(&b) {
                            Terminator::UBr(b)
                        } else {
                            Terminator::Ret
                        }
                    }
                    Terminator::CBrLazy(c, t, f) => {
                        block.term = if let (Some(&t), Some(&f)) = (map.get(&t), map.get(&f)) {
                            if t == f {
                                Terminator::UBr(t)
                            } else {
                                Terminator::CBr(c, t, f)
                            }
                        } else {
                            Terminator::Ret
                        }
                    }
                    _ => {}
                }
            }
            blocks[0].reached = true_;
            let mut seen = HashSet::new();
            let mut queue = match blocks[0].term {
                Terminator::UBr(b) => {
                    seen.insert((0, b));
                    vec![(0, false, b)]
                }
                Terminator::CBr(_, t, f) => {
                    seen.insert((0, t));
                    seen.insert((0, f));
                    vec![(0, false, f), (0, true, t)]
                }
                _ => vec![],
            };
            while let Some((prev, pos, next)) = queue.pop() {
                if let Some(i) = blocks[prev].block.get_last_instruction() {
                    ctx.builder.position_before(&i)
                } else {
                    ctx.builder.position_at_end(blocks[prev].block)
                }
                match blocks[prev].term {
                    Terminator::UBr(_) => blocks[next].reached = blocks[prev].reached,
                    Terminator::CBr(c, ..) => {
                        blocks[next].reached = {
                            let mut val = match blocks[prev].reached.get_zero_extended_constant() {
                                Some(0) => false_,
                                Some(1) => c.into_int_value(),
                                _ => ctx.builder.build_and(
                                    blocks[prev].reached,
                                    c.into_int_value(),
                                    "",
                                ),
                            };
                            if !pos {
                                val = match val.get_zero_extended_constant() {
                                    Some(0) => true_,
                                    Some(1) => false_,
                                    _ => ctx.builder.build_not(val, ""),
                                };
                            }
                            val
                        }
                    }
                    _ => unreachable!(),
                }
                match blocks[next].term {
                    Terminator::UBr(b) => {
                        if !seen.contains(&(next, b)) {
                            seen.insert((next, b));
                            queue.push((next, false, b));
                        }
                    }
                    Terminator::CBr(_, t, f) => {
                        if !seen.contains(&(next, f)) {
                            seen.insert((next, f));
                            queue.push((next, false, f));
                        }
                        if !seen.contains(&(next, t)) {
                            seen.insert((next, t));
                            queue.push((next, true, t));
                        }
                    }
                    _ => {}
                }
            }
            let mut preds = std::iter::repeat_with(HashSet::new)
                .take(blocks.len())
                .collect::<Vec<_>>();
            for (n, block) in blocks.iter().enumerate() {
                if block.reached.get_name().to_bytes().is_empty() {
                    block.reached.set_name(&format!(
                        "reached.{}",
                        block.block.get_name().to_str().unwrap()
                    ));
                }
                match block.term {
                    Terminator::UBr(b) => {
                        preds[b].insert(n);
                    }
                    Terminator::CBr(_, t, f) => {
                        preds[t].insert(n);
                        preds[f].insert(n);
                    }
                    _ => {}
                }
            }
            Self {
                blocks,
                preds,
                last: end,
            }
        }
    }

    /// Search CFG for double moves
    pub fn validate(&self, ctx: &'a CompCtx<'ctx>) -> Vec<cobalt_errors::CobaltError> {
        let mut errs = vec![];
        unsafe {
            // Get all the variables which have moved.
            let vars = self
                .blocks
                .iter()
                .flat_map(|v| &v.moves)
                .map(|m| for_both!(m, m => &(**m).name));

            // For each of these variables, look inside each block and find all the move
            // instructions for it.
            //
            // - 2: Consider the first of these instructions, i.e. the first move instruction
            // for this variable in this block. If it's a use, then the variable must be
            // initialized on entry to the block.
            //
            // - 3: Now consider the last of these instructions. If it's a use, check if that use
            // moves the variable and if so mark the variable as unitialized/moved. If it is a
            // store, then the variable is initialized on exit from the block.
            //
            // - 4: Now go through the rest of the instructions and look for a double move, i.e. a
            // use after move.
            //
            // Note however that this doesn't capture all possible uses after move;
            // for example, it will not detect if the variable was used once but was unitialized
            // upon entering the block. This is the next step.
            for var in vars {
                for block in &self.blocks {
                    let insts = block
                        .moves
                        .iter()
                        .filter(|m| for_both!(m, m => &(**m).name == var))
                        .collect::<Vec<_>>();

                    block // 2
                        .input
                        .set(insts.first().map_or(false, |v| v.is_left()));

                    block.output.set(insts.iter().rev().find_map(|v| match v {
                        // 3
                        Either::Left(u) => (**u).is_move.then_some(false),
                        Either::Right(_) => Some(true),
                    }));

                    // 4
                    let mut prev = None; // location of previous move
                    for inst in insts {
                        match inst {
                            Either::Left(u) => {
                                let u = &**u;
                                if let Some(prev) = prev {
                                    if u.real {
                                        errs.push(DoubleMove {
                                            name: var.0.clone(),
                                            loc: u.loc,
                                            prev: (u.loc != prev).then_some(prev),
                                            guaranteed: true,
                                        })
                                    }
                                }
                                if u.is_move && u.real {
                                    prev = Some(u.loc);
                                }
                            }
                            Either::Right(_) => prev = None,
                        }
                    }
                }

                // Handle possible double moves relating to the initialization status of the
                // variable entering/exiting the block.
                //
                // - 1: Blocks to analyze, in order.
                // - 2: If the initialization status is the same at the beginning and end of the
                // block then we don't need to worry.
                let mut queue = vec![]; // 1
                let mut seen = HashSet::new();
                for (n, block) in self.blocks.iter().enumerate() {
                    if block.output.get() != Some(false) {
                        // 1
                        continue;
                    }
                    queue.clear();
                    seen.clear();
                    seen.insert(n);
                    match block.term {
                        Terminator::UBr(b) => queue.push(b),
                        Terminator::CBr(_, t, f) => queue.extend_from_slice(&[f, t]),
                        _ => {}
                    };
                    while let Some(idx) = queue.pop() {
                        let next = &self.blocks[idx];
                        if next.input.get() {
                            let loc = next
                                .moves
                                .iter()
                                .find_map(|m| m.left().and_then(|m| (*m).real.then_some((*m).loc)))
                                .unwrap();
                            let prev = block.moves.iter().rev().find_map(|m| {
                                m.left()
                                    .and_then(|m| ((*m).is_move && (*m).real).then_some((*m).loc))
                            });
                            errs.push(DoubleMove {
                                name: var.0.clone(),
                                loc,
                                prev: (prev != Some(loc)).then_some(prev).flatten(),
                                guaranteed: false,
                            });
                        }
                        if next.output.get().is_none() && !seen.contains(&idx) {
                            match next.term {
                                Terminator::UBr(b) => {
                                    seen.insert(b);
                                    queue.push(b);
                                }
                                Terminator::CBr(_, t, f) => {
                                    seen.insert(f);
                                    seen.insert(t);
                                    queue.push(f);
                                    queue.push(t);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        errs.sort_unstable();
        errs.dedup_by_key(|m| m.loc);

        let mut to_return = errs
            .iter()
            .map(
                |DoubleMove {
                     name,
                     loc,
                     prev,
                     guaranteed,
                 }| CobaltError::DoubleMove {
                    loc: *loc,
                    prev: *prev,
                    name: name.clone(),
                    guaranteed: *guaranteed,
                },
            )
            .collect::<Vec<_>>();

        to_return.append(&mut self.validate_linear_types(ctx));
        to_return
    }

    fn validate_linear_types(&self, ctx: &'a CompCtx<'ctx>) -> Vec<cobalt_errors::CobaltError> {
        let mut errs = Vec::new();

        // Go through each variable, see if it's a linear type.
        //
        // - 1: We check if it has been moved at least once by verifying that the variable
        // appears in the moves list.
        ctx.with_vars(|v| {
            v.symbols.iter().for_each(|(n, v)| {
                let is_linear_type = is_linear_type(&v.0.data_type, ctx);
                if is_linear_type {
                    let lex_scope = match v.1.scope {
                        Some(scope) => Some(scope.into()),
                        None => None,
                    };
                    let is_moved = self.is_moved(n, lex_scope, None, ctx);
                    match is_moved.get_zero_extended_constant() {
                        Some(1) => {}
                        _ => {
                            errs.push(CobaltError::LinearTypeNotUsed {
                                name: n.clone(),
                                loc: v.1.loc.unwrap_or(0.into()),
                            });
                        }
                    }
                }
            })
        });

        errs
    }

    /// Check if a value has been moved before an instruction value, or by the end of the graph
    ///
    /// ## Parameters
    /// - `name`: The name of the value.
    /// - `lex_scope`: The lexical scope of the value.
    /// - `inst`: The instruction. If `None`, the function will check if the value has been moved
    /// by the end of the graph.
    /// - `ctx`: The compilation context.
    ///
    /// ## Returns
    /// A boolean value indicating whether the value has been moved, represented as an `IntValue`.
    /// If the value is guarenteed to have been moved, returns `1`. If the value is guarenteed to
    /// not have been moved, returns `0`. If the value may or may not have been moved, a different
    /// value is returned.
    ///
    /// ## Notes
    /// To determine if the function returns `0` or `1`, you can do this:
    /// ```rust
    /// let is_moved = is_moved(...);
    /// match is_moved.get_zero_extended_constant() {
    ///    Some(0) => { /* not moved */ }
    ///    Some(1) => { /* moved */ }
    ///    _ => { /* maybe moved */ }
    /// }
    /// ```
    pub fn is_moved(
        &self,
        name: &str,
        lex_scope: Option<usize>,
        inst: Option<Location<'ctx>>,
        ctx: &CompCtx<'ctx>,
    ) -> IntValue<'ctx> {
        let inst = inst.unwrap_or(self.last);
        let mut blk = None;

        // - 1: Pick out the instructions which include the variable and match the lexical scope.
        // - 2: Set whether the variable is initialized entering/exiting the block.
        unsafe {
            for (n, block) in self.blocks.iter().enumerate() {
                // 1
                let insts = block.moves.iter().filter(|m| for_both!(m, m => (**m).name.0 == name && lex_scope.map_or(true, |ls| (**m).name.1 == ls))).collect::<Vec<_>>();
                // 2
                block
                    .input
                    .set(insts.first().map_or(false, |v| v.is_left()));
                block.output.set(insts.iter().rev().find_map(|v| match v {
                    Either::Left(u) => (**u).is_move.then_some(false),
                    Either::Right(_) => Some(true),
                }));
                if block.block == inst.block() {
                    blk = Some(n)
                }
            }

            let true_ = ctx.context.bool_type().const_all_ones();
            let false_ = ctx.context.bool_type().const_zero();

            let blk = if let Some(blk) = blk {
                blk
            } else {
                // We are checking if the value has moved before an instruction which does not even
                // belong to the same block as the value, so it doesn't make sense to compare them.
                // But for convention we will return false.
                return false_;
            };

            // - 1: Starting from the last move and going backwards...
            // - 2: If the move is wrt this variable and happens before the specified
            // instruction...
            // - 3: If `e` is a use, check if it is a move. If `e` is a store, then the variable
            // is guarenteed to be initialized (not moved) after this block.
            self.blocks[blk]
                .moves
                .iter()
                .rev() // 1
                .filter(|e| for_both!(e, e => (**e).name.0 == name && (**e).inst <= inst)) // 2
                .find_map(|e| match e {
                    // 3
                    Either::Left(u) => ((**u).is_move && (**u).real).then_some(true_),
                    Either::Right(s) => (**s).real.then_some(false_),
                })
                .unwrap_or_else(|| {
                    let mut queue = self.preds[blk].iter().copied().collect::<Vec<_>>();
                    let mut out = false_;
                    let mut seen = HashSet::new();
                    while let Some(idx) = queue.pop() {
                        let block = &self.blocks[idx];
                        match block.output.get() {
                            None => {
                                let len = queue.len();
                                queue.extend(
                                    self.preds[idx]
                                        .iter()
                                        .copied()
                                        .filter(|q| !seen.contains(q)),
                                );
                                seen.extend(queue.iter().copied().skip(len));
                            }
                            Some(false) => {
                                out = match (
                                    out.get_zero_extended_constant(),
                                    block.reached.get_zero_extended_constant(),
                                ) {
                                    (Some(0), _) => block.reached,
                                    (_, Some(0)) => out,
                                    (Some(1), _) | (_, Some(1)) => true_,
                                    _ => ctx.builder.build_or(out, block.reached, ""),
                                }
                            }
                            Some(true) => {}
                        }
                    }
                    if out.get_name().to_bytes().is_empty() {
                        out.set_name(&format!(
                            "moved.{}.{name}{}",
                            self.blocks[blk].block.get_name().to_str().unwrap(),
                            lex_scope.map_or_else(String::new, |l| format!(".{l}"))
                        ));
                    }
                    out
                })
        }
    }
    /// Insert destructor calls before stores if necessary.
    /// If `at_end` is true, insert the destructors for all values in the top VarMap layer as well
    pub fn insert_dtors(&self, ctx: &CompCtx<'ctx>, at_end: bool) {
        let f = ctx
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        self.blocks
            .iter()
            .flat_map(|b| &b.moves)
            .filter_map(|e| e.as_ref().right())
            .for_each(|m| unsafe {
                let m = &**m;
                match m.inst {
                    Location::Block(b) => {
                        if let Some(i) = b.get_first_instruction() {
                            ctx.builder.position_before(&i)
                        } else {
                            ctx.builder.position_at_end(b)
                        }
                    }
                    Location::Inst(i, _) => ctx.builder.position_before(&i),
                    Location::AfterInst(i) => {
                        if let Some(i) = i.get_next_instruction() {
                            ctx.builder.position_before(&i)
                        } else {
                            ctx.builder.position_at_end(i.get_parent().unwrap())
                        }
                    }
                }
                let c = self.is_moved(&m.name.0, Some(m.name.1), Some(m.inst), ctx);
                match c.get_zero_extended_constant() {
                    Some(0) => ctx.lookup(&m.name.0, false).unwrap().0.ins_dtor(ctx),
                    Some(1) => {}
                    _ => {
                        let db = ctx
                            .context
                            .append_basic_block(f, &format!("dtor.{}.{}", m.name.0, m.name.1));
                        let mb = ctx.context.append_basic_block(f, "merge");
                        ctx.builder.build_conditional_branch(c, mb, db);
                        ctx.builder.position_at_end(db);
                        ctx.lookup(&m.name.0, false).unwrap().0.ins_dtor(ctx);
                        ctx.builder.build_unconditional_branch(mb);
                        ctx.builder.position_at_end(mb);
                    }
                }
            });
        if at_end {
            match self.last {
                Location::Block(b) => {
                    if let Some(i) = b.get_first_instruction() {
                        ctx.builder.position_before(&i)
                    } else {
                        ctx.builder.position_at_end(b)
                    }
                }
                Location::Inst(i, _) => ctx.builder.position_before(&i),
                Location::AfterInst(i) => {
                    if let Some(i) = i.get_next_instruction() {
                        ctx.builder.position_before(&i)
                    } else {
                        ctx.builder.position_at_end(i.get_parent().unwrap())
                    }
                }
            }
            ctx.with_vars(|v| {
                v.symbols.iter().for_each(|(n, v)| {
                    let scope =
                        v.0.name
                            .as_ref()
                            .map_or_else(|| ctx.lex_scope.get(), |x| x.1);
                    let c = self.is_moved(n, Some(scope), None, ctx);
                    match c.get_zero_extended_constant() {
                        Some(0) => v.0.ins_dtor(ctx),
                        Some(1) => {}
                        _ => {
                            let db = ctx
                                .context
                                .append_basic_block(f, &format!("dtor.{n}.{scope}"));
                            let mb = ctx.context.append_basic_block(f, "merge");
                            ctx.builder.build_conditional_branch(c, mb, db);
                            ctx.builder.position_at_end(db);
                            v.0.ins_dtor(ctx);
                            ctx.builder.build_unconditional_branch(mb);
                            ctx.builder.position_at_end(mb);
                        }
                    }
                });
            })
        }
    }
}
