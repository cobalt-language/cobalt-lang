// I'm so sorry to whoever has to decipher what happens in here
use crate::*;
use inkwell::values::{BasicValueEnum, InstructionValue, IntValue};
use inkwell::basic_block::BasicBlock;
use std::hash::{Hash, Hasher};
use std::collections::{HashSet, HashMap};
use std::cell::{Cell, Ref};
use either::{Either, for_both};
use std::cmp::{PartialOrd, Ordering};
static UNIT: () = ();
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
    CBrLazy(BasicValueEnum<'ctx>, BasicBlock<'ctx>, BasicBlock<'ctx>)
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Use<'ctx> {
    pub inst: InstructionValue<'ctx>,
    pub idx: usize,
    pub loc: SourceSpan,
    pub name: String,
    pub is_move: bool,
    /// whether this is tracked or just for debugging
    pub real: bool
}
/// compare the order in which moves (or their underlying instructions) occur. 
impl PartialOrd for Use<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        if self.inst.get_parent() != other.inst.get_parent() {return None}
        if self.inst == other.inst {return Some(self.idx.cmp(&other.idx))}
        let mut i = self.inst.get_next_instruction();
        while let Some(inst) = i {
            if inst == other.inst {return Some(Less)}
            i = inst.get_next_instruction();
        }
        i = other.inst.get_next_instruction();
        while let Some(inst) = i {
            if inst == self.inst {return Some(Greater)}
            i = inst.get_next_instruction();
        }
        None
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
        if self.inst.get_parent() != other.get_parent() {return None}
        use std::cmp::Ordering::*;
        if self.inst == *other {return Some(Equal)}
        let mut i = self.inst.get_next_instruction();
        while let Some(inst) = i {
            if inst == *other {return Some(Less)}
            i = inst.get_next_instruction();
        }
        i = other.get_next_instruction();
        while let Some(inst) = i {
            if inst == self.inst {return Some(Greater)}
            i = inst.get_next_instruction();
        }
        None
    }
}
impl<'ctx> PartialOrd<Store<'ctx>> for Use<'ctx> {
    fn partial_cmp(&self, other: &Store<'ctx>) -> Option<Ordering> {
        self.partial_cmp(&other.inst)
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
        self.idx.hash(state);
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Store<'ctx> {
    pub inst: InstructionValue<'ctx>,
    pub name: String,
    pub real: bool
}
/// compare the order in which moves (or their underlying instructions) occur. 
impl PartialOrd for Store<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
        if self.inst.get_parent() != other.inst.get_parent() {return None}
        if self.inst == other.inst {return Some(Equal)}
        let mut i = self.inst.get_next_instruction();
        while let Some(inst) = i {
            if inst == other.inst {return Some(Less)}
            i = inst.get_next_instruction();
        }
        i = other.inst.get_next_instruction();
        while let Some(inst) = i {
            if inst == self.inst {return Some(Greater)}
            i = inst.get_next_instruction();
        }
        None
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
        if self.inst.get_parent() != other.get_parent() {return None}
        use Ordering::*;
        if self.inst == *other {return Some(Equal)}
        let mut i = self.inst.get_next_instruction();
        while let Some(inst) = i {
            if inst == *other {return Some(Less)}
            i = inst.get_next_instruction();
        }
        i = other.get_next_instruction();
        while let Some(inst) = i {
            if inst == self.inst {return Some(Greater)}
            i = inst.get_next_instruction();
        }
        None
    }
}
impl<'ctx> PartialOrd<Use<'ctx>> for Store<'ctx> {
    fn partial_cmp(&self, other: &Use<'ctx>) -> Option<Ordering> {
        use Ordering::*;
        other.partial_cmp(self).map(|v| match v {
            Less => Greater,
            Greater => Less,
            Equal => Equal
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
    unsafe {
        for_both!(l, l => for_both!(r, r => (**l).partial_cmp(&**r)))
    }.unwrap_or(Ordering::Equal)
}
// TODO: come up with a better name
/// A reference to a value in a RefCell, but constructed in a way that allows added flexibility
#[derive(Debug)]
pub struct TrustMeRef<'a, T>(*const T, Ref<'a, ()>);
impl<'a, T> TrustMeRef<'a, T> {
    /// # Safety
    /// `val` must come from the same RefCell as `lock`
    #[inline(always)]
    pub unsafe fn new(val: *const T, lock: Ref<'a, ()>) -> Self {Self(val, lock)}
}
impl<T> std::ops::Deref for TrustMeRef<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {unsafe {&*self.0}}
}
/// structure of a block
struct Block<'a, 'ctx> {
    block: BasicBlock<'ctx>,
    // these pointers are safe because they're borrowed from a container in a RefCell, which cannot be mutated because of the Ref
    moves: Vec<Either<*const Use<'ctx>, *const Store<'ctx>>>,
    term: Terminator<'ctx>,
    // these are used to prevent extra allocations
    input: Cell<bool>,
    output: Cell<Option<bool>>,
    reached: IntValue<'ctx>,
    // marker to for safety
    _ref: Ref<'a, ()>
}
impl std::fmt::Debug for Block<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
        f.debug_struct("Block")
            .field("block", &self.block)
            .field("moves", &self.moves.iter().map(|e| match e {
                Either::Left(u) => Either::Left(&**u),
                Either::Right(u) => Either::Right(&**u)
            }).collect::<Vec<_>>())
            .field("term", &self.term)
            .field("reached", &self.reached)
            .finish()
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoubleMove {
    pub name: String,
    pub loc: SourceSpan,
    pub prev: Option<SourceSpan>,
    pub guaranteed: bool
}
impl PartialOrd for DoubleMove {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.loc.offset() == other.loc.offset() {
            if self.loc.len() == other.loc.len() {
                let lprev = self.prev.unwrap_or(self.loc);
                let rprev = other.prev.unwrap_or(other.loc);
                if lprev.offset() == rprev.offset() {
                    Some(lprev.len().cmp(&rprev.len()))
                }
                else {
                    Some(lprev.offset().cmp(&rprev.offset()))
                }
            }
            else {
                Some(self.loc.len().cmp(&other.loc.len()))
            }
        }
        else {
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
                }
                else {
                    lprev.offset().cmp(&rprev.offset())
                }
            }
            else {
                self.loc.len().cmp(&other.loc.len())
            }
        }
        else {
            self.loc.offset().cmp(&other.loc.offset())
        }
    }
}
/// control flow graph
#[derive(Debug)]
pub struct Cfg<'a, 'ctx: 'a> {
    blocks: Vec<Block<'a, 'ctx>>,
    last: Either<InstructionValue<'ctx>, BasicBlock<'ctx>>,
    preds: Vec<HashSet<usize>>
}
impl<'a, 'ctx> Cfg<'a, 'ctx> {
    /// create a CFG tracking the moves between `start` and `end`
    pub fn new(start: Either<InstructionValue<'ctx>, BasicBlock<'ctx>>, end: Either<InstructionValue<'ctx>, BasicBlock<'ctx>>, ctx: &'a CompCtx<'ctx>) -> Self {
        let start_block = start.right_or_else(|i| i.get_parent().expect("start and end instructions for CFG must be in blocks"));
        let end_block = end.right_or_else(|i| i.get_parent().expect("start and end instructions for CFG must be in blocks"));
        let false_ = ctx.context.bool_type().const_zero();
        if start_block == end_block {
            let inv = if let (Either::Left(start), Either::Left(end)) = (start, end) {
                'check: { // end <= start
                    let mut i = Some(end);
                    while let Some(inst) = i {
                        if inst == start {break 'check true}
                        i = inst.get_next_instruction();
                    }
                    false
                }
            } else {false};
            if inv {
                return Self {
                    blocks: vec![Block {
                        block: start_block,
                        term: Terminator::Ret,
                        moves: vec![],
                        input: Cell::default(), output: Cell::default(),
                        reached: ctx.context.bool_type().const_all_ones(),
                        _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT)
                    }],
                    preds: vec![HashSet::new()],
                    last: end
                }
            }
            let borrow = ctx.moves.borrow();
            let (moves, stores) = borrow.last().unwrap();
            let mut moves = moves.iter().map(Either::Left).chain(stores.iter().map(Either::Right)).filter(|m| for_both!(m, m => m.inst.get_parent().unwrap() == start_block && start.left().map_or(true, |start| **m >= start) && end.left().map_or(true, |end| **m <= end))).map(|e| match e {
                Either::Left(l) => Either::Left(l as _),
                Either::Right(r) => Either::Right(r as _)
            }).collect::<Vec<_>>();
            moves.sort_unstable_by(cmp_ops);
            let term = start_block.get_terminator();
            Self {
                blocks: vec![Block {
                    block: start_block,
                    term: term.map_or(Terminator::Ret, |term| if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                        todo!()
                    } else {Terminator::Ret}),
                    moves,
                    input: Cell::default(), output: Cell::default(),
                    reached: ctx.context.bool_type().const_all_ones(),
                    _ref: Ref::map(borrow, |_| &UNIT)
                }],
                preds: vec![HashSet::new()],
                last: end
            }
        }
        else {
            let borrow = ctx.moves.borrow();
            let (mvs, sts) = borrow.last().unwrap();
            let mut moves = HashMap::new();
            for m in mvs {
                moves.entry(m.inst.get_parent().unwrap()).or_insert_with(Vec::new).push(Either::Left(std::ptr::addr_of!(*m)));
            }
            for s in sts {
                moves.entry(s.inst.get_parent().unwrap()).or_insert_with(Vec::new).push(Either::Right(std::ptr::addr_of!(*s)));
            }
            moves.entry(start_block).or_insert_with(Vec::new).retain(|e| start.left().map_or(true, |start| for_both!(e, m => unsafe {**m >= start})));
            let mut seen = HashSet::from([end_block]);
            let mut queue = vec![start_block]; // depth-first traversal, with the queue being in reverse order (last element first)
            let mut blocks = vec![];
            while let Some(block) = queue.pop() {
                seen.insert(block);
                let term = block.get_terminator().map_or(Terminator::Ret, |term| if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                    match term.get_num_operands() {
                        1 => if let Some(Either::Right(b)) = term.get_operand(0) {
                            if !seen.contains(&b) {queue.push(b)}
                            Terminator::UBrLazy(b)
                        } else {Terminator::Ret}
                        3 => if let (
                            Some(Either::Left(c)),
                            Some(Either::Right(t)),
                            Some(Either::Right(f))
                         ) = (term.get_operand(0), term.get_operand(1), term.get_operand(2)) {
                            if !seen.contains(&t) {queue.push(t)}
                            if !seen.contains(&f) {queue.push(f)}
                            Terminator::CBrLazy(c, t, f)
                        } else {Terminator::Ret}
                        _ => Terminator::Ret
                    }
                } else {Terminator::Ret});
                let mut moves = moves.remove(&block).unwrap_or_default();
                moves.sort_unstable_by(cmp_ops);
                blocks.push(Block {
                    block, term, moves,
                    input: Cell::default(), output: Cell::default(),
                    reached: false_,
                    _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT)
                });
            }
            { // end block
                let term = end_block.get_terminator().map_or(Terminator::Ret, |term| if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                    match term.get_num_operands() {
                        1 => if let Some(Either::Right(b)) = term.get_operand(0) {Terminator::UBrLazy(b)} else {Terminator::Ret}
                        3 => if let (
                            Some(Either::Left(c)),
                            Some(Either::Right(t)),
                            Some(Either::Right(f))
                         ) = (term.get_operand(0), term.get_operand(1), term.get_operand(2)) {Terminator::CBrLazy(c, t, f)} else {Terminator::Ret}
                        _ => Terminator::Ret
                    }
                } else {Terminator::Ret});
                let mut moves = moves.remove(&end_block).unwrap_or_default();
                moves.retain(|e| start.left().map_or(true, |start| for_both!(e, m => unsafe {**m >= start})));
                moves.sort_unstable_by(cmp_ops);
                blocks.push(Block {
                    block: end_block,
                    term, moves,
                    input: Cell::default(), output: Cell::default(),
                    reached: false_,
                    _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT)
                });
            }
            let map = blocks.iter().enumerate().map(|(n, b)| (b.block, n)).collect::<HashMap<_, _>>();
            for block in &mut blocks {
                match block.term {
                    Terminator::UBrLazy(b) => block.term = if let Some(&b) = map.get(&b) {Terminator::UBr(b)} else {Terminator::Ret},
                    Terminator::CBrLazy(c, t, f) => block.term = if let (Some(&t), Some(&f)) = (map.get(&t), map.get(&f)) {
                        if t == f {Terminator::UBr(t)} else {Terminator::CBr(c, t, f)}
                    } else {Terminator::Ret},
                    _ => {}
                }
            }
            blocks[0].reached = ctx.context.bool_type().const_all_ones();
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
                _ => vec![]
            };
            while let Some((prev, pos, next)) = queue.pop() {
                match blocks[prev].term {
                    Terminator::UBr(_) => blocks[next].reached = blocks[prev].reached,
                    Terminator::CBr(..) => blocks[next].reached = {
                        let mut val = blocks[prev].reached;
                        if !pos {
                            val = ctx.builder.build_not(val, "");
                        }
                        val
                    },
                    _ => unreachable!()
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
            let mut preds = std::iter::repeat_with(HashSet::new).take(blocks.len()).collect::<Vec<_>>();
            for (n, block) in blocks.iter().enumerate() {
                match block.term {
                    Terminator::UBr(b) => {preds[b].insert(n);}
                    Terminator::CBr(_, t, f) => {
                        preds[t].insert(n);
                        preds[f].insert(n);
                    }
                    _ => {}
                }
            }
            Self {blocks, preds, last: end}
        }
    }
    /// Search CFG for double moves
    pub fn validate(&self) -> Vec<DoubleMove> {
        let mut errs = vec![];
        unsafe {
            let vars = self.blocks.iter().flat_map(|v| &v.moves).map(|m| for_both!(m, m => (**m).name.as_str()));
            for var in vars {
                for block in &self.blocks {
                    let insts = block.moves.iter().filter(|m| for_both!(m, m => (**m).name == var)).collect::<Vec<_>>();
                    block.input.set(insts.first().map_or(false, |v| v.is_left()));
                    block.output.set(insts.iter().rev().find_map(|v| match v {
                        Either::Left(u) => (**u).is_move.then_some(false),
                        Either::Right(_) => Some(true)
                    }));
                    let mut prev = None;
                    for inst in insts {
                        match inst {
                            Either::Left(u) => {
                                let u = &**u;
                                if let Some(prev) = prev {
                                    if u.real {
                                        errs.push(DoubleMove {
                                            name: var.to_string(),
                                            loc: u.loc,
                                            prev: (u.loc != prev).then_some(prev),
                                            guaranteed: true
                                        })
                                    }
                                }
                                if u.is_move && u.real {
                                    prev = Some(u.loc);
                                }
                            }
                            Either::Right(_) => prev = None
                        }
                    }
                }
                let mut queue = vec![];
                for block in &self.blocks {
                    if block.output.get() != Some(false) {continue}
                    queue.clear();
                    match block.term {
                        Terminator::UBr(b) => queue.push(b),
                        Terminator::CBr(_, t, f) => queue.extend_from_slice(&[f, t]),
                        _ => {}
                    };
                    while let Some(idx) = queue.pop() {
                        let next = &self.blocks[idx];
                        if next.input.get() {
                            let loc = next.moves.iter().find_map(|m| m.left().and_then(|m| (*m).real.then_some((*m).loc))).unwrap();
                            let prev = block.moves.iter().rev().find_map(|m| m.left().and_then(|m| ((*m).is_move && (*m).real).then_some((*m).loc)));
                            errs.push(DoubleMove {
                                name: var.to_string(),
                                loc,
                                prev: (prev != Some(loc)).then_some(prev).flatten(),
                                guaranteed: false
                            });
                        }
                        if next.output.get().is_none() {
                            match next.term {
                                Terminator::UBr(b) => queue.push(b),
                                Terminator::CBr(_, t, f) => queue.extend_from_slice(&[f, t]),
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        errs.sort_unstable();
        errs.dedup_by_key(|m| m.loc);
        errs
    }
    /// Check if a value has been moved before an instruction value, or by the end of the graph
    pub fn is_moved(&self, name: &str, inst: Option<InstructionValue<'ctx>>, ctx: &CompCtx<'ctx>) -> IntValue<'ctx> {
        let inst = inst.map_or(self.last, Either::Left);
        let mut blk = None;
        let mut moved = [None].repeat(self.blocks.len());
        fn moved_by<'ctx>(idx: usize, moved: &mut [Option<IntValue<'ctx>>], ctx: &CompCtx<'ctx>, preds: &[HashSet<usize>], blocks: &[Block<'_, 'ctx>]) -> IntValue<'ctx> {
            if let Some(moved) = moved[idx] {return moved}
            let false_ = ctx.context.bool_type().const_zero();
            let mut queue = preds[idx].iter().copied().collect::<Vec<_>>();
            let mut out = false_;
            while let Some(idx) = queue.pop() {
                moved_by(idx, moved, ctx, preds, blocks);
                let block = &blocks[idx];
                match block.output.get() {
                    None => queue.extend(preds[idx].iter().copied()),
                    Some(false) => out = ctx.builder.build_or(out, block.reached, ""),
                    Some(true) => {}
                }
            }
            moved[idx] = Some(out);
            out
        }
        unsafe {
            for (n, block) in self.blocks.iter().enumerate() {
                let insts = block.moves.iter().filter(|m| for_both!(m, m => (**m).name == name)).collect::<Vec<_>>();
                block.input.set(insts.first().map_or(false, |v| v.is_left()));
                block.output.set(insts.iter().rev().find_map(|v| match v {
                    Either::Left(u) => (**u).is_move.then_some(false),
                    Either::Right(_) => Some(true)
                }));
                if inst.either(|i| i.get_parent() == Some(block.block), |b| b == block.block) {blk = Some(n)}
            }
            let true_ = ctx.context.bool_type().const_all_ones();
            let false_ = ctx.context.bool_type().const_zero();
            let blk = if let Some(blk) = blk {blk} else {return false_};
            self.blocks[blk].moves.iter().rev().filter(|e| for_both!(e, m => inst.left().map_or(false, |inst| **m <= inst))).find_map(|e| match e {
                Either::Left(u) => ((**u).is_move && (**u).real).then_some(true_),
                Either::Right(s) => (**s).real.then_some(false_)
            }).unwrap_or_else(|| moved_by(blk, &mut moved, ctx, &self.preds, &self.blocks))
        }
    }
    /// Insert destructor calls before stores if necessary.
    /// If `at_end` is true, insert the destructors for all values in the top VarMap layer as well
    pub fn insert_dtors(&self, ctx: &CompCtx<'ctx>, at_end: bool) {
        let f = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
        self.blocks.iter().flat_map(|b| &b.moves).filter_map(|e| e.as_ref().right()).for_each(|m| {
            unsafe {
                let m = &**m;
                let c = self.is_moved(&m.name, Some(m.inst), ctx);
                let db = ctx.context.append_basic_block(f, &format!("dtor.{}", m.name));
                let mb = ctx.context.append_basic_block(f, "merge");
                ctx.builder.build_conditional_branch(c, mb, db);
                ctx.builder.position_at_end(db);
                ctx.lookup(&m.name, false).unwrap().0.ins_dtor(ctx);
                ctx.builder.build_unconditional_branch(mb);
                ctx.builder.position_at_end(mb);
            }
        });
        if at_end {
            ctx.with_vars(|v| {
                v.symbols.iter().for_each(|(n, v)| {
                    let c = self.is_moved(n, None, ctx);
                    let db = ctx.context.append_basic_block(f, &format!("dtor.{n}"));
                    let mb = ctx.context.append_basic_block(f, "merge");
                    ctx.builder.build_conditional_branch(c, mb, db);
                    ctx.builder.position_at_end(db);
                    v.0.ins_dtor(ctx);
                    ctx.builder.build_unconditional_branch(mb);
                    ctx.builder.position_at_end(mb);
                });
            })
        }
    }
}