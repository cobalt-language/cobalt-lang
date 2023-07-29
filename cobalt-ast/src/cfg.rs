#![allow(dead_code)] // TODO: remove before merge
use crate::*;
use inkwell::values::{BasicValueEnum, InstructionValue, IntValue};
use inkwell::basic_block::BasicBlock;
use std::hash::{Hash, Hasher};
use std::collections::{HashSet, HashMap};
use std::cell::Ref;
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
pub struct Move<'ctx> {
    pub inst: InstructionValue<'ctx>,
    pub idx: usize,
    pub loc: SourceSpan,
    pub name: String,
    /// whether this is tracked or just for debugging
    pub real: bool
}
/// compare the order in which moves (or their underlying instructions) occur. 
impl PartialOrd for Move<'_> {
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
impl<'ctx> PartialEq<InstructionValue<'ctx>> for Move<'ctx> {
    fn eq(&self, other: &InstructionValue<'ctx>) -> bool {
        self.inst == *other
    }
}
/// compare the order in which moves (or their underlying instructions) occur. 
impl<'ctx> PartialOrd<InstructionValue<'ctx>> for Move<'ctx> {
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
impl<'ctx> PartialOrd<Store<'ctx>> for Move<'ctx> {
    fn partial_cmp(&self, other: &Store<'ctx>) -> Option<Ordering> {
        self.partial_cmp(&other.inst)
    }
}
impl<'ctx> PartialEq<Store<'ctx>> for Move<'ctx> {
    fn eq(&self, _other: &Store<'ctx>) -> bool {
        false
    }
}
impl Hash for Move<'_> {
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
impl<'ctx> PartialOrd<Move<'ctx>> for Store<'ctx> {
    fn partial_cmp(&self, other: &Move<'ctx>) -> Option<Ordering> {
        use Ordering::*;
        other.partial_cmp(self).map(|v| match v {
            Less => Greater,
            Greater => Less,
            Equal => Equal
        })
    }
}
impl<'ctx> PartialEq<Move<'ctx>> for Store<'ctx> {
    fn eq(&self, _other: &Move<'ctx>) -> bool {
        false
    }
}
impl Hash for Store<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inst.hash(state);
    }
}
fn cmp_ops(l: &Either<*const Move, *const Store>, r: &Either<*const Move, *const Store>) -> Ordering {
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
#[derive(Debug)]
struct Block<'a, 'ctx> {
    block: BasicBlock<'ctx>,
    // these pointers are safe because they're borrowed from a container in a RefCell, which cannot be mutated because of the Ref
    moves: Vec<Either<*const Move<'ctx>, *const Store<'ctx>>>,
    term: Terminator<'ctx>,
    _ref: Ref<'a, ()>
}
#[derive(Debug, Clone, Copy)]
pub enum IsMoved<'ctx> {
    Yes,
    No,
    Maybe(IntValue<'ctx>)
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
    blocks: Vec<Block<'a, 'ctx>>
}
impl<'a, 'ctx> Cfg<'a, 'ctx> {
    /// create a CFG tracking the moves between `start` and `end`
    pub fn new(start: InstructionValue<'ctx>, end: InstructionValue<'ctx>, ctx: &'a CompCtx<'ctx>) -> Self {
        let start_block = start.get_parent().expect("start and end instructions for CFG must be in blocks");
        let end_block = end.get_parent().expect("start and end instructions for CFG must be in blocks");
        if start_block == end_block {
            let inv = 'check: { // end <= start
                let mut i = Some(end);
                while let Some(inst) = i {
                    if inst == start {break 'check true}
                    i = inst.get_next_instruction();
                }
                false
            };
            if inv {return Self {blocks: vec![]}}
            let borrow = ctx.moves.borrow();
            let (moves, stores) = borrow.last().unwrap();
            let mut moves = moves.iter().map(Either::Left).chain(stores.iter().map(Either::Right)).filter(|m| for_both!(m, m => m.inst.get_parent().unwrap() == start_block && **m >= start && **m <= end)).map(|e| match e {
                Either::Left(l) => Either::Left(l as _),
                Either::Right(r) => Either::Right(r as _)
            }).collect::<Vec<_>>();
            moves.sort_by(cmp_ops);
            let term = start_block.get_terminator();
            Self {
                blocks: vec![Block {
                    block: start_block,
                    term: term.map_or(Terminator::Ret, |term| if term.get_opcode() == inkwell::values::InstructionOpcode::Br {
                        todo!()
                    } else {Terminator::Ret}),
                    moves,
                    _ref: Ref::map(borrow, |_| &UNIT)
                }]
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
            moves.entry(start_block).or_insert_with(Vec::new).retain(|e| for_both!(e, m => unsafe {**m >= start}));
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
                moves.sort_by(cmp_ops);
                blocks.push(Block {
                    block, term, moves,
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
                moves.retain(|m| for_both!(m, m => unsafe {**m <= end}));
                moves.sort_by(cmp_ops);
                blocks.push(Block {
                    block: end_block,
                    term, moves,
                    _ref: Ref::map(ctx.moves.borrow(), |_| &UNIT)
                });
            }
            let map = blocks.iter().enumerate().map(|(n, b)| (b.block, n)).collect::<HashMap<_, _>>();
            for block in &mut blocks {
                match block.term {
                    Terminator::UBrLazy(b) => block.term = if let Some(&b) = map.get(&b) {Terminator::UBr(b)} else {Terminator::Ret},
                    Terminator::CBrLazy(c, t, f) => block.term = if let (Some(&t), Some(&f)) = (map.get(&t), map.get(&f)) {Terminator::CBr(c, t, f)} else {Terminator::Ret},
                    _ => {}
                }
            }
            Self {blocks}
        }
    }
    /// Search CFG for double moves
    pub fn validate(&self) -> Vec<DoubleMove> {
        warn();
        let mut errs: Vec<DoubleMove> = vec![];
        errs.sort();
        errs.dedup_by_key(|m| m.loc);
        errs
    }
    /// Check if a value has been moved before an instruction value, or by the end of the graph
    pub fn is_moved(&self, _inst: Option<InstructionValue<'ctx>>) -> IsMoved<'ctx> {
        warn();
        IsMoved::Yes
    }
    /// Insert destructor calls before stores if necessary.
    /// If `at_end` is true, insert the destructors for all values in the top VarMap layer as well
    pub fn insert_dtors(&self, _ctx: &CompCtx<'ctx>, _at_end: bool) {
        warn();
    }
}
use std::sync::atomic::AtomicBool;
static SHOULD_WARN: AtomicBool = AtomicBool::new(true);
fn warn() {
    if SHOULD_WARN.fetch_and(false, std::sync::atomic::Ordering::Relaxed) {
        eprintln!("destructors haven't been implemented yet!");
    }
}