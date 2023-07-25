use crate::*;
use bstr::ByteSlice;
use cobalt_llvm::inkwell::values::FunctionValue;
use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, InstructionValue, IntValue};
use inkwell::basic_block::BasicBlock;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
#[derive(Debug, Clone, Copy)]
pub enum Terminator<'ctx> {
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
    pub name: String
}
/// compare the order in which moves (or their underlying instructions) occur. 
impl PartialOrd for Move<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering::*;
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
#[derive(Debug, Clone)]
pub struct Block<'ctx> {
    pub moves: Vec<Move<'ctx>>,
    pub block: BasicBlock<'ctx>,
    pub term: Terminator<'ctx>
}
impl<'ctx> Block<'ctx> {
    pub fn parse(block: BasicBlock<'ctx>, ctx: &CompCtx<'ctx>) -> Self {
        let kind = ctx.context.get_kind_id("cobalt.move");
        let mut i = block.get_first_instruction();
        let mut this = Block {block, moves: vec![], term: Terminator::Ret};
        while let Some(inst) = i {
            'node_parse: {
                use BasicMetadataValueEnum::MetadataValue;
                if let Some(moves) = inst.get_metadata(kind) {
                    if !moves.is_node() {break 'node_parse}
                    let vals = moves.get_node_values();
                    this.moves.reserve(vals.len());
                    for (idx, val) in vals.into_iter().enumerate() {
                        if let MetadataValue(val) = val {
                            if !val.is_node() {continue}
                            if val.get_node_size() != 2 {continue}
                            let vals = val.get_node_values();
                            if let (MetadataValue(loc), MetadataValue(name)) = (vals[0], vals[1]) {
                                if !loc.is_string() {continue}
                                if !name.is_string() {continue}
                                if let Some(name) = name.get_string_value().and_then(|v| std::str::from_utf8(v.to_bytes()).ok().map(String::from)) {
                                    if let Some(loc) = loc.get_string_value().and_then(|v| {
                                        let v = v.to_bytes();
                                        let (off, len) = v.split_at(v.find_byte(b'+')?);
                                        Some(SourceSpan::from((std::str::from_utf8(off).ok()?.parse::<usize>().ok()?, std::str::from_utf8(len).ok()?.parse::<usize>().ok()?)))
                                    }) {
                                        this.moves.push(Move {inst, idx, name, loc})
                                    }
                                }
                            }
                        }
                    }
                }
            }
            i = inst.get_next_instruction();
            if i.is_none() {
                use inkwell::values::InstructionOpcode as Op;
                this.term = match dbg!(inst).get_opcode() {
                    Op::Br => match inst.get_num_operands() {
                        1 => if let Some(either::Right(blk)) = inst.get_operand(0) {Terminator::UBrLazy(blk)} else {Terminator::Ret},
                        3 => if let (
                            Some(either::Left(cond)),
                            Some(either::Right(itb)),
                            Some(either::Right(ifb))
                        ) = (inst.get_operand(0), inst.get_operand(1), inst.get_operand(2)) {Terminator::CBrLazy(cond, itb, ifb)} else {Terminator::Ret}
                        _ => Terminator::Ret
                    }
                    _ => Terminator::Ret
                };
            }
        }
        this
    }
}
pub struct Cfg<'ctx> {
    blocks: Vec<Block<'ctx>>,
    finalized: bool
}
impl<'ctx> Cfg<'ctx> {
    /// take a list of blocks and create a CFG that has *not* been finalized
    #[inline(always)]
    pub fn new(blocks: Vec<Block<'ctx>>) -> Self {Self {blocks, finalized: false}}
    #[inline(always)]
    pub fn parse_fn(f: FunctionValue<'ctx>, ctx: &CompCtx<'ctx>) -> Self {
        let mut this = Self {
            blocks: f.get_basic_blocks().into_iter().map(|block| Block::parse(block, ctx)).collect(),
            finalized: false
        };
        this.finalize();
        this
    }
    #[inline(always)]
    pub fn parse_blocks(blocks: impl IntoIterator<Item = BasicBlock<'ctx>>, ctx: &CompCtx<'ctx>) -> Self {
        let mut this = Self {
            blocks: blocks.into_iter().map(|block| Block::parse(block, ctx)).collect(),
            finalized: false
        };
        this.finalize();
        this
    }
    #[inline(always)]
    pub fn blocks(&self) -> &Vec<Block<'ctx>> {&self.blocks}
    #[inline(always)]
    pub fn blocks_mut(&mut self) -> &mut Vec<Block<'ctx>> {
        self.finalized = false;
        &mut self.blocks
    }
    #[inline(always)]
    pub fn into_blocks(self) -> Vec<Block<'ctx>> {self.blocks}
    
    /// finalize the blocks, converting lazy terminators into normal ones (replace references to blocks with their indices)
    pub fn finalize(&mut self) {
        if self.finalized {return}
        let blocks = self.blocks.iter().enumerate().map(|(n, &Block {block, ..})| (block, n)).collect::<HashMap<_, _>>();
        for Block {term, ..} in &mut self.blocks {
            match term {
                Terminator::UBrLazy(b) =>
                    if let Some(&b) = blocks.get(b) {
                        *term = Terminator::UBr(b)
                    }
                    else {
                        *term = Terminator::Ret
                    }
                Terminator::CBrLazy(c, t, f) =>
                    if let (Some(&t), Some(&f)) = (blocks.get(t), blocks.get(f)) {
                        *term = Terminator::CBr(*c, t, f);
                    }
                    else {
                        *term = Terminator::Ret
                    }
                _ => {}
            }
        }
        self.finalized = true;
    }
    /// validate the affinity of a CFG: all values are moved from at most once
    /// Double moves are returned
    /// returns Err if not finalized
    pub fn validate(&self) -> Result<Vec<(&Move<'ctx>, &Move<'ctx>)>, NotFinalized> {
        if !self.finalized {return Err(NotFinalized)}
        let mut errs = vec![];
        for start in 0..self.blocks.len() {
            let mut moves = HashMap::<&str, &Move>::new();
            for m in &self.blocks[start].moves {
                match moves.entry(m.name.as_str()) {
                    Entry::Occupied(e) => {
                        let e = *e.get();
                        if m < e {
                            errs.push((m, e));
                        }
                        else {
                            errs.push((e, m));
                        }
                    }
                    Entry::Vacant(e) => {e.insert(m);}
                }
            }
            let mut queue = match self.blocks[start].term {
                Terminator::Ret => VecDeque::new(),
                Terminator::UBr(b) => VecDeque::from([b]),
                Terminator::CBr(_, t, f) => VecDeque::from([t, f]),
                _ => unreachable!()
            };
            while let Some(next) = queue.pop_front() {
                let block = &self.blocks[next];
                for m in &block.moves {
                    if let Some(e) = moves.get(m.name.as_str()) {
                        errs.push((e, m));
                    }
                }
                match block.term {
                    Terminator::Ret => {},
                    Terminator::UBr(b) => queue.push_back(b),
                    Terminator::CBr(_, t, f) => {
                        queue.push_back(t);
                        queue.push_back(f);
                    }
                    _ => unreachable!()
                }
            }
        }
        errs.sort_by_key(|v| v.0.name.as_str());
        // TODO: error deduplication
        Ok(errs)
    }
    /// check if the variable with a given name has been moved from by an instruction
    /// returns Err if not finalized
    pub fn is_moved(&self, _name: &str, _inst: Option<InstructionValue<'ctx>>) -> Result<IsMoved<'ctx>, NotFinalized> {
        if !self.finalized {return Err(NotFinalized)}
        if self.blocks.is_empty() {return Ok(IsMoved::No)}
        // TODO: implement
        Ok(IsMoved::No)
    }
}
#[derive(Debug, Clone, Copy)]
pub struct NotFinalized;
#[derive(Debug, Clone, Copy)]
pub enum IsMoved<'ctx> {
    Yes,
    No,
    Maybe(IntValue<'ctx>)
}