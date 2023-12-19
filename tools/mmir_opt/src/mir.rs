use crate::types::TypeRef;

pub struct Function
{
    pub locals: Vec<(String, TypeRef)>,
    pub drop_flags: Vec<(String, bool)>,
    pub blocks: Vec<BasicBlock>,
}
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}
pub enum Statement {
    SpanComment(String),
    Assign(Slot, Value),
}
pub enum Terminator {
    Invalid,
    Return,
    Diverge,
    Goto(usize),
    Call(TerminatorCall),
    If(Slot, usize, usize),
}
pub struct TerminatorCall {
    pub target: CallTarget,
    pub args: Vec<Param>,
    pub dst: Slot,
    pub bb_ret: usize,
    pub bb_panic: usize,
}
pub enum CallTarget {
    Path(String),
    Intrinsic(String, Vec<TypeRef>),
    Value(Slot),
}

pub struct Slot {
    pub root: SlotRoot,
    pub wrappers: Vec<SlotWrapper>,
}
impl Slot {
    pub fn is_local(&self) -> Option<usize> {
        match self.root {
        SlotRoot::Local(i) if self.wrappers.is_empty() => Some(i),
        _ => None,
        }
    }
}
pub enum SlotRoot {
    Named(String),
    Argument(usize),
    Local(usize),
    Return,
}
pub enum SlotWrapper {
    Deref,
    Index(usize),
    Field(usize),
    Downcast(usize),
}

// Aka `RValue`
pub enum Value {
    Constant(Const),
    Use(Slot),
    Borrow(crate::types::Mutability, Slot),
    BinOp(Param, BinOp, Param),
    UniOp(UniOp, Slot),
    Cast(Slot, crate::types::TypeRef),
    DstPtr(Slot),
    DstMeta(Slot),
    Tuple(Vec<Param>),
    Array(Vec<Param>),
    Struct(String, Vec<Param>),
    UnionVariant(String, usize, Param),
    EnumVariant(String, usize, Vec<Param>),
}
#[derive(Debug)]
pub enum UniOp {
    Inv,
    Neg,
}
#[derive(Debug)]
pub enum BinOp {
    Add, Sub, Div, Mul, Rem,
    Shr, Shl,
    BitAnd, BitOr, BitXor,

    Less, Greater,
    LessEqual, GreaterEqual,
    Equals, NotEquals,
}

pub enum Param {
    Const(Const),
    Slot(Slot),
}
#[derive(Debug,Clone)]
pub enum Const {
    Boolean(bool),
    Unsigned(u128, crate::types::Bits),
    Signed(i128, crate::types::Bits),
    Float(f64, crate::types::Bits),
    String(String),
}