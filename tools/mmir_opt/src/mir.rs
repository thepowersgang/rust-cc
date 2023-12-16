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
    Cast(Param, crate::types::TypeRef),
    DstPtr(Param),
    DstMeta(Slot),
    Tuple(Vec<Param>),
    Array(Vec<Param>),
    Struct(String, Vec<Param>),
    Variant(String, usize, Slot),
}
pub enum UniOp {
    Inv,
    Neg,
}
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
pub enum Const {
    Boolean(bool),
    Unsigned(u128, crate::types::Bits),
    Signed(i128, crate::types::Bits),
    Float(f64, crate::types::Bits),
    String(String),
}