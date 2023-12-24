use crate::types::TypeRef;

#[derive(PartialEq)]
pub struct Function
{
    pub locals: Vec<(String, TypeRef)>,
    pub drop_flags: Vec<(String, bool)>,
    pub blocks: Vec<BasicBlock>,
}
#[derive(PartialEq)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}
#[derive(Debug)]
pub enum Statement {
    SpanComment(String),
    Assign(Slot, Value),
}
impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::SpanComment(_), Self::SpanComment(_)) => true,
            (Self::Assign(l0, l1), Self::Assign(r0, r1)) => l0 == r0 && l1 == r1,
            _ => false,
        }
    }
}
#[derive(PartialEq, Debug)]
pub enum Terminator {
    Removed,
    Invalid,
    Return,
    Diverge,
    Goto(usize),
    Call(TerminatorCall),
    If(Slot, usize, usize),
    SwitchValue(Slot, SwitchValues, Vec<usize>, usize)
}
#[derive(PartialEq, Debug)]
pub struct TerminatorCall {
    pub target: CallTarget,
    pub args: Vec<Param>,
    pub dst: Slot,
    pub bb_ret: usize,
    pub bb_panic: usize,
}
#[derive(PartialEq, Debug)]
pub enum CallTarget {
    Path(String),
    Intrinsic(String, Vec<TypeRef>),
    Value(Slot),
}
#[derive(PartialEq, Debug)]
pub enum SwitchValues {
    Signed(Vec<i128>),
    Unsigned(Vec<u128>),
    Float(Vec<f64>),
    String(Vec< Vec<u8> >),
}

#[derive(PartialEq, Debug)]
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
#[derive(PartialEq, Debug)]
pub enum SlotRoot {
    Named(String),
    Argument(usize),
    Local(usize),
    Return,
}
#[derive(PartialEq, Debug)]
pub enum SlotWrapper {
    Deref,
    Index(usize),
    Field(usize),
    Downcast(usize),
}

// Aka `RValue`
#[derive(PartialEq, Debug)]
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
#[derive(PartialEq, Debug)]
pub enum UniOp {
    Inv,
    Neg,
}
#[derive(PartialEq, Debug)]
pub enum BinOp {
    Add, Sub, Div, Mul, Rem,
    Shr, Shl,
    BitAnd, BitOr, BitXor,

    Less, Greater,
    LessEqual, GreaterEqual,
    Equals, NotEquals,
}

#[derive(PartialEq, Debug)]
pub enum Param {
    Const(Const),
    Slot(Slot),
}
#[derive(PartialEq, Debug, Clone)]
pub enum Const {
    Boolean(bool),
    Unsigned(u128, crate::types::Bits),
    Signed(i128, crate::types::Bits),
    Float(f64, crate::types::Bits),
    String(String),
    ItemAddr(String),
}