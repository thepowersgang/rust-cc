#[derive(PartialEq, Debug)]
pub struct TypeRef
{
    pub root: Root,
    pub wrappers: Vec<Wrapper>,
}
impl TypeRef {
    pub fn root(root: Root) -> Self {
        TypeRef { root, wrappers: Vec::new() }
    }
    pub fn unit() -> Self {
        TypeRef { root: Root::Tuple(Vec::new()), wrappers: Vec::new() }
    }

    pub fn is_unit(&self) -> bool {
        match self.root {
        Root::Tuple(ref ents) if ents.is_empty() && self.wrappers.is_empty() => true,
        _ => false,
        }
    }
    pub fn wrapped(mut self, w: Wrapper) -> Self {
        self.wrappers.push(w);
        self
    }
}
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Mutability {
    Shared,
    Unique,
    Move,
}
impl Mutability {
    pub fn to_str(&self) -> &'static str {
        match self {
        Mutability::Shared => "const",
        Mutability::Unique => "mut",
        Mutability::Move => "move",
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Root {
    Diverge,
    Str,
    Unsigned(Bits),
    Signed(Bits),
    Float(Bits),
    Named(String),
    Tuple(Vec<TypeRef>),
    Function(Box<FcnTy>),
}
#[derive(PartialEq, Debug)]
pub enum Wrapper {
    Slice,
    Array(usize),
    Pointer(Mutability),
    Borrow(Mutability),
}
#[repr(transparent)]
#[derive(PartialEq,PartialOrd, Debug, Copy, Clone)]
pub struct Bits(u8);
impl Bits {
    pub const SIZE: Bits = Bits(0xFF);
    pub const _8: Bits = Bits(3);
    pub const _16: Bits = Bits(4);
    pub const _32: Bits = Bits(5);
    pub const _64: Bits = Bits(6);
    pub const _128: Bits = Bits(7);
}
impl Bits {
    pub fn mask_unsigned(&self, v: u128) -> u128 {
        if self.0 == 0xFF {
            v & (1 << 64)-1
        }
        else if self.0 >= 7 {
            v
        }
        else {
            v & (1 << (8*self.0))-1
        }
    }
    pub fn mask_signed(&self, v: i128) -> i128 {
        if v < 0 {
            let v = -v;
            -(self.mask_unsigned(v as u128) as i128)
        }
        else {
            self.mask_unsigned(v as u128) as i128
        }
    }
}
impl ::std::fmt::Display for Bits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 == 0xFF {
            "size".fmt(f)
        }
        else {
            (1 << self.0).fmt(f)
        }
    }
}
#[derive(PartialEq, Debug)]
pub struct FcnTy {
    pub abi: String,
    pub args: Vec<TypeRef>,
    pub is_variadic: bool,
    pub ret: TypeRef,
}