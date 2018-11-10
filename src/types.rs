//! C types

use std::rc::Rc;
use std::cell::RefCell;

#[derive(PartialEq)]
pub struct Type
{
	pub basetype: BaseType,
	pub qualifiers: Qualifiers,
}

#[derive(PartialEq,Clone)]	/* Debug impl is manual */
pub enum BaseType
{
	Void,
	Bool,
	Struct(StructRef),
	Enum(EnumRef),
	Union(UnionRef),
	Float(FloatClass),
	Integer(IntClass),

	MagicType(MagicType),
	
	Pointer(Rc<Type>),
	Array(Rc<Type>, ArraySize),
	Function(Rc<Type>,Vec<(Rc<Type>,String)>),
}
#[derive(Clone,PartialEq,Debug)]
pub enum MagicType
{
	VaList,
	Named(String),
}
#[derive(Clone,PartialEq)]
pub enum ArraySize
{
	None,
	Fixed(u64),
	//Expr(::ast::Node),
}

/// Boolean signedness
#[derive(Debug,PartialEq,Clone,Copy)]
pub enum Signedness
{
	Signed,
	Unsigned,
}
pub use self::Signedness::*;
impl Signedness {
	pub fn from_bool_signed(s: bool) -> Self {
		if s {
			Signedness::Signed
		}
		else {
			Signedness::Unsigned
		}
	}
	pub fn is_unsigned(&self) -> bool { *self == Signedness::Unsigned }
}
/// Qualifiers on a type (const, volatile, restrict)
// NOTE: `const volatile` is valid and has meaning (code can't change it, but hardware could)
#[derive(PartialEq,Clone)]
pub struct Qualifiers {
	v: u8,
}
impl Qualifiers {
	pub fn new() -> Self { Qualifiers { v: 0 } }

	pub fn set_const(&mut self) -> &mut Self { self.v |= 1; self }
	pub fn set_volatile(&mut self) -> &mut Self { self.v |= 2; self }
	pub fn set_restrict(&mut self) -> &mut Self { self.v |= 4; self }

	pub fn is_const(&self) -> bool { self.v & 1 != 0 }
	pub fn is_volatile(&self) -> bool { self.v & 2 != 0 }
	pub fn is_restrict(&self) -> bool { self.v & 4 != 0 }

	pub fn merge_from(&mut self, other: &Qualifiers) {
		self.v |= other.v;
	}
}
impl ::std::fmt::Debug for Qualifiers {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(f, "{}{}{}",
			if self.is_const() { "const " } else { "" },
			if self.is_volatile() { "volatile " } else { "" },
			if self.is_restrict() { "restrict " } else { "" },
			)
	}
}

/// Various integer types
#[derive(Debug,PartialEq,Clone)]
pub enum IntClass
{
	/// Fixed-size type
	Bits(Signedness,u8),
	/// `char` (three variants: char, signed char, and unsigned char)
	Char(Option<Signedness>),
	/// `[un]signed short [int]`
	Short(Signedness),
	/// `[un]signed int`
	Int(Signedness),
	/// `[un]signed long [int]`
	Long(Signedness),
	/// `[un]signed long long [int]`
	LongLong(Signedness),
}
impl IntClass {
	pub fn char() -> Self { IntClass::Char(None) }
	//pub fn uchar() -> Self { IntClass::Char(Some(Unsigned)) }
	//pub fn schar() -> Self { IntClass::Char(Some(Signed)) }
	pub const fn int() -> Self { IntClass::Int(Signed) }
}

#[derive(Debug,PartialEq,Clone)]
pub enum FloatClass
{
	Float,
	Double,
	LongDouble,
}

#[derive(Debug,PartialEq,Clone)]
pub enum StorageClass
{
	Auto,
	Extern,
	Static,
	Register,
}

pub type TypeRef = Rc<Type>;
pub type StructRef = Rc<RefCell<Struct>>;
pub type UnionRef  = Rc<RefCell<Union>>;
pub type EnumRef   = Rc<RefCell<Enum>>;

#[derive(Debug,PartialEq)]
pub struct Struct
{
	pub name: String,
	items:	Vec<(TypeRef,String)>,
}

#[derive(Debug,PartialEq)]
pub struct Union
{
	pub name: String,
	items: Option<Vec<(TypeRef,String)>>,
}

#[derive(Debug,PartialEq)]
pub struct Enum
{
	pub name: String,
	items: Option<Vec<(u64,String)>>,
}

impl ::std::fmt::Debug for Type
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>
	{
		write!(fmt, "{:?}{:?}", self.qualifiers, self.basetype)
	}
}

impl ::std::fmt::Debug for BaseType
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>
	{
		match self
		{
		&BaseType::Void => write!(fmt, "void"),
		&BaseType::Bool => write!(fmt, "_Bool"),
		&BaseType::Struct(ref sr) => write!(fmt, "struct {:?}", sr.borrow().name),
		&BaseType::Union(ref ur)  => write!(fmt, "union {:?}",  ur.borrow().name),
		&BaseType::Enum(ref er)   => write!(fmt, "enum {:?}",   er.borrow().name),
		&BaseType::Float(ref fc) => write!(fmt, "{:?}", fc),
		&BaseType::Integer(ref ic) => write!(fmt, "{:?}", ic),
		&BaseType::MagicType(ref v) => write!(fmt, "/*magic*/ {:?}", v),
		
		&BaseType::Array(ref typeref, ref size) => write!(fmt, "{:?}{}", typeref, size),
		&BaseType::Pointer(ref typeref) => write!(fmt, "*{:?}", typeref),
		&BaseType::Function(ref ret, ref args) => write!(fmt, "Fcn({:?}, {:?})", ret, args),
		}
	}
}

impl ::std::fmt::Display for ArraySize
{
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result
	{
		match self
		{
		&ArraySize::None => f.write_str("[]"),
		&ArraySize::Fixed(v) => write!(f, "[{}]", v),
		}
	}
}

impl Type
{
	pub fn new_ref_bare(basetype: BaseType) -> TypeRef
	{
		Type::new_ref(basetype, Qualifiers::new())
	}
	pub fn new_ref(basetype: BaseType, qualifiers: Qualifiers) -> TypeRef
	{
		Rc::new(Type {
			basetype: basetype,
			qualifiers: qualifiers,
			})
	}
}

impl Struct
{
	pub fn new_ref(name: &str) -> StructRef
	{
		Rc::new( RefCell::new(Struct {
			name: name.to_string(),
			items: Vec::new(),
			}) )
	}
	
	pub fn is_populated(&self) -> bool
	{
		return self.items.is_empty() == false;
	}
	pub fn set_items(&mut self, items: Vec<(TypeRef,String)>)
	{
		assert!( self.items.is_empty() );
		self.items = items;
	}
}

impl Union
{
	pub fn new_ref(name: &str) -> UnionRef
	{
		Rc::new(RefCell::new( Union {
			name: name.to_string(),
			items: None,
			} ))
	}
	
	pub fn is_populated(&self) -> bool
	{
		return self.items.is_some();
	}
	pub fn set_items(&mut self, items: Vec<(TypeRef,String)>)
	{
		assert!( self.items.is_none() );
		self.items = Some(items);
	}
}

impl Enum
{
	pub fn new_ref(name: &str) -> EnumRef
	{
		Rc::new(RefCell::new( Enum {
			name: name.to_string(),
			items: None,
			} ))
	}
	
	pub fn is_populated(&self) -> bool
	{
		return self.items.is_some();
	}
	pub fn set_items(&mut self, items: Vec<(u64,String)>)
	{
		assert!( self.items.is_none() );
		self.items = Some(items);
	}
}

// vim: ft=rust
