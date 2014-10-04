/*
 */

use std::rc::Rc;
use std::cell::RefCell;

#[allow(non_camel_case_types)]
#[deriving(Show)]
#[deriving(PartialEq)]
#[deriving(Clone)]
pub enum IntClass
{
	IntClass_Char(bool),
	IntClass_Short(bool),
	IntClass_Int(bool),
	IntClass_Long(bool),
	IntClass_LongLong(bool),
}

#[allow(non_camel_case_types)]
#[deriving(Show)]
#[deriving(PartialEq)]
#[deriving(Clone)]
pub enum FloatClass
{
	FloatClass_Float,
	FloatClass_Double,
	FloatClass_LongDouble,
}

pub enum StorageClass
{
	StorageAuto,
	StorageExtern,
	StorageStatic,
	StorageRegister,
}

pub type TypeRef = Rc<Type>;
pub type StructRef = Rc<RefCell<Struct>>;

#[deriving(PartialEq)]
pub struct Type
{
	pub basetype: BaseType,
	pub is_const: bool,
	pub is_volatile: bool,
}

#[deriving(Clone)]
#[deriving(PartialEq)]
pub enum BaseType
{
	TypeVoid,
	TypeStruct(StructRef),
	TypeFloat(FloatClass),
	TypeInteger(IntClass),
	TypePointer(Rc<Type>),
	TypeFunction(Rc<Type>,Vec<(Rc<Type>,String)>),
}

#[deriving(Show)]
#[deriving(PartialEq)]
pub struct Struct
{
	name: String,
	items:	Vec<(TypeRef,String)>,
}

impl ::std::fmt::Show for Type
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError>
	{
		write!(fmt, "{}{}{}",
			if self.is_const    { "const " } else { "" },
			if self.is_volatile { "volatile " } else { "" },
			self.basetype)
	}
}

impl ::std::fmt::Show for BaseType
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError>
	{
		match *self
		{
		TypeVoid => write!(fmt, "void"),
		TypeStruct(ref sr) => write!(fmt, "struct {}", sr.borrow().name),
		TypeFloat(fc) => write!(fmt, "{}", fc),
		TypeInteger(ic) => write!(fmt, "{}", ic),
		TypePointer(ref typeref) => write!(fmt, "*{}", typeref),
		TypeFunction(ref ret, ref args) => write!(fmt, "Fcn({}, {})", ret, args),
		}
	}
}

impl ::std::fmt::Show for RefCell<Struct>
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError>
	{
		match self.try_borrow()
		{
		Some(x) => write!(fmt, "{}", x),
		None => write!(fmt, "Struct[INUSE]")
		}
	}
}

impl Type
{
	pub fn new_ref(basetype: BaseType, is_const: bool, is_volatile: bool) -> TypeRef
	{
		Rc::new(Type {
			basetype: basetype,
			is_const: is_const,
			is_volatile: is_volatile,
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

// vim: ft=rust
