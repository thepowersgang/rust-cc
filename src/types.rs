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
	IntClass_Bits(bool,uint),
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
pub type UnionRef = Rc<Union>;
pub type EnumRef = Rc<Enum>;

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
	TypeBool,
	TypeStruct(StructRef),
	TypeEnum(EnumRef),
	TypeUnion(UnionRef),
	TypeFloat(FloatClass),
	TypeInteger(IntClass),
	
	TypePointer(Rc<Type>),
	TypeArray(Rc<Type>),	// TODO: Should the array include the size? How to handle non-literal sizes?
	TypeFunction(Rc<Type>,Vec<(Rc<Type>,String)>),
}

#[deriving(Show)]
#[deriving(PartialEq)]
pub struct Struct
{
	name: String,
	items:	Vec<(TypeRef,String)>,
}

#[deriving(Show)]
#[deriving(PartialEq)]
pub struct Union
{
	name: String,
	items:	Vec<(TypeRef,String)>,
}

#[deriving(Show)]
#[deriving(PartialEq)]
pub struct Enum
{
	name: String,
	items: Vec<(uint,String)>,
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
		TypeBool => write!(fmt, "_Bool"),
		TypeStruct(ref sr) => write!(fmt, "struct {}", sr.borrow().name),
		TypeUnion(ref ur)  => write!(fmt, "union {}", ur.name),
		TypeEnum(ref er)   => write!(fmt, "enum {}", er.name),
		TypeFloat(fc) => write!(fmt, "{}", fc),
		TypeInteger(ic) => write!(fmt, "{}", ic),
		
		TypeArray(ref typeref) => write!(fmt, "{}[]", typeref),
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

impl Union
{
	pub fn new_ref(name: &str, items: Vec<(TypeRef,String)>) -> UnionRef
	{
		Rc::new( Union {
			name: name.to_string(),
			items: items,
			} )
	}
}

impl Enum
{
	pub fn new_ref(name: &str, items: Vec<(uint,String)>) -> EnumRef
	{
		Rc::new( Enum {
			name: name.to_string(),
			items: items,
			} )
	}
}

// vim: ft=rust
