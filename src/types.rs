/*
 */

use std::rc::Rc;

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

pub type TypeRef = Rc<Type>;
pub type StructRef = Rc<Struct>;

#[deriving(PartialEq)]
pub struct Type
{
	pub basetype: BaseType,
	pub is_const: bool,
	pub is_volatile: bool,
}

#[deriving(Show)]
#[deriving(Clone)]
#[deriving(PartialEq)]
pub enum BaseType
{
	TypeVoid,
	TypeStruct(Rc<Struct>),
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
	items:	Vec<(String,TypeRef)>,
}

impl ::std::fmt::Show for Type
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::FormatError>
	{
		write!(fmt, "'{}{}{}'",
			if self.is_const    { "const " } else { "" },
			if self.is_volatile { "volatile " } else { "" },
			self.basetype)
	}
}

impl Type
{
	pub fn new_ref(basetype: BaseType, is_const: bool, is_volatile: bool) -> TypeRef
	{
		::std::rc::Rc::new( Type {
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
		::std::rc::Rc::new( Struct {
			name: name.to_string(),
			items: Vec::new(),
			})
	}
	
	pub fn is_populated(&self) -> bool
	{
		return self.items.is_empty() == false;
	}
}

// vim: ft=rust
