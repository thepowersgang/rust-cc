/*
 */

use std::rc::Rc;
use std::cell::RefCell;

#[allow(non_camel_case_types)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum IntClass
{
	Bits(bool,u8),
	Char(bool),
	Short(bool),
	Int(bool),
	Long(bool),
	LongLong(bool),
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub enum FloatClass
{
	Float,
	Double,
	LongDouble,
}

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

#[derive(PartialEq)]
pub struct Type
{
	pub basetype: BaseType,
	pub is_const: bool,
	pub is_volatile: bool,
}

#[derive(Clone)]
#[derive(PartialEq)]
pub enum BaseType
{
	Void,
	Bool,
	Struct(StructRef),
	Enum(EnumRef),
	Union(UnionRef),
	Float(FloatClass),
	Integer(IntClass),
	
	Pointer(Rc<Type>),
	Array(Rc<Type>),	// TODO: Should the array include the size? How to handle non-literal sizes?
	Function(Rc<Type>,Vec<(Rc<Type>,String)>),
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct Struct
{
	name: String,
	items:	Vec<(TypeRef,String)>,
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct Union
{
	name: String,
	items: Option<Vec<(TypeRef,String)>>,
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct Enum
{
	name: String,
	items: Option<Vec<(u64,String)>>,
}

impl ::std::fmt::Debug for Type
{
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>
	{
		write!(fmt, "{}{}{:?}",
			if self.is_const    { "const " } else { "" },
			if self.is_volatile { "volatile " } else { "" },
			self.basetype)
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
		
		&BaseType::Array(ref typeref) => write!(fmt, "{:?}[]", typeref),
		&BaseType::Pointer(ref typeref) => write!(fmt, "*{:?}", typeref),
		&BaseType::Function(ref ret, ref args) => write!(fmt, "Fcn({:?}, {:?})", ret, args),
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
