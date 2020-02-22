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
	Function(FunctionType),
}
#[derive(Clone,PartialEq,Debug)]
pub enum MagicType
{
	VaList,
	Named(String, String),
}
#[derive(Clone,PartialEq)]
pub enum ArraySize
{
	None,
	Fixed(u64),
	Expr(ArraySizeExpr),
}
impl ArraySize {
	pub fn get_value(&self) -> u64 {
		match *self
		{
		ArraySize::None => panic!("No array size?"),
		ArraySize::Fixed(v) => v,
		ArraySize::Expr(ref e) => e.get_value(),
		}
	}
}
impl From<ArraySizeExpr> for ArraySize {
	fn from(v: ArraySizeExpr) -> Self {
		ArraySize::Expr(v)
	}
}
#[derive(Clone)]
pub struct ArraySizeExpr(Rc<::ast::Node>, ::std::cell::Cell<Option<u64>>);
impl ArraySizeExpr {
	pub fn new(n: ::ast::Node) -> Self {
		ArraySizeExpr(Rc::new(n), Default::default())
	}
	pub fn get_value(&self) -> u64 {
		if let Some(v) = self.1.get() {
			return v;
		}
		match self.0.literal_integer()
		{
		Some(v) => {
			self.1.set( Some(v) );
			v
			},
		None => todo!("ArraySizeExpr::get_value - {:?} (not suppored by `literal_integer`)", self.0),
		}
	}
}
impl PartialEq for ArraySizeExpr {
	fn eq(&self, v: &Self) -> bool {
		panic!("TODO: eq for ArraySizeExpr - {:?} == {:?}", self.0, v.0);
	}
}
impl ::std::ops::Deref for ArraySizeExpr {
	type Target = ::ast::Node;
	fn deref(&self) -> &::ast::Node {
		&*self.0
	}
}

#[derive(Debug,Default,PartialEq,Clone)]
pub struct Attributes
{
	pub gcc: Vec<(String, Vec<String>)>,
}

#[derive(Clone,Debug)]
pub struct FunctionType
{
	pub ret: Rc<Type>,
	pub args: Vec<(Rc<Type>, String)>,
	pub attributes: Attributes,
}
impl PartialEq for FunctionType
{
	fn eq(&self, v: &Self) -> bool {
		self.ret == v.ret
			&& self.args.len() == v.args.len()
			// Checks just the base types (ignoring qualifiers like `const` on the top level)
			&& Iterator::zip( self.args.iter(), v.args.iter() ).all( |(a,b)| a.0.basetype == b.0.basetype )
	}
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

	pub fn is_lesser_than(&self, other: &Self) -> bool {
		// If self is a subset of other (no missing bits
		self.v & other.v == self.v
	}

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
#[derive(Debug,PartialEq,Clone,Copy)]
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
	pub const fn char() -> Self { IntClass::Char(None) }
	//pub const fn uchar() -> Self { IntClass::Char(Some(Unsigned)) }
	//pub const fn schar() -> Self { IntClass::Char(Some(Signed)) }
	pub const fn int() -> Self { IntClass::Int(Signed) }

	pub fn signedness(&self) -> Signedness {
		match *self
		{
		IntClass::Bits(s,_) => s,
		IntClass::Char(s) => s.unwrap_or(Signedness::Unsigned),
		IntClass::Short(s) => s,
		IntClass::Int(s) => s,
		IntClass::Long(s) => s,
		IntClass::LongLong(s) => s,
		}
	}
	pub fn clone_with_sgn(&self, s: Signedness) -> Self {
		match *self
		{
		IntClass::Bits(_,b) => IntClass::Bits(s,b),
		IntClass::Char(_) => IntClass::Char(Some(s)),
		IntClass::Short(_) => IntClass::Short(s),
		IntClass::Int(_) => IntClass::Int(s),
		IntClass::Long(_) => IntClass::Long(s),
		IntClass::LongLong(_) => IntClass::LongLong(s),
		}
	}
}

#[derive(Debug,PartialEq,Clone,Copy)]
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
// TODO: Use a type that handles the name too?
pub type StructRef = RcRefCellPtrEq<Struct>;
pub type UnionRef  = RcRefCellPtrEq<Union>;
pub type EnumRef   = RcRefCellPtrEq<Enum>;

#[derive(Debug)]
pub struct RcRefCellPtrEq<T>( Rc<RefCell<T>> );
impl<T> Clone for RcRefCellPtrEq<T> {
	fn clone(&self) -> Self {
		RcRefCellPtrEq(self.0.clone())
	}
}
impl<T> PartialEq for RcRefCellPtrEq<T> {
	fn eq(&self, x: &Self) -> bool {
		Rc::ptr_eq(&self.0, &x.0)
	}
}
impl<T> RcRefCellPtrEq<T> {
	pub fn new(v: T) -> Self {
		RcRefCellPtrEq( Rc::new(RefCell::new(v)) )
	}
	pub fn borrow(&self) -> ::std::cell::Ref<T> {
		self.0.borrow()
	}
	pub fn borrow_mut(&self) -> ::std::cell::RefMut<T> {
		self.0.borrow_mut()
	}
}

#[derive(Debug,PartialEq)]
pub struct Struct
{
	pub name: String,
	pub items: Option<StructBody>,
}
#[derive(Default,Debug,PartialEq)]
pub struct StructBody
{
	pub fields: Vec<(TypeRef, String)>,
	pub attributes: Attributes,
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
impl Enum
{
	pub fn find_var(&self, name: &str) -> Option<usize> {
		if let Some(ref items) = self.items {
			items.iter().position(|v| v.1 == name)
		}
		else {
			None
		}
	}
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
		&BaseType::Function(ref info) => write!(fmt, "Fcn({:?}, {:?})", info.ret, info.args),
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
		&ArraySize::Expr(ref v) => write!(f, "[{:?}]", *v.0),
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

	pub fn deref(&self) -> Option<TypeRef>
	{
		match self.basetype
		{
		BaseType::Pointer(ref ty) => Some( ty.clone() ),
		BaseType::Array(ref ty, _) => Some( ty.clone() ),
		_ => None,
		}
	}
	pub fn get_field(&self, name: &str) -> Option<(u32, TypeRef)> {
		match self.basetype
		{
		BaseType::Struct(ref r) => {
			let b = r.borrow();
			match b.items
			{
			None => todo!("Proper error when getting field of opaque"),
			Some(ref body) => {
				let mut ofs = 0;
				for (fld_ty, fld_name) in &body.fields
				{
					if fld_name == name {
						return Some( (ofs, fld_ty.clone()) );
					}
					ofs += fld_ty.get_size().expect("Opaque type in struct");
				}
				None
				},
			}
			},
		BaseType::Union(ref r) => todo!("Type::get_field({:?}, {})", self, name),
		BaseType::MagicType(_) => todo!("Type::get_field({:?}, {})", self, name),
		_ => None,
		}
	}

	pub fn get_size(&self) -> Option<u32> {
		match self.basetype
		{
		BaseType::Pointer(_) => Some(4),
		BaseType::Integer(IntClass::Char(_)) => Some(1),
		BaseType::Integer(IntClass::Short(_)) => Some(2),
		BaseType::Integer(IntClass::Int(_)) => Some(4),
		BaseType::Integer(IntClass::Long(_)) => Some(4),
		BaseType::Integer(IntClass::LongLong(_)) => Some(8),

		BaseType::Array(ref inner, ref sz) => Some(inner.get_size()? * sz.get_value() as u32),
		_ => todo!("Type::get_size(): {:?}", self),
		}
	}
}

impl Struct
{
	pub fn new_ref(name: &str) -> StructRef
	{
		RcRefCellPtrEq::new(Struct {
			name: name.to_string(),
			items: None,
			})
	}
	
	pub fn is_populated(&self) -> bool
	{
		self.items.is_some()
	}
	pub fn set_items(&mut self, items: StructBody)
	{
		assert!( self.items.is_none() );
		self.items = Some(items);
	}
}

impl Union
{
	pub fn new_ref(name: &str) -> UnionRef
	{
		RcRefCellPtrEq::new(Union {
			name: name.to_string(),
			items: None,
			})
	}
	
	pub fn is_populated(&self) -> bool
	{
		self.items.is_some()
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
		RcRefCellPtrEq::new(Enum {
			name: name.to_string(),
			items: None,
			})
	}
	
	pub fn is_populated(&self) -> bool
	{
		self.items.is_some()
	}
	pub fn set_items(&mut self, items: Vec<(u64,String)>)
	{
		assert!( self.items.is_none() );
		self.items = Some(items);
	}

	pub fn get_item_val(&self, idx: usize) -> Option<u64> {
		self.items.as_ref()
			.expect("Enum::get_item_val on opaque enum")
			.get(idx)
			.map(|v| v.0)
	}
}

// vim: ft=rust
