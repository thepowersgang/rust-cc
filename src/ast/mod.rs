/*!
 * Representation of the C source in a tree
 */
use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub mod pretty_print;

#[derive(Default)]
pub struct Program
{
	// - TODO: Store type definition order too.
	item_order: Vec<ItemRef>,

	typedefs: HashMap<String, ::types::TypeRef>,
	structs: HashMap<String, ::types::StructRef>,
	unions: HashMap<String, ::types::UnionRef>,
	enums: HashMap<String, ::types::EnumRef>,
	// Aka global variables/functions
	symbols: HashMap<String, Symbol>,
}
enum ItemRef
{
	ValueDecl(String),
	Value(String),

	Typedef(String),
	Struct(String),
	Union(String),
	Enum(String),
}

struct Symbol
{
	name: String,
	symtype: ::types::TypeRef,
	value: Option<Node>,
}

impl Program
{
	pub fn new() -> Program
	{
		Program {
			..::std::default::Default::default()
		}
	}
	
	pub fn define_variable(&mut self, typeid: ::types::TypeRef, name: String, value: Option<Node>)
	{
		if value.is_some() {
			self.item_order.push(ItemRef::Value(name.clone()));
		}
		else {
			self.item_order.push(ItemRef::ValueDecl(name.clone()));
		}
		error!("TODO: Define variable '{}': '{:?}' = {:?}", name, typeid, value);
		match self.symbols.entry(name.clone())
		{
		Entry::Occupied(mut e) => {
			if e.get().symtype != typeid {
				// ERROR: Conflicting declarations
			}
			else if e.get().value.is_some() {
				if value.is_some() {
					// ERROR: Duplicated definition
				}
				else {
					// WARN: Trailing declaration
				}
			}
			else {
				e.get_mut().value = value;
			}
			},
		Entry::Vacant(e) => {
			e.insert(Symbol {
				name: name,
				symtype: typeid,
				value: value,
				});
			},
		}
	}
	
	pub fn set_typedef(&mut self, name: String, typeid: ::types::TypeRef) -> bool
	{
		self.item_order.push(ItemRef::Typedef(name.clone()));
		self.typedefs.insert(name, typeid).is_none()
	}
	pub fn get_typedef(&self, name: &str) -> Option<::types::TypeRef>
	{
		// HACK! Define __builtin_va_list (a GCC internal) to be a magic type
		if name == "__builtin_va_list" {
			return Some( ::types::Type::new_ref_bare(::types::BaseType::MagicType(::types::MagicType::VaList)) );
		}
		
		self.typedefs.get(name)
			.map(::std::rc::Rc::clone)
	}
	
	pub fn get_struct(&mut self, name: &str) -> ::types::StructRef
	{
		if name == "" {
			::types::Struct::new_ref("")
		}
		else {
			self.structs.entry(name.to_string())
				.or_insert_with(|| ::types::Struct::new_ref(name))
				.clone()
		}
	}
	pub fn get_union(&mut self, name: &str) -> ::types::UnionRef
	{
		if name == "" {
			::types::Union::new_ref("")
		}
		else {
			self.unions.entry(name.to_string())
				.or_insert_with(|| ::types::Union::new_ref(name))
				.clone()
		}
	}
	pub fn get_enum(&mut self, name: &str) -> ::types::EnumRef
	{
		if name == "" {
			::types::Enum::new_ref("")
		}
		else {
			self.enums.entry(name.to_string())
				.or_insert_with(|| ::types::Enum::new_ref(name))
				.clone()
		}
	}

	pub fn make_struct(&mut self, name: &str, items: Vec<(::types::TypeRef,String)>) -> Result<::types::StructRef,()> {
		self.item_order.push(ItemRef::Struct(name.to_owned()));
		let sr = self.get_struct(name);
		let ispop = sr.borrow().is_populated();
		
		if ispop {
			Err( () )
		}
		else {
			// Set items in enum
			sr.borrow_mut().set_items(items);
			Ok( sr )
		}
	}
	pub fn make_union(&mut self, name: &str, items: Vec<(::types::TypeRef,String)>) -> Result<::types::UnionRef,()> {
		self.item_order.push(ItemRef::Union(name.to_owned()));
		let ur = self.get_union(name);
		let ispop = ur.borrow().is_populated();
		
		if ispop {
			Err( () )
		}
		else {
			// Set items in enum
			ur.borrow_mut().set_items(items);
			Ok( ur )
		}
	}
	pub fn make_enum(&mut self, name: &str, items: Vec<(u64,String)>) -> Result<::types::EnumRef,Option<String>> {
		self.item_order.push(ItemRef::Enum(name.to_owned()));
		let er = self.get_enum(name);
		let ispop = er.borrow().is_populated();
		
		if ispop {
			Err(None)
		}
		else {
			// Insert 'items' into the global constant scope
			error!("TODO: Insert enum values");
			// Set items in enum
			er.borrow_mut().set_items(items);
			Ok( er )
		}
	}
}

#[derive(Debug)]
pub enum Node
{
	Block(Vec<Node>),
	StmtList(Vec<Node>),	// Comma operator
	DefVar(::types::TypeRef,String,Option<Box<Node>>),
	
	Identifier(String),
	String(String),
	Integer(u64),
	Float(f64),
	ListLiteral(Vec<Node>),	// {a, b, c}
	ArrayLiteral(Vec<(usize,Node)>),	// {[0] = a, [1] = b, [2] = c}
	StructLiteral(Vec<(String,Node)>),	// {.a = a, .b = b, .c = c}
	
	IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>),
	WhileLoop(Box<Node>, Box<Node>),
	DoWhileLoop(Box<Node>, Box<Node>),
	ForLoop(Option<Box<Node>>, Option<Box<Node>>, Option<Box<Node>>, Box<Node>),
	
	Label(String),
	Goto(String),
	Continue,
	Break,
	
	Switch(Box<Node>, Vec<Node>),
	CaseDefault,
	CaseSingle(u64),
	CaseRange(u64, u64),
	
	FcnCall(Box<Node>, Vec<Node>),
	
	Return(Option<Box<Node>>),
	
	Assign(Box<Node>, Box<Node>),
	AssignOp(BinOp, Box<Node>, Box<Node>),
	
	Cast(::types::TypeRef,Box<Node>),
	SizeofType(::types::TypeRef),
	SizeofExpr(Box<Node>),
	
	Ternary(Box<Node>,Box<Node>,Box<Node>),
	UniOp(UniOp, Box<Node>),
	BinOp(BinOp, Box<Node>, Box<Node>),
	
	Index(Box<Node>, Box<Node>),
	DerefMember(Box<Node>, String),
	Member(Box<Node>, String),
}

#[derive(Debug)]
pub enum BinOp
{
	LogicAnd,
	LogicOr,
	
	BitAnd,
	BitOr,
	BitXor,
	
	ShiftLeft,
	ShiftRight,
	
	CmpEqu,
	CmpNEqu,
	CmpLt,
	CmpLtE,
	CmpGt,
	CmpGtE,
	
	Add,
	Sub,
	
	Mul,
	Div,
	Mod,
}

#[derive(Debug)]
pub enum UniOp
{
	Neg,
	BitNot,
	LogicNot,
	PreInc,
	PreDec,
	PostInc,
	PostDec,
	Address,
	Deref,
}

impl Node
{
	/// Attempt to interpret the node as a trivally constant integer
	pub fn literal_integer(&self) -> Option<u64>
	{
		match self
		{
		&Node::Integer(v) => Some(v),
		&Node::UniOp(ref op,ref a) => match (op,a.literal_integer())
			{
			(&UniOp::Neg,Some(a)) => Some(!a + 1),
			_ => None,
			},
		&Node::BinOp(ref op,ref a,ref b) => match (op,a.literal_integer(), b.literal_integer())
			{
			(&BinOp::Sub,Some(a),Some(b)) => Some(a-b),
			_ => None,
			},
		_ => None,
		}
	}
}

// vim: ft=rust
