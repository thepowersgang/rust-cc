/*
 */
use std::collections::HashMap;

#[deriving(Default)]
pub struct Program
{
	typedefs: HashMap<String,::types::TypeRef>,
	structs: HashMap<String, ::types::StructRef>,
	unions: HashMap<String, ::types::UnionRef>,
	enums: HashMap<String, ::types::EnumRef>,
	symbols: ::std::vec::Vec<Symbol>,
}

struct Symbol
{
	name: String,
	symtype: ::types::TypeRef,
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
		error!("TODO: Define variable '{}': '{}' = {}", name, typeid, value);
	}
	
	pub fn set_typedef(&mut self, name: String, typeid: ::types::TypeRef) -> bool
	{
		self.typedefs.insert(name, typeid)
	}
	pub fn get_typedef(&self, name: &str) -> Option<::types::TypeRef>
	{
		// HACK! Define __builtin_va_list (a GCC internal) to be void
		// TODO: It should be its own type
		if name == "__builtin_va_list" {
			return Some( ::types::Type::new_ref(::types::TypeVoid, false, false) );
		}
		return match self.typedefs.find( &name.to_string() )
			{
			Some(x) => Some(x.clone()),
			None => None
			};
	}
	
	pub fn get_struct(&mut self, name: &str) -> ::types::StructRef
	{
		if name == ""
		{
			return ::types::Struct::new_ref("");
		}
		else
		{
			let key = name.to_string();
			if ! self.structs.contains_key(&key)
			{
				self.structs.insert(key.clone(), ::types::Struct::new_ref(name));
			}
			return self.structs.find(&key).unwrap().clone();
		}
	}
	
	pub fn get_union(&mut self, name: &str) -> ::types::UnionRef
	{
		if name == "" {
			::types::Union::new_ref(name)
		}
		else {
			match self.unions.entry(name.to_string())
			{
			::std::collections::hashmap::Occupied(s) => s.get().clone(),
			::std::collections::hashmap::Vacant(h) => {
				h.set(::types::Union::new_ref(name)).clone()
				}
			}
		}
	}
	pub fn make_union(&mut self, name: &str, items: Vec<(::types::TypeRef,String)>) -> Result<::types::UnionRef,()> {
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
	
	pub fn get_enum(&mut self, name: &str) -> ::types::EnumRef
	{
		if name == "" {
			::types::Enum::new_ref(name)
		}
		else {
			match self.enums.entry(name.to_string())
			{
			::std::collections::hashmap::Occupied(s) => s.get().clone(),
			::std::collections::hashmap::Vacant(h) => {
				h.set(::types::Enum::new_ref(name)).clone()
				}
			}
		}
	}
	pub fn make_enum(&mut self, name: &str, items: Vec<(uint,String)>) -> Result<::types::EnumRef,Option<String>> {
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

#[deriving(Show)]
pub enum Node
{
	NodeBlock(Vec<Node>),
	NodeStmtList(Vec<Node>),	// Comma operator
	NodeDefVar(::types::TypeRef,String,Option<Box<Node>>),
	
	NodeIdentifier(String),
	NodeString(String),
	NodeInteger(u64),
	NodeFloat(f64),
	NodeListLiteral(Vec<Node>),	// {a, b, c}
	NodeArrayLiteral(Vec<(uint,Node)>),	// {[0] = a, [1] = b, [2] = c}
	NodeStructLiteral(Vec<(String,Node)>),	// {.a = a, .b = b, .c = c}
	
	NodeIfStatement(Box<Node>, Box<Node>, Option<Box<Node>>),
	NodeWhileLoop(Box<Node>, Box<Node>),
	NodeDoWhileLoop(Box<Node>, Box<Node>),
	NodeForLoop(Option<Box<Node>>, Option<Box<Node>>, Option<Box<Node>>, Box<Node>),
	
	NodeLabel(String),
	NodeGoto(String),
	NodeContinue,
	NodeBreak,
	
	NodeSwitch(Box<Node>, Vec<Node>),
	NodeCaseDefault,
	NodeCaseSingle(uint),
	NodeCaseRange(uint, uint),
	
	NodeFcnCall(Box<Node>, Vec<Node>),
	
	NodeReturn(Option<Box<Node>>),
	
	NodeAssign(Box<Node>, Box<Node>),
	NodeAssignOp(BinOp, Box<Node>, Box<Node>),
	
	NodeCast(::types::TypeRef,Box<Node>),
	
	NodeTernary(Box<Node>,Box<Node>,Box<Node>),
	NodeUniOp(UniOp, Box<Node>),
	NodeBinOp(BinOp, Box<Node>, Box<Node>),
	
	NodeIndex(Box<Node>, Box<Node>),
	NodeDerefMember(Box<Node>, String),
	NodeMember(Box<Node>, String),
}

#[deriving(Show)]
pub enum BinOp
{
	BinOpLogicAnd,
	BinOpLogicOr,
	
	BinOpBitAnd,
	BinOpBitOr,
	BinOpBitXor,
	
	BinOpShiftLeft,
	BinOpShiftRight,
	
	BinOpCmpEqu,
	BinOpCmpNEqu,
	BinOpCmpLt,
	BinOpCmpLtE,
	BinOpCmpGt,
	BinOpCmpGtE,
	
	BinOpAdd,
	BinOpSub,
	
	BinOpMul,
	BinOpDiv,
	BinOpMod,
}

#[deriving(Show)]
pub enum UniOp
{
	UniOpNeg,
	UniOpBitNot,
	UniOpLogicNot,
	UniOpPreInc,
	UniOpPreDec,
	UniOpPostInc,
	UniOpPostDec,
	UniOpAddress,
	UniOpDeref,
}

impl Node
{
	pub fn literal_integer(&self) -> Option<u64>
	{
		match self
		{
		&NodeInteger(v) => Some(v),
		&NodeUniOp(ref op,ref a) => match (op,a.literal_integer())
			{
			(&UniOpNeg,Some(a)) => Some(-a),
			_ => None,
			},
		&NodeBinOp(ref op,ref a,ref b) => match (op,a.literal_integer(), b.literal_integer())
			{
			(&BinOpSub,Some(a),Some(b)) => Some(a-b),
			_ => None,
			},
		_ => None,
		}
	}
}

// vim: ft=rust
