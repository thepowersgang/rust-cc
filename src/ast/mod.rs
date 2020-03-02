/*!
 * Representation of the C source in a tree
 */
use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub mod pretty_print;

pub type Ident = String;

#[derive(Debug)]
pub struct Span;

#[derive(Default)]
/// Representation of a C program/compilation unit
pub struct Program
{
	/// Item definition order
	item_order: Vec<ItemRef>,

	typedefs: HashMap<Ident, ::types::TypeRef>,
	structs: HashMap<Ident, ::types::StructRef>,
	unions: HashMap<Ident, ::types::UnionRef>,
	enums: HashMap<Ident, ::types::EnumRef>,
	// Aka global variables/functions
	symbols: HashMap<Ident, Symbol>,
}
/// Referece to a defined item (typedef/struct/value/...)
enum ItemRef
{
	ValueDecl(Ident),
	Value(Ident),

	Typedef(Ident),
	Struct(Ident),
	Union(Ident),
	Enum(Ident),

	//CppDefine {
	//	name: String,
	//	args: Vec<String>,
	//	tokens: Vec<Token>,
	//},
	//CppInclude(String),
}

// TODO: Have a disinction between functions and globals?
pub struct Symbol
{
	// TODO: Storage classes?
	name: Ident,
	pub symtype: ::types::TypeRef,
	pub value: Option<SymbolValue>,
}
#[derive(Debug)]
pub enum SymbolValue
{
	Value(::std::cell::RefCell<Initialiser>),
	Code(::std::cell::RefCell<FunctionBody>),
}
#[derive(Debug)]
pub struct FunctionBody
{
	pub code: Block,
	pub var_table: Vec<VarTableEnt>,
}
#[derive(Debug)]
pub struct VarTableEnt
{
	pub span: Span,
	pub name: Ident,
	pub ty: crate::types::TypeRef,
}

impl Program
{
	pub fn new() -> Program
	{
		Program {
			..::std::default::Default::default()
		}
	}
	
	pub fn define_function(&mut self, typeid: ::types::TypeRef, name: Ident, value: Option<Block>)
	{
		self.define_symbol(typeid, name, value.map(|v| SymbolValue::Code(::std::cell::RefCell::new(FunctionBody { code: v, var_table: Vec::new() }))))
	}
	pub fn define_variable(&mut self, typeid: ::types::TypeRef, name: Ident, value: Option<Initialiser>)
	{
		self.define_symbol(typeid, name, value.map(std::cell::RefCell::new).map(SymbolValue::Value))
	}
	fn define_symbol(&mut self, typeid: ::types::TypeRef, name: Ident, value: Option<SymbolValue>)
	{
		if value.is_some() {
			self.item_order.push(ItemRef::Value(name.clone()));
		}
		else {
			self.item_order.push(ItemRef::ValueDecl(name.clone()));
		}
		info!("Define variable '{}': '{:?}' = {:?}", name, typeid, value);
		match self.symbols.entry(name.clone())
		{
		Entry::Occupied(mut e) => {
			if e.get().symtype != typeid {
				// TODO: Don't check argument names (they should be separate?)
				// ERROR: Conflicting declarations
				panic!("TODO: Conflicting definitions of {} - {:?} != {:?}", name, e.get().symtype, typeid);
			}
			else if e.get().value.is_some() {
				if value.is_some() {
					// ERROR: Duplicated definition
					panic!("TODO: Re-definition of {} - {:?} and {:?}", name, e.get().value, value);
				}
				else {
					// WARN: Trailing declaration
				}
			}
			else {
				e.get_mut().symtype = typeid;
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
	
	pub fn set_typedef(&mut self, name: Ident, typeid: ::types::TypeRef) -> bool
	{
		self.item_order.push(ItemRef::Typedef(name.clone()));
		self.typedefs.insert(name, typeid).is_none()
	}
	pub fn get_typedef(&self, name: &str) -> Option<::types::TypeRef>
	{
		// TODO: Custom types
		// HACK! Define __builtin_va_list (a GCC internal) to be a magic type
		if name == "__builtin_va_list" {
			return Some( ::types::Type::new_ref_bare(::types::BaseType::MagicType(::types::MagicType::VaList)) );
		}
		
		//if name.len() > 2 && &name[name.len()-2 .. ] == "_t" {
		//	return Some(::types::Type::new_ref_bare( ::types::BaseType::MagicType(::types::MagicType::Named(name.to_owned())) ))
		//}
		
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

	pub fn make_struct(&mut self, name: &str, items: ::types::StructBody) -> Result<::types::StructRef,()> {
		if name != "" {
			self.item_order.push(ItemRef::Struct(name.to_owned()));
		}
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
	pub fn make_union(&mut self, name: &str, items: Vec<(::types::TypeRef,Ident)>) -> Result<::types::UnionRef,()> {
		if name != "" {
			self.item_order.push(ItemRef::Union(name.to_owned()));
		}
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
	pub fn make_enum(&mut self, name: &str, items: Vec<(u64,Ident)>) -> Result<::types::EnumRef,Option<Ident>> {
		if name != "" {
			self.item_order.push(ItemRef::Enum(name.to_owned()));
		}
		let er = self.get_enum(name);
		let ispop = er.borrow().is_populated();
		
		if ispop {
			Err(None)
		}
		else {
			// Set items in enum
			er.borrow_mut().set_items(items);
			Ok( er )
		}
	}

	pub fn get_symbol(&self, name: &Ident) -> Option<&Symbol> {
		self.symbols.get(name)
	}
	pub fn find_enum_var(&self, name: &Ident) -> Option<(crate::types::EnumRef, usize)> {
		for (enm_name, enm) in &self.enums
		{
			if let Some(idx) = enm.borrow().find_var(name) {
				return Some( (enm.clone(), idx) );
			}
		}
		None
	}

	pub fn iter_functions(&self) -> impl Iterator<Item=(&Ident, &crate::types::TypeRef, &::std::cell::RefCell<FunctionBody>)> {
		self.item_order.iter()
			.filter_map(move |v| match v { ItemRef::Value(ref n) => Some(n), _ => None })
			.filter_map(move |name| {
				let s = &self.symbols[name];
				match s.value
				{
				Some(SymbolValue::Code(ref e)) => Some( (name, &s.symtype, e) ),
				_ => None,
				}
				})
	}
	pub fn iter_symbols(&self) -> impl Iterator<Item=(&Ident, &crate::types::TypeRef, &SymbolValue)> {
		self.item_order.iter()
			.filter_map(move |v| match v { ItemRef::Value(ref n) => Some(n), _ => None })
			.filter_map(move |name| {
				let s = &self.symbols[name];
				match s.value
				{
				Some(ref v) => Some( (name, &s.symtype, v,) ),
				None => {
					if false /*s.symtype.qualifiers.is_extern()*/ {
						None
					}
					else if let crate::types::BaseType::Function(..) = s.symtype.basetype {
						None
					}
					else {
						// Non-extern statics with no value should have `Initialiser::None`
						//static NONE_INIT: SymbolValue = SymbolValue::Value(Initialiser::None);
						//Some( (name, &s.symtype, &NONE_INIT,) )
						todo!("");
					}
					},
				}
				})
	}
}

pub type Block = StatementList;
pub type StatementList = Vec<Statement>;
pub type VarDefList = Vec<VariableDefinition>;
/// Block statement
#[derive(Debug)]
pub enum Statement
{
	Empty,
	// TODO: Store the base type (for later printing)
	VarDef(VarDefList),
	Expr(Node),

	Block(Block),
	IfStatement {
		cond: ExprOrDef,
		true_arm: StatementList,
		else_arm: Option<StatementList>
	},
	WhileLoop {
		cond: ExprOrDef,
		body: StatementList,
	},
	DoWhileLoop {
		body: StatementList,
		cond: Node,
	},
	ForLoop {
		init: Option<ExprOrDef>,
		cond: Option<Node>,
		inc: Option<Node>,
		body: StatementList,
	},

	Continue,
	Break,
	Return(Option<Node>),

	Switch(Node, StatementList),
	CaseDefault,
	CaseSingle(u64),
	CaseRange(u64, u64),

	Goto(Ident),
	Label(Ident),
}
#[derive(Debug)]
pub struct VariableDefinition
{
	pub ty: ::types::TypeRef,
	pub name: Ident,
	pub index: Option<usize>,
	pub value: Initialiser,
}
#[derive(Debug)]
pub enum Initialiser
{
	/// No initialisation
	None,
	/// Single value
	Value(Node),
	/// List literal `{ a, b, c }`
	ListLiteral(Vec<Node>),
	/// Array literal `{[0] = a, [1] = b, [2] = c}`
	ArrayLiteral(Vec<(Node,Node)>),	// 
	/// Struct literal `{.a = a, .b = b, .c = c}`
	StructLiteral(Vec<(Ident,Node)>),
}
/// Either a evaluatable expression, or a variable definition
#[derive(Debug)]
pub enum ExprOrDef
{
	Expr(Node),
	Definition(VarDefList),
}
/// Expression node
pub struct Node
{
	pub kind: NodeKind,
	pub meta: Option<NodeMeta>,
}
/// Custom Debug formatter that reduces noise
impl ::std::fmt::Debug for Node
{
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		self.kind.fmt(f)?;
		if let Some(ref v) = self.meta {
			f.write_str(" ")?;
			v.fmt(f)?;
		}
		Ok( () )
	}
}
#[derive(Debug)]
pub struct NodeMeta {
	/// Result type
	pub ty: ::types::TypeRef,
	/// Indicates that this node needs to be assignable
	pub is_lvalue: bool,
}
#[derive(Debug)]
pub enum NodeKind
{
	StmtList(Vec<Node>),	// Comma operator
	
	Identifier(Ident, Option<IdentRef>),
	String(String),
	Integer(u64, crate::types::IntClass),
	Float(f64, crate::types::FloatClass),

	// TODO: Specialise this for expression/literal calls?
	FcnCall(Box<Node>, Vec<Node>),
	
	Assign(Box<Node>, Box<Node>),
	AssignOp(BinOp, Box<Node>, Box<Node>),
	Intrinsic(String, Vec<::types::TypeRef>, Vec<Box<Node>>),
	
	ImplicitCast(::types::TypeRef, Box<Node>),
	Cast(::types::TypeRef,Box<Node>),
	SizeofType(::types::TypeRef),
	SizeofExpr(Box<Node>),
	
	Ternary(Box<Node>,Box<Node>,Box<Node>),
	UniOp(UniOp, Box<Node>),
	BinOp(BinOp, Box<Node>, Box<Node>),
	
	Index(Box<Node>, Box<Node>),
	DerefMember(Box<Node>, Ident),
	Member(Box<Node>, Ident),
}
#[derive(Debug)]
pub enum IdentRef
{
	Local(usize),
	StaticItem,
	Function,
	Enum(::types::EnumRef, usize),
}
// Lower precedence is weaker binding
#[derive(Debug,PartialOrd,PartialEq,Copy,Clone)]
#[repr(u8)]
pub enum NodePrecedence
{
	/// Parens are never applied
	Lowest,	// NOTE: has to be first
	CommaOperator,
	Assignment,
	Bitwise,
	Logic,	// TODO: Is this where it is?
	Ternary,
	Comparison,
	BitShift,
	MulDivMod,
	AddSub,
	Unary,	// TODO: Are there more in here?
	UnarySuffix,
	DeRef,
	MemberAccess,
	Value,
	/// Parens are always applied
	Highest,
}
impl NodePrecedence
{
	pub fn up(&self) -> Self
	{
		if *self < NodePrecedence::Highest {
			// SAFE: Repr u8, c-like, bounds enforced
			unsafe {
				::std::mem::transmute(*self as u8 + 1)
			}
		}
		else {
			*self
		}
	}
	pub fn down(&self) -> Self
	{
		if *self > NodePrecedence::Lowest {
			// SAFE: Repr u8, c-like, bounds enforced
			unsafe {
				::std::mem::transmute(*self as u8 - 1)
			}
		}
		else {
			*self
		}
	}
}

#[derive(Debug,Copy,Clone)]
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

#[derive(Debug,Copy,Clone)]
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
	pub fn new(kind: NodeKind) -> Node
	{
		Node {
			kind: kind,
			meta: None,
			}
	}
	/// Attempt to interpret the node as a trivally constant integer
	pub fn literal_integer(&self) -> Option<u64>
	{
		match self.kind
		{
		NodeKind::Integer(v, _ty) => Some(v),
		NodeKind::UniOp(ref op,ref a) => match (op,a.literal_integer())
			{
			(&UniOp::Neg,Some(a)) => Some(!a + 1),
			_ => None,
			},
		NodeKind::BinOp(ref op,ref a,ref b) => match (op,a.literal_integer(), b.literal_integer())
			{
			(&BinOp::Add,Some(a),Some(b)) => Some(a+b),
			(&BinOp::Sub,Some(a),Some(b)) => Some(a-b),
			_ => None,
			},
		NodeKind::Identifier(..) => None,	// TODO: Look up ident in the global/constant scope
		_ => None,
		}
	}

	pub fn get_precedence(&self) -> NodePrecedence
	{
		match self.kind
		{
		NodeKind::StmtList(_) => NodePrecedence::CommaOperator,

		NodeKind::Identifier(..)
		| NodeKind::String(_)
		| NodeKind::Integer(..)
		| NodeKind::Float(..)
			=> NodePrecedence::Value,

		NodeKind::FcnCall(_, _) => NodePrecedence::MemberAccess,

		NodeKind::Assign(_, _)
		| NodeKind::AssignOp(_, _, _)
			=> NodePrecedence::Assignment,

		NodeKind::ImplicitCast(_, ref i) => i.get_precedence(),
		NodeKind::Cast(_, _) => NodePrecedence::Unary,	// TODO: Double-check
		NodeKind::SizeofType(_) => NodePrecedence::Value,
		NodeKind::SizeofExpr(_) => NodePrecedence::Value,
		NodeKind::Intrinsic(..) => NodePrecedence::Value,

		NodeKind::Ternary(_,_,_) => NodePrecedence::Ternary,
		NodeKind::UniOp(ref op, _) => match *op
			{
			UniOp::Neg => NodePrecedence::Unary,
			UniOp::BitNot   => NodePrecedence::Unary,
			UniOp::LogicNot => NodePrecedence::Unary,
			UniOp::PreInc
			| UniOp::PreDec
				=> NodePrecedence::Unary,
			UniOp::PostInc
			| UniOp::PostDec
				=> NodePrecedence::UnarySuffix,
			UniOp::Address
			| UniOp::Deref
				=> NodePrecedence::DeRef,
			},
		NodeKind::BinOp(ref op, _, _) => match *op
			{
			BinOp::LogicAnd
			| BinOp::LogicOr
				=> NodePrecedence::Logic,

			BinOp::BitAnd
			| BinOp::BitOr
			| BinOp::BitXor
				=> NodePrecedence::Bitwise,

			BinOp::ShiftLeft
			| BinOp::ShiftRight
				=> NodePrecedence::BitShift,

			BinOp::CmpEqu
			| BinOp::CmpNEqu
			| BinOp::CmpLt
			| BinOp::CmpLtE
			| BinOp::CmpGt
			| BinOp::CmpGtE
				=> NodePrecedence::Comparison,

			BinOp::Add
			| BinOp::Sub
				=> NodePrecedence::AddSub,

			BinOp::Mul
			| BinOp::Div
			| BinOp::Mod
				=> NodePrecedence::MulDivMod,
			},

		NodeKind::Index(_, _) => NodePrecedence::MemberAccess,
		NodeKind::DerefMember(_, _) => NodePrecedence::MemberAccess,
		NodeKind::Member(_, _) => NodePrecedence::MemberAccess,
		}
	}
}

// vim: ft=rust
