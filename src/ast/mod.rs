/*!
 * Representation of the C source in a tree
 */
use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub mod pretty_print;

pub type Ident = String;

#[derive(Debug,Clone)]
pub struct Span {
	pub layers: ::std::rc::Rc<Vec<String>>,
}
impl Span {
	fn message(&self, ty: &str, msg: ::std::fmt::Arguments<'_>) {
		let mut it = self.layers.iter().rev();
		if let Some(l) = it.next() {
			eprintln!("{}: {}: {}", l, ty, msg);
			for v in it {
				eprintln!("{}: from here", v);
			}
		}
		else {
			eprintln!("???: {}: {}", ty, msg);
		}
	}
	pub fn warning(&self, msg: ::std::fmt::Arguments<'_>) {
		self.message("warning", msg);
	}
	pub fn error(&self, msg: ::std::fmt::Arguments<'_>) -> ! {
		self.message("compile error", msg);
		::std::process::exit(1);
	}
	//pub fn bug(&self, msg: ::std::fmt::Arguments<'_>) -> ! {
	//	self.message("bug", msg);
	//	panic!("bug hit");
	//}
	#[track_caller]
	pub fn todo(&self, msg: ::std::fmt::Arguments<'_>) -> ! {
		self.message("TODO", msg);
		panic!("TODO hit");
	}
	pub fn layers(&self) -> &[String] {
		&self.layers
	}
}

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

	anon_enum_count: usize,
	empty_symbol: Option<crate::ast::SymbolValue>,
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
	#[allow(dead_code)]
	vis: Visibility,
	pub symtype: ::types::TypeRef,
	pub value: Option<SymbolValue>,
}
#[derive(Debug)]
pub enum SymbolValue
{
	Value(::std::cell::RefCell<Option<Initialiser>>),
	Code(::std::cell::RefCell<FunctionBody>),
}
#[derive(Debug)]
pub struct FunctionBody
{
	#[allow(dead_code)]	// Neither backend supports inline flags
	pub inline: bool,
	pub code: Block,
	pub var_table: Vec<VarTableEnt>,
}
#[derive(Debug)]
pub struct VarTableEnt
{
	//pub span: Span,
	pub name: Ident,
	pub ty: crate::types::TypeRef,
}
#[derive(PartialEq,Copy,Clone)]
pub enum Visibility
{
	Auto,
	Static,
	Extern,
}

impl Program
{
	pub fn new() -> Program
	{
		Program {
			empty_symbol: Some(crate::ast::SymbolValue::Value(::std::cell::RefCell::new(None))),
			..::std::default::Default::default()
		}
	}
	
	pub fn define_function(&mut self, span: Span, is_inline: bool, storage_class: Option<::types::StorageClass>, typeid: ::types::TypeRef, name: Ident, value: Option<Block>)
	{
		let vis = match storage_class
			{
			None => Visibility::Auto,
			Some(crate::types::StorageClass::Extern) => {
				if value.is_some() {
					// TODO: Warning?
				}
				Visibility::Extern
			},
			Some(crate::types::StorageClass::Static) => Visibility::Static,
			//Some(crate::types::StorageClass::Inline) => Visibility::Extern,
			Some(crate::types::StorageClass::Register)
			|Some(crate::types::StorageClass::Auto) => panic!("TODO: Error message"),
			};
		let value = match value
			{
			None => None,
			Some(v) => Some(SymbolValue::Code(::std::cell::RefCell::new(FunctionBody {
					inline: is_inline,
					code: v,
					var_table: Vec::new()
				}))),
			};
		self.define_symbol(span, vis, typeid, name, value)
	}
	pub fn define_variable(&mut self, span: Span, is_inline: bool, storage_class: Option<::types::StorageClass>, typeid: ::types::TypeRef, name: Ident, value: Option<Initialiser>)
	{
		if let crate::types::BaseType::Function(_) = typeid.basetype {
			if value.is_none() {
				return self.define_function(span, is_inline, storage_class, typeid, name, None);
			}
		}
		if is_inline {
			// TODO: Warn/error?
		}
		let vis = match storage_class
			{
			None => Visibility::Auto,
			Some(crate::types::StorageClass::Extern) => {
				if value.is_some() {
					// TODO: Warning?
				}
				Visibility::Extern
			},
			Some(crate::types::StorageClass::Static) => Visibility::Static,
			Some(crate::types::StorageClass::Register) => Visibility::Auto,
			//Some(crate::types::StorageClass::Inline)|
			Some(crate::types::StorageClass::Auto) => todo!("Invalid storage class on global?"),
			};
		let value = match value
			{
			Some(v) => Some(SymbolValue::Value(::std::cell::RefCell::new(Some(v)))),
			None => None,
			};
		self.define_symbol(span, vis, typeid, name, value)
	}
	fn define_symbol(&mut self, span: Span, vis: Visibility, typeid: ::types::TypeRef, name: Ident, value: Option<SymbolValue>)
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
				span.error(format_args!("Conflicting definitions of `{}` - {:?} != {:?}", name, e.get().symtype, typeid));
			}
			else {
				// Update the visibility to the most recently seen visibility
				match &mut e.get_mut().vis {
				Visibility::Auto => {},	// Error if different?
				Visibility::Static => {},	// Error if different?
				v @ Visibility::Extern => *v = vis,
				}
				match e.get().value {
				None => {
					e.get_mut().symtype = typeid;
					e.get_mut().value = value;
					},
				Some(SymbolValue::Value(ref v)) if v.borrow().is_none() => {
					// Was forward declared with no initialiser
					e.get_mut().value = value;
					},
				Some(_) => 
					match value
					{
					None => {
						// WARN: Trailing declaration
						},
					Some(SymbolValue::Value(ref v)) if v.borrow().is_none() => {
						// WARN: Also trailing declaration
						},
					Some(_) => {
						// ERROR: Duplicated definition
						span.error(format_args!("Re-definition of `{}` - {:?} and {:?}", name, e.get().value, value));
						}
					},
				}
			}
			},
		Entry::Vacant(e) => {
			e.insert(Symbol {
				name,
				vis,
				symtype: typeid,
				value,
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
		let name = if name == "" {
				let name = format!("enum#{}", self.anon_enum_count);
				self.anon_enum_count += 1;
				name
			}
			else {
				name.to_string()
			};
		match self.enums.entry(name.clone())
		{
		Entry::Occupied(e) => e.get().clone(),
		Entry::Vacant(e) => {
			let r = ::types::Enum::new_ref(e.key());
			e.insert(r).clone()
			}
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
	pub fn make_enum(&mut self, name: &str, items: Vec<(i64,Ident)>) -> Result<::types::EnumRef,Option<Ident>> {
		let er = self.get_enum(name);
		if er.borrow().name != "" {
			self.item_order.push(ItemRef::Enum(er.borrow().name.clone()));
		}
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
		for (_enm_name, enm) in &self.enums
		{
			if let Some(idx) = enm.borrow().find_var(name) {
				return Some( (enm.clone(), idx) );
			}
		}
		None
	}

	pub fn iter_symbols(&self) -> impl Iterator<Item=(&Ident, &crate::types::TypeRef, &SymbolValue)> {
		self.iter_symbols_with_prototypes().filter_map(|v| match v
			{
			(_vis, n, t, Some(v)) => Some( (n,t,v,) ),
			(_v, _n, _t, None) => None,
			}
			)
	}
	pub fn iter_symbols_with_prototypes(&self) -> impl Iterator<Item=(Visibility, &Ident, &crate::types::TypeRef, Option<&SymbolValue>)> {
		// Any items defined with `TypeName foo;` can be both forward declarations and default/zero definitions
		let values_for_uninit = {
			let mut uninit_values: Vec<(&Ident, bool)> = Vec::new();
			for v in self.item_order.iter()
			{
				match v
				{
				ItemRef::ValueDecl(ref name) => {
					let s = &self.symbols[name];
					match s.symtype.basetype {
					crate::types::BaseType::Function(_) => {},
					_ => {
						if (s.vis == Visibility::Auto || s.vis == Visibility::Static) && s.value.is_none() {
							if uninit_values.iter().any(|v| v.0 == name ) {
							}
							else {
								uninit_values.push((name, true));
							}
						}
					}
					}
				},
				_ => {},
				}
			}
			uninit_values.into_iter().map(move |(name, emit)| {
				if emit {
					println!("FORWARD {}", name);
					let s = &self.symbols[name];
					(s.vis, name, &s.symtype, self.empty_symbol.as_ref(),)
				}
				else {
					todo!()
				}
			})
		};
		self.item_order.iter()
			.filter_map(move |v| {
				let (name, is_fwd) = match v
					{
					ItemRef::Value(ref n) => (n, false),
					ItemRef::ValueDecl(ref n) => (n, true),
					_ => return None,
					};
				let s = &self.symbols[name];
				match s.value
				{
				Some(ref v) if !is_fwd => Some( (s.vis, name, &s.symtype, Some(v),) ),
				_ => Some( (s.vis, name, &s.symtype, None,) ),
				}
				})
			.chain(values_for_uninit)
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
	pub span: Span,
	pub ty: ::types::TypeRef,
	pub name: Ident,
	pub index: Option<usize>,
	pub value: Option<Initialiser>,
}
#[derive(Debug)]
pub enum Initialiser
{
	/// Single value
	Value(Node),
	/// List literal `{ a, b, c }`
	ListLiteral(Vec<Initialiser>),
	/// Array literal `{[0] = a, [1] = b, [2] = c}`
	ArrayLiteral(Vec<(Node,Initialiser)>),	// 
	/// Struct literal `{.a = a, .b = b, .c = c}`
	StructLiteral(Vec<(Ident,Initialiser)>),
}
/// Either a evaluatable expression, or a variable definition
#[derive(Debug)]
pub enum ExprOrDef
{
	Expr(Node),
	Definition(VarDefList),
}
impl ExprOrDef {
	pub fn res_ty(&self) -> Option<&crate::types::TypeRef> {
		match self {
		ExprOrDef::Expr(n) => n.meta.as_ref().map(|v| &v.ty),
		ExprOrDef::Definition(d) => Some(&d.last()?.ty),
		}
	}
}
/// Expression node
pub struct Node
{
	pub span: Span,
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
	StaticItem/*(*const Symbol)*/,
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

#[derive(Debug)]
pub enum ConstVal
{
	None,
	Integer(u64),
	Float(f64),
	Address(Ident, usize),
	String(String),
}

impl Node
{
	pub fn new(span: Span, kind: NodeKind) -> Node
	{
		Node {
			span,
			kind,
			meta: None,
			}
	}
	pub fn const_eval_opt(&self) -> ConstVal
	{
		self.const_eval(false)
	}
	pub fn const_eval_req(&self) -> ConstVal
	{
		self.const_eval(true)
	}
	fn const_eval_address(&self) -> Option<(String, usize)> {
		match self.kind
		{
		NodeKind::Identifier(ref name, _) => Some( (name.clone(), 0) ),
		NodeKind::Member(ref inner, ref name) => {
			let (rv_name,rv_ofs) = inner.const_eval_address()?;
			let ty = &inner.meta.as_ref().unwrap().ty;
			match ty.basetype
			{
			crate::types::BaseType::Struct(ref s) =>
				match s.borrow().iter_fields().find(|v| v.1 == name)
				{
				Some( (ofs, _, _, None) ) => Some( (rv_name, rv_ofs + ofs as usize) ),
				Some( (_, _, _, Some(_)) ) => self.span.error(format_args!("Getting address of bitfield")),
				None => self.span.error(format_args!("Unknown struct entry: {} in {:?}", name, ty)),
				},
			_ => self.span.todo(format_args!("Struct literal {:?}", ty)),
			}
			},
		_ => None,
		}
	}
	fn const_eval(&self, required: bool) -> ConstVal
	{
		match match self.kind
			{
			NodeKind::String(ref s) => ConstVal::String(s.clone()),
			NodeKind::Integer(v, _ty) => ConstVal::Integer(v),
			NodeKind::Float(v, _ty) => ConstVal::Float(v),
			NodeKind::UniOp(UniOp::Address,ref a) => {
				match a.const_eval_address()
				{
				Some( (name, ofs) ) => ConstVal::Address(name, ofs),
				None => ConstVal::None,
				}
				},
			NodeKind::UniOp(ref op,ref a) => match (op,a.const_eval(false))
				{
				(&UniOp::Neg,ConstVal::Integer(a)) => ConstVal::Integer(!a + 1),
				(&UniOp::Neg,ConstVal::Float(a)) => ConstVal::Float(-a),
				(&UniOp::BitNot,ConstVal::Integer(a)) => ConstVal::Integer(!a),
				_ => ConstVal::None,
				},
			NodeKind::BinOp(ref op,ref a,ref b) => match (op,a.const_eval(required), b.const_eval(required))
				{
				(&BinOp::Add,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a+b),
				(&BinOp::Sub,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a-b),
				(&BinOp::Div,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a/b),
				(&BinOp::Mod,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a%b),
				(&BinOp::Mul,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a*b),
				(&BinOp::BitOr,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a|b),
				(&BinOp::BitAnd,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a&b),
				(&BinOp::ShiftLeft,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a<<b),
				(&BinOp::ShiftRight,ConstVal::Integer(a),ConstVal::Integer(b)) => ConstVal::Integer(a<<b),

				(&BinOp::Add,ConstVal::Float(a),ConstVal::Float(b)) => ConstVal::Float(a+b),
				(&BinOp::Sub,ConstVal::Float(a),ConstVal::Float(b)) => ConstVal::Float(a-b),
				(&BinOp::Div,ConstVal::Float(a),ConstVal::Float(b)) => ConstVal::Float(a/b),
				(&BinOp::Mul,ConstVal::Float(a),ConstVal::Float(b)) => ConstVal::Float(a*b),

				_ => ConstVal::None,
				},
			NodeKind::Identifier(ref name, ref binding) =>
				match binding
				{
				&Some(IdentRef::Enum(ref e, idx)) => ConstVal::Integer( e.borrow().get_item_val(idx).unwrap() as u64 ),
				&Some(IdentRef::Function) => ConstVal::Address(name.clone(), 0),
				_ => if let Some(crate::types::BaseType::Array(_, _)) = self.meta.as_ref().map(|m| &m.ty.basetype) {
						ConstVal::Address(name.clone(), 0)
					}
					else {
						ConstVal::None
					},
				},
			NodeKind::ImplicitCast(ref ty, ref val)|NodeKind::Cast(ref ty, ref val) =>
				match val.const_eval(false)
				{
				v @ ConstVal::None => v,
				v @ ConstVal::Address(..) => match ty.basetype {
					crate::types::BaseType::Pointer(..) => v,
					crate::types::BaseType::Integer(ref ic) if ic.size_align().0 >= 4 => v,
					_ => ConstVal::None,
					},
				ConstVal::Integer(i) => match ty.basetype {
					crate::types::BaseType::Pointer(..) if i == 0 => ConstVal::Integer(i),
					crate::types::BaseType::Integer(_) => ConstVal::Integer(i),
					_ => ConstVal::None,
					},
				v @ ConstVal::String(_) => match ty.basetype {
					crate::types::BaseType::Pointer(ref inner)
						if matches!(inner.basetype, crate::types::BaseType::Integer(crate::types::IntClass::Char(None)))
						=> v,
					_ => todo!("const_eval: {:?} from {:?} - v={:?}", ty, self, v),
					},
				v @ _ => todo!("const_eval: {:?} from {:?} - v={:?}", ty, self, v),
				},
			NodeKind::SizeofType(ref ty) => ConstVal::Integer(ty.get_size().expect("") as u64),
			NodeKind::SizeofExpr(ref e) => ConstVal::Integer(e.meta.as_ref().unwrap().ty.get_size().expect("") as u64),
			_ => ConstVal::None,
			}
		{
		ConstVal::None if required => self.span.todo(format_args!("consteval {:#?}", self)),
		rv => rv,
		}
	}
	/// Attempt to interpret the node as a trivally constant integer
	pub fn literal_integer(&self) -> Option<u64>
	{
		match self.const_eval_opt()
		{
		ConstVal::None => None,
		ConstVal::Integer(v) => Some(v),
		_ => todo!("literal_integer: {:?}", self),
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
