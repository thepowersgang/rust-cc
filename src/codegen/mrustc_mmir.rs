//! Codegen backend that emits mrustc's MMIR format
use ::std::collections::HashMap;
use ::std::io::Write;

use crate::ast::Ident;
use crate::types::BaseType;

pub struct Context
{
	types: Vec<crate::types::TypeRef>,
	declared_functions: Vec<(crate::ast::Ident, crate::types::FunctionType)>,
	//defined_functions: Vec<crate::ast::Ident>,

	output_buffer: Vec<u8>,
}
impl Context
{
	pub fn new() -> Self
	{
		Context {
			types: Default::default(),
			declared_functions: Default::default(),
			//defined_functions: Default::default(),
			output_buffer: Vec::new(),
			}
	}
	pub fn finish(mut self, mut sink: impl ::std::io::Write) -> Result<(), Box<dyn std::error::Error>>
	{
		write!(self.output_buffer, "fn main#(isize, *const *const i8): i32 {{\n").unwrap();
		write!(self.output_buffer, "0: {{ CALL RETURN = main(arg0, arg1) goto 1 else 1 }}\n1: {{ RETURN }}\n").unwrap();
		write!(self.output_buffer, "}}\n").unwrap();
		sink.write_all(&self.output_buffer).map_err(|e| e.into())
	}

	pub fn declare_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType)
	{
		self.declared_functions.push((name.clone(), ty.clone()));
		self.register_functiontype(ty);
		write!(self.output_buffer, "{} = \"{}\":\"\";\n", self.fmt_function_ty(ty, Some(name)), name).unwrap();
	}
	pub fn declare_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef)
	{
		self.register_type(ty);
		//write!(self.output_buffer, "static {}: {};\n", name, self.fmt_type(ty)).unwrap();
		let _ = name;
	}
	pub fn lower_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		self.register_functiontype(ty);

		// Define variables
		let mut builder = Builder::new(self);
		for (i,(var_ty,var_name)) in ty.args.iter().enumerate()
		{
			let name = match 0
				{
				1 => format!("{}_a{}", var_name, i),	// Would like this
				2 => format!("{}", var_name),	// But this is what `fmt_function_ty` would emit
				_ => format!("arg{}", i),	// And this is what `standalone_miri` expects
				};
			// HACK: Handle anon arrays
			let name = if let BaseType::Array(_, crate::types::ArraySize::None) = var_ty.basetype {
					format!("(*{})", name)
				}
				else {
					name
				};
			builder.vars.push( Variable { lvalue: name, ty: builder.parent.fmt_type(var_ty).to_string(), } );
		}
		for (i,var) in body.var_table.iter().skip( ty.args.len() ).enumerate()
		{
			builder.vars.push( Variable { lvalue: format!("{}_v{}", var.name, i), ty: builder.parent.fmt_type(&var.ty).to_string(), } );
		}
		builder.handle_block(&body.code);
		if let BaseType::Void = ty.ret.basetype {
			builder.push_term("RETURN".to_owned());
		}

		let vars = builder.vars;
		let blocks = builder.blocks;

		// Dump code!
		write!(self.output_buffer, "{}\n", self.fmt_function_ty(ty, Some(name))).unwrap();
		write!(self.output_buffer, "{{\n").unwrap();

		for (i,v) in vars.into_iter().enumerate() {
			if i < ty.args.len() {
				continue;
			}
			write!(self.output_buffer, "\tlet {}: {};\n", v.lvalue, v.ty).unwrap();
		}
		for (i,(stmts,term)) in blocks.into_iter().enumerate() {
			write!(self.output_buffer, "\t{}: {{\n", i).unwrap();
			for stmt in stmts {
				if !stmt.ends_with('/') {
					write!(self.output_buffer, "\t\t{};\n", stmt).unwrap();
				}
				else {
					write!(self.output_buffer, "\t\t{}\n", stmt).unwrap();
				}
			}
			write!(self.output_buffer, "\t\t{}\n", term.unwrap_or_else(|| "INCOMPLETE".to_owned())).unwrap();
			write!(self.output_buffer, "\t}}\n").unwrap();
		}

		write!(self.output_buffer, "}}\n").unwrap();
	}
	pub fn lower_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef, val: Option<&crate::ast::Initialiser>)
	{
		self.register_type(ty);
		write!(self.output_buffer, "static {}: {} = ", name, self.fmt_type(ty)).unwrap();

		let size = ty.get_size().expect("lower_value with unsized/undefined type") as usize;
		enum Reloc {
			String(String),
			Addr(String),
		}
		fn generate_init(base_ofs: usize, buf: &mut [u8], relocs: &mut Vec<(usize,Reloc)>, ty: &crate::types::TypeRef, val: &crate::ast::Initialiser)
		{
			match val
			{
			crate::ast::Initialiser::Value(v) => {
				let v = v.const_eval_req();
				match v
				{
				crate::ast::ConstVal::None => todo!("Is None possible here?"),
				crate::ast::ConstVal::Integer(val) => {
					let s = ty.get_size().unwrap() as usize;
					buf[..s].copy_from_slice(&val.to_le_bytes()[..s])
					},
				crate::ast::ConstVal::Float(_) => todo!("float"),
				crate::ast::ConstVal::Address(s,ofs) => {
					buf[..4].copy_from_slice( &(0x10 + ofs as u32).to_le_bytes() );
					relocs.push( (base_ofs, Reloc::Addr(s)) );
					},
				crate::ast::ConstVal::String(s) => {
					buf[1] = 0x10;
					relocs.push( (base_ofs, Reloc::String(s)) );
					},
				}
				},
			crate::ast::Initialiser::ListLiteral(ref ents) => {
				for (i, e) in ents.iter().enumerate()
				{
					let (ofs, ty) = match ty.basetype
						{
						BaseType::Array(ref inner, _) => (i*inner.get_size().unwrap() as usize, inner.clone()),
						BaseType::Struct(ref s) =>
							match s.borrow().get_field_idx(i)
							{
							Some( (ofs, _, ty) ) => (ofs as usize, ty.clone()),
							None => panic!("Too many initialisers for struct"),
							},
						_ => todo!("List literal {:?}", ty),
						};
					generate_init(base_ofs+ofs, &mut buf[ofs..], &mut *relocs, &ty, e);
				}
				},
			crate::ast::Initialiser::ArrayLiteral(ref ents) => {
				for (idx, e) in ents.iter()
				{
					let i = match idx.const_eval_req()
						{
						crate::ast::ConstVal::None => todo!(),
						crate::ast::ConstVal::Integer(v) => v as usize,
						_ => panic!("Invalid consteval for array literal index")
						};
					let (ofs, ty) = match ty.basetype
						{
						BaseType::Array(ref inner, _) => (i*inner.get_size().unwrap() as usize, inner.clone()),
						_ => todo!("List literal {:?}", ty),
						};
					generate_init(base_ofs+ofs, &mut buf[ofs..], &mut *relocs, &ty, e);
				}
				},
			crate::ast::Initialiser::StructLiteral(ref ents) => {
				for (name, e) in ents.iter()
				{
					let (ofs,ty) = match ty.basetype
						{
						BaseType::Struct(ref s) =>
							match s.borrow().iter_fields().find(|v| v.1 == name)
							{
							Some( (ofs, _, ty) ) => (ofs as usize, ty.clone()),
							None => panic!("Unknown struct entry: {} in {:?}", name, ty),
							},
						_ => todo!("Struct literal {:?}", ty),
						};
					generate_init(base_ofs+ofs, &mut buf[ofs..], &mut *relocs, &ty, e);
				}
				},
			}
		}
		fn fmt_bytes(dst: &mut Vec<u8>, src: &[u8]) {
			write!(dst, "\"").unwrap();
			for &b in src {
				match b {
				0 => write!(dst, "\\0").unwrap(),
				b'\\'|b'"' => write!(dst, "\\{}", b as char).unwrap(),
				0x20 ..= 0x7E => write!(dst, "{}", b as char).unwrap(),
				_ => write!(dst, "\\x{:02x}", b).unwrap(),
				}
			}
			write!(dst, "\"").unwrap();
		}
		let mut buf = vec![0u8; size];
		let mut relocs = vec![];
		if let Some(val) = val {
			generate_init(0, &mut buf, &mut relocs, ty, val);
		}
		fmt_bytes(&mut self.output_buffer, &buf);
		if relocs.len() > 0
		{
			write!(self.output_buffer, "{{").unwrap();
			for (ofs,r) in relocs {
				write!(self.output_buffer, " @{}+{}=", ofs, crate::types::POINTER_SIZE).unwrap();
				match r {
				Reloc::String(s) => fmt_bytes(&mut self.output_buffer, &s.as_bytes()),
				Reloc::Addr(a) => write!(self.output_buffer, "{}", a).unwrap(),
				}
				write!(self.output_buffer, ",").unwrap();
			}
			write!(self.output_buffer, " }}").unwrap();
		}
		write!(self.output_buffer, ";\n").unwrap();
/*
		Initialiser::ListLiteral(ref vals) =>
			match ty.basetype
			{
			BaseType::Array(ref inner_ty, _) => {
				let inner_size = inner_ty.get_size().unwrap() as usize;
				for (ofs, val) in Iterator::zip( (0 .. ).map(|i| i * inner_size), vals.iter() )
				{
					self.init_data_ctx_node(data_ctx, data, offset + ofs, inner_ty, val);
				}
				},
			BaseType::Struct(ref s) => {
				for (val, (ofs, _name, inner_ty)) in Iterator::zip( vals.iter(), s.borrow().iter_fields() )
				{
					self.init_data_ctx_node(data_ctx, data, offset + ofs as usize, inner_ty, val);
				}
				},
			_ => todo!("init_data_ctx: ListLiteral with {:?}", ty),
			},
		Initialiser::Value(ref val) => self.init_data_ctx_node(data_ctx, data, offset, ty, val),
		_ => todo!("init_data_ctx: init={:?}", init),
		}
		*/
	}
}

impl Context
{
	fn fmt_function_ty(&self, fcn_ty: &crate::types::FunctionType, name: Option<&crate::ast::Ident>) -> String {
		use ::std::fmt::Write;
		let mut rv = String::new();
		//rv += "extern \"C\" ";
		rv += "fn";
		if let Some(name) = name {
			write!(rv, " {}", name).unwrap();
		}
		rv += "(";
		for (arg_ty, arg_name) in &fcn_ty.args {
			if false && name.is_some() {
				if arg_name == "" {
					write!(&mut rv, "_: ").unwrap();
				}
				else {
					write!(&mut rv, "{}: ", arg_name).unwrap();
				}
			}
			write!(&mut rv, "{}, ", self.fmt_type(arg_ty)).unwrap();
		}
		if fcn_ty.is_variadic {
			rv += "...";
		}
		rv += ") -> ";
		write!(&mut rv, "{}", self.fmt_type(&fcn_ty.ret)).unwrap();
		return rv;
	}
	fn fmt_type(&self, ty: &crate::types::TypeRef) -> impl ::std::fmt::Display {
		// Ignore qualifiers, they only matter for pointers
		use crate::types::{FloatClass,IntClass};
		use crate::types::Signedness;
		use crate::types::ArraySize;
		match &ty.basetype
		{
		BaseType::Void => "()".to_owned(),
		BaseType::Bool => "bool".to_owned(),
		BaseType::Struct(sr) => {
			let name = sr.borrow().name.clone();
			if name == "" {
				format!("struct_{:p}", sr)
			}
			else {
				name
			}
			},
		BaseType::Enum(_enm) => {
			// TODO: Get the min/max range to determine which type to use
			format!("i32")
		},
		BaseType::Union(_) => {
			todo!("Union types");
			},
		BaseType::Float(fc) => match fc
			{
			FloatClass::Float => "f32".to_owned(),
			FloatClass::Double => "f64".to_owned(),
			FloatClass::LongDouble => "f64".to_owned(),
			}
		BaseType::Integer(ic) => {
			let s = match ic
				{
				IntClass::Char(None) => Signedness::Signed,
				IntClass::Char(Some(s)) => *s,
				IntClass::Short(s) => *s,
				IntClass::Int(s) => *s,
				IntClass::Long(s) => *s,
				IntClass::LongLong(s) => *s,
				};
			let size = ty.get_size().unwrap();
			format!("{}{}", match s { Signedness::Signed => "i", Signedness::Unsigned => "u" }, size*8)
			}
		BaseType::MagicType(mt) => match mt
			{
			crate::types::MagicType::VaList => todo!("va_list"),
			crate::types::MagicType::Named(name, crate::types::MagicTypeRepr::VoidPointer) => format!("MAGIC_V_{}#", name),
			crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { signed, bits }) =>
				format!("{}{}", if *signed { "i" } else { "u" }, bits),
			crate::types::MagicType::Named(name, crate::types::MagicTypeRepr::Opaque { .. }) =>
				format!("MAGIC_O_{}#", name),
			},
		BaseType::Pointer(inner) => {
			format!("*{} {}", if inner.qualifiers.is_const() { "const" } else { "mut" }, self.fmt_type(inner))
			},
		BaseType::Array(inner, size) => {
			match size {
			ArraySize::None => format!("*mut {}", self.fmt_type(inner)),
			ArraySize::Fixed(v) => format!("[{}; {}]", self.fmt_type(inner), v),
			ArraySize::Expr(e) => match e.get_value_opt()
				{
				Ok(v) => format!("[{}; {}]", self.fmt_type(inner), v),
				Err(_) => format!("*mut {}", self.fmt_type(inner)),
				},
			}
			},
		BaseType::Function(ft) => self.fmt_function_ty(ft, None),
		}
		
	}

	fn register_type(&mut self, ty: &crate::types::TypeRef) {
		for t in self.types.iter() {
			if t == ty {
				return ;
			}
		}
		self.types.push(ty.clone());
		match &ty.basetype
		{
		BaseType::Void => {},
		BaseType::Bool => {},
		BaseType::Struct(structref) => {
			if structref.borrow().is_populated()
			{
				for (_ofs,_name,ty) in structref.borrow().iter_fields()
				{
					self.register_type(ty)
				}
				write!(self.output_buffer, "type {} {{\n", self.fmt_type(ty)).unwrap();
				let (size,align) = ty.get_size_align().unwrap_or((0,0) );
				write!(self.output_buffer, "\tSIZE {}, ALIGN {};\n", size,align).unwrap();
				for (ofs,name,ty) in structref.borrow().iter_fields()
				{
					write!(self.output_buffer, "\t{} = {}; // {}\n", ofs, self.fmt_type(ty), name).unwrap();
					//write!(self.output_buffer, "\t{} = {};\n", ofs, self.fmt_type(ty)).unwrap();
				}
				write!(self.output_buffer, "}}\n").unwrap();
			}
			},
		BaseType::Enum(_) => {},	// Nothing needed for enums, they're not rust enums
		BaseType::Union(_) => todo!("union"),
		BaseType::Float(_) => {},
		BaseType::Integer(_) => {},
		BaseType::MagicType(crate::types::MagicType::VaList) => {},
		BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::VoidPointer)) => {},
		BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => {},
		BaseType::MagicType(crate::types::MagicType::Named(_name, crate::types::MagicTypeRepr::Opaque { bytes })) => {
			write!(self.output_buffer, "type {} {{\n", self.fmt_type(ty)).unwrap();
			let (s,a) = ty.get_size_align().unwrap();
			assert!(s == *bytes as u32);
			write!(self.output_buffer, "\tSIZE {}, ALIGN {};\n", s,a).unwrap();
			write!(self.output_buffer, "\t0 = [u8; {}];\n", bytes).unwrap();
			write!(self.output_buffer, "}}\n").unwrap();
			},
		BaseType::Pointer(inner) => self.register_type(inner),
		BaseType::Array(inner, _size) => self.register_type(inner),
		BaseType::Function(ft) => self.register_functiontype(ft),
		}
	}
	fn register_functiontype(&mut self, ty: &crate::types::FunctionType) {
		self.register_type(&ty.ret);
		for arg in &ty.args {
			self.register_type(&arg.0);
		}
	}
}

type BbIdx = usize;
struct Builder<'a>
{
	parent: &'a mut Context,
	vars: Vec<Variable>,
	stack: Vec<Scope>,

	cur_block: usize,
	blocks: Vec< (Vec<String>,Option<String>,)>,

	// -- `goto` label support --
	/// Defined labels
	labels: HashMap<Ident, BbIdx>,
	/// Labels that are not yet defined
	missed_labels: HashMap<Ident, BbIdx>,
}
struct Variable {
	lvalue: String,
	ty: String,
}
struct Scope {
	blk_break: Option<BbIdx>,
	blk_continue: Option<BbIdx>,
	switch: Option<SwitchScope>,
}
impl Scope {
	fn new() -> Scope {
		Scope {
			blk_break: None,
			blk_continue: None,
			switch: None,
			}
	}
	fn new_switch(blk_break: BbIdx) -> Self {
		Scope {
			blk_break: Some(blk_break),
			blk_continue: None,
			switch: Some(Default::default()),
			}
	}
	fn new_loop(blk_break: BbIdx, blk_continue: BbIdx) -> Self {
		Scope {
			blk_break: Some(blk_break),
			blk_continue: Some(blk_continue),
			switch: None,
			}
	}
}
#[derive(Default)]
struct SwitchScope
{
	case_default: Option<BbIdx>,
	case_labels: Vec<(u64, BbIdx)>,
}

#[derive(Debug,Clone)]
enum ValueRef {
	// LValue
	Slot(String),
	// RValue (with type)
	Value(String, String),
	// A function name (makes calls nicer)
	Function(crate::ast::Ident, String),
}
impl ValueRef {
	fn unwrap_slot(self) -> String {
		match self {
		ValueRef::Slot(v) => v,
		_ => panic!(""),
		}
	}
}
impl<'a> Builder<'a>
{
	fn indent(&self) -> impl ::std::fmt::Display {
		struct Indent(usize);
		impl ::std::fmt::Display for Indent {
			fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
				for _ in 0 .. self.0 {
					f.write_str(" ")?;
				}
				Ok( () )
			}
		}
		Indent(self.stack.len())
	}
	fn new(parent: &'a mut Context) -> Self {
		Self {
			parent,
			vars: Vec::new(),
			stack: Default::default(),
			cur_block: 0,
			blocks: vec![ Default::default() ],
			labels: Default::default(),
			missed_labels: Default::default(),
		}
	}
}

impl Builder<'_>
{
	fn create_block(&mut self) -> usize {
		let rv = self.blocks.len();
		self.blocks.push(Default::default());
		rv
	}
	fn set_block(&mut self, new_block: usize) {
		assert!(new_block < self.blocks.len(), "Block index out of range");
		assert!( self.blocks[new_block].1.is_none(), "Setting to a closed block" );
		self.cur_block = new_block;
	}
	fn push_term(&mut self, t: String) {
		assert!( self.blocks[self.cur_block].1.is_none(), "Pushing to a closed block" );
		self.blocks[self.cur_block].1 = Some(t);
	}
	fn push_term_goto(&mut self, dst: usize) {
		self.push_term(format!("GOTO {}", dst))
	}
	fn push_term_if(&mut self, val: String, bb_true: usize, bb_false: usize) {
		self.push_term(format!("IF {} goto {} else {}", val, bb_true, bb_false))
	}

	fn push_comment(&mut self, t: ::std::fmt::Arguments<'_>) {
		self.blocks[self.cur_block].0.push(format!("/* {} */", t));
	}
	fn push_stmt(&mut self, t: String) {
		assert!( self.blocks[self.cur_block].1.is_none(), "Pushing to a closed block" );
		self.blocks[self.cur_block].0.push(t);
	}
	fn push_stmt_assign(&mut self, dst: String, src: ValueRef) {
		match src
		{
		ValueRef::Slot(src) => self.push_stmt(format!("ASSIGN {} = ={}", dst, src)),
		ValueRef::Value(src, _) => self.push_stmt(format!("ASSIGN {} = {}", dst, src)),
		ValueRef::Function(name, _) => self.push_stmt(format!("ASSIGN {} = ADDR {}", dst, name)),
		}
	}

	fn alloc_local_raw(&mut self, ty: String) -> String {
		let idx = self.vars.len();
		let rv = format!("temp_{}", idx);
		self.vars.push(Variable { lvalue: rv.clone(), ty });
		rv
	}
	fn alloc_local(&mut self, ty: &crate::types::TypeRef) -> String {
		self.alloc_local_raw(self.parent.fmt_type(ty).to_string())
	}

	/// Get a stack slot/lvalue (e.g. for a param)
	fn get_value(&mut self, vr: ValueRef) -> String {
		match vr
		{
		ValueRef::Slot(rv) => rv,
		ValueRef::Value(val, ty) => {
			let local = self.alloc_local_raw(ty);
			self.push_stmt_assign(local.clone(), ValueRef::Value(val, Default::default()));
			local
			},
		ValueRef::Function(name, ty) => {
			let local = self.alloc_local_raw(ty);
			self.push_stmt_assign(local.clone(), ValueRef::Value(format!("ADDROF {}", name), Default::default()));
			local
			}
		}
	}
	fn orphaned_block(&mut self, debug_label: &str) {
		let blk_orphan = self.create_block();
		trace!("++{:?} (orphan from {})", blk_orphan, debug_label);
		self.set_block(blk_orphan);
	}
}

impl Builder<'_>
{

	fn handle_expr_def(&mut self, node: &crate::ast::ExprOrDef) -> ValueRef
	{
		use crate::ast::ExprOrDef;
		match node
		{
		ExprOrDef::Expr(ref e) => self.handle_node(e),
		ExprOrDef::Definition(ref list) => {
			for var_def in list
			{
				self.define_var(var_def);
			}
			ValueRef::Slot(self.vars[list.last().unwrap().index.unwrap()].lvalue.clone())
			},
		}
	}

	fn define_var(&mut self, var_def: &crate::ast::VariableDefinition)
	{
		// TODO: If the type is an array with a variable-length, then insert an alloca
		if let BaseType::Array(inner, size) = &var_def.ty.basetype {
			match size {
			crate::types::ArraySize::None => todo!("Error for unsized array local?"),
			crate::types::ArraySize::Fixed(_) => {},
			crate::types::ArraySize::Expr(e) => match e.get_value_opt()
				{
				Ok(_) => {},
				Err(e) => {
					let idx = var_def.index.unwrap();

					let size = self.handle_node(e);
					let size = self.get_value(size);
					let next_block = self.create_block();
					self.push_term(format!("CALL {} = \"alloca_array\"<{}>({}) goto {} else {}",
						self.vars[idx].lvalue, self.parent.fmt_type(inner), size, next_block, next_block));
					self.set_block(next_block);
					},
				}
			}
		}
		match var_def.value
		{
		None => {},
		Some(ref init) => {
			let idx = var_def.index.unwrap();
			self.handle_init(&var_def.ty, self.vars[idx].lvalue.clone(), init);
			},
		}
	}
	fn handle_init(&mut self, ty: &crate::types::TypeRef, slot: String, init: &crate::ast::Initialiser)
	{
		match init
		{
		crate::ast::Initialiser::Value(ref node) => {
			let v = self.handle_node(node);
			self.push_stmt_assign(slot, v);
			},
		crate::ast::Initialiser::ListLiteral(ref ents) => {
			let ents_it = ents.iter().map(Some).chain(::std::iter::repeat(None));
			match &ty.basetype
			{
			BaseType::Array(inner, count) => {
				let count = count.get_value();
				for (idx, val) in (0..count).zip(ents_it)
				{
					if let Some(i) = val {
						self.handle_init(inner, format!("{}.{}", slot, idx), i);
					}
					else {
						self.handle_init_zero(inner, format!("{}.{}", slot, idx));
					}
				}
			},
			BaseType::Struct(str) => {
				for (idx,((_,_,fty),val)) in str.borrow().iter_fields().zip(ents_it).enumerate()
				{
					if let Some(i) = val {
						self.handle_init(fty, format!("{}.{}", slot, idx), i);
					}
					else {
						self.handle_init_zero(fty, format!("{}.{}", slot, idx));
					}
				}
				},
			_ => todo!("{} = {:?}: {:?}", slot, init, ty),
			}
			},
		_ => todo!("{} = {:?}", slot, init),
		}
	}
	fn handle_init_zero(&mut self, ty: &crate::types::TypeRef, slot: String)
	{
		// Zero initialise a field
		let bb_next = self.create_block();
		self.push_term(format!("CALL {} = \"zeroed\"<{}>() goto {} else {}", slot, self.parent.fmt_type(ty), bb_next, bb_next));
		self.set_block(bb_next);
	}

	fn handle_block(&mut self, stmts: &crate::ast::StatementList)
	{
		trace!("{}>>", self.indent());
		self.stack.push(Scope::new());
		for stmt in stmts {
			self.handle_stmt(stmt);
		}
		self.stack.pop();
		trace!("{}<<", self.indent());
	}
	fn handle_stmt(&mut self, stmt: &crate::ast::Statement)
	{
		use crate::ast::Statement;
		match *stmt
		{
		Statement::Empty => {},
		Statement::VarDef(ref list) => {
			trace!("{}{:?}", self.indent(), stmt);
			for var_def in list
			{
				self.define_var(var_def);
			}
			},
		Statement::Expr(ref e) => {
			trace!("{}{:?}", self.indent(), stmt);
			self.push_comment(format_args!("{:?}", e.span));
			let _v = self.handle_node(e);
			},
		Statement::Block(ref stmts) => {
			self.handle_block(stmts);
			},
		Statement::IfStatement { ref cond, ref true_arm, ref else_arm } => {
			trace!("{}if {:?}", self.indent(), cond);
			let cond_v = self.handle_expr_def(cond);
			let cond_v = self.get_value(cond_v);

			let true_blk = self.create_block(); trace!("++{:?} true", true_blk);
			let else_blk = self.create_block(); trace!("++{:?} false", else_blk);
			let done_blk = self.create_block(); trace!("++{:?} done", done_blk);
			self.push_term_if(cond_v, true_blk, else_blk);

			self.set_block(true_blk);
			self.handle_block(true_arm);
			self.push_term_goto(done_blk);

			self.set_block(else_blk);
			if let Some(else_arm) = else_arm
			{
				self.handle_block(else_arm);
			}
			self.push_term_goto(done_blk);

			self.set_block(done_blk);
			},

		Statement::WhileLoop { ref cond, ref body } => {
			trace!("{}while {:?}", self.indent(), cond);
			let blk_top = self.create_block(); trace!("++{:?}", blk_top);
			let blk_body = self.create_block(); trace!("++{:?}", blk_body);
			let blk_exit = self.create_block(); trace!("++{:?}", blk_exit);
			self.push_term_goto(blk_top);

			self.set_block(blk_top);

			self.stack.push(Scope::new_loop(blk_top, blk_exit));
			let cond_v = self.handle_expr_def(cond);
			let cond_v = self.get_value(cond_v);
			self.push_term_if(cond_v, blk_body, blk_exit);

			self.set_block(blk_body);
			self.stack.push(Scope::new());
			self.handle_block(body);
			self.stack.pop();	// Body scope

			self.push_term_goto(blk_top);
			self.stack.pop();	// Loop scope

			self.set_block(blk_exit);
			},

		Statement::DoWhileLoop { ref body, ref cond } => {
			let blk_body = self.create_block(); trace!("++{:?}", blk_body);
			let blk_foot = self.create_block(); trace!("++{:?}", blk_foot);	// target of continue
			let blk_exit = self.create_block(); trace!("++{:?}", blk_exit);	// target of break
			self.push_term_goto(blk_body);

			self.stack.push(Scope::new_loop(blk_foot, blk_exit));
			self.set_block(blk_body);
			self.stack.push(Scope::new());
			self.handle_block(body);
			self.stack.pop();	// Body scope
			
			self.push_term_goto(blk_foot);
			self.set_block(blk_foot);
			self.stack.pop();	// Loop scope

			{
				let cond_v = self.handle_node(cond);
				let cond_v = self.get_value(cond_v);
				self.push_term_if(cond_v, blk_body, blk_exit);
			}

			self.set_block(blk_exit);
			},
		Statement::ForLoop { ref init, ref cond, ref inc, ref body } => {
			if let Some(init) = init {
				self.handle_expr_def(init);
			}

			let blk_top  = self.create_block(); trace!("++{:?} (for top)", blk_top);	// loop back
			let blk_body = self.create_block(); trace!("++{:?} (for body)", blk_body);
			let blk_foot = self.create_block(); trace!("++{:?} (for foot)", blk_foot);	// target of continue
			let blk_exit = self.create_block(); trace!("++{:?} (for exit)", blk_exit);	// target of break
			self.push_term_goto(blk_top);

			self.set_block(blk_top);

			if let Some(cond) = cond {
				let cond_v = self.handle_node(cond);
				let cond_v = self.get_value(cond_v);
				self.push_term_if(cond_v, blk_body, blk_exit);
			}
			else {
				self.push_term_goto(blk_body);
			}

			self.stack.push(Scope::new_loop(blk_foot, blk_exit));
			self.set_block(blk_body);
			self.stack.push(Scope::new());
			self.handle_block(body);
			self.stack.pop();	// Body scope

			self.push_term_goto(blk_foot);
			self.set_block(blk_foot);
			self.stack.pop();	// Loop scope

			if let Some(inc) = inc {
				self.handle_node(inc);
			}

			self.push_term_goto(blk_top);

			self.set_block(blk_exit);
			},

		Statement::Continue => {
			trace!("{}continue", self.indent());
			for e in self.stack.iter().rev()
			{
				if let Some(blk) = e.blk_continue {
					self.push_term_goto(blk);
					self.orphaned_block("continue");
					return ;
				}
			}
			panic!("Continue without a loop");
			},
		Statement::Break => {
			trace!("{}break", self.indent());
			for e in self.stack.iter().rev()
			{
				if let Some(blk) = e.blk_break {
					self.push_term_goto(blk);
					self.orphaned_block("break");
					return ;
				}
			}
			panic!("Break without a loop");
			},
		Statement::Return(ref opt_val) => {
			trace!("{}return {:?}", self.indent(), opt_val);
			if let Some(val) = opt_val
			{
				let val = self.handle_node(val);
				self.push_stmt_assign("RETURN".to_string(), val);
				self.push_term("RETURN".to_string());
			}
			else
			{
				self.push_term("RETURN".to_string());
			}
			self.orphaned_block("return");
			},

		Statement::Switch(ref val, ref body) => {
			trace!("{}switch {:?}", self.indent(), val);
			let val = self.handle_node(val);
			let val = self.get_value(val);
			// - Make a block to contain the condition table (don't start it yet), and for the break target
			let blk_cond = self.create_block();
			self.push_term_goto(blk_cond);
			let blk_end = self.create_block();
			self.stack.push(Scope::new_switch(blk_end));

			// - Convert the body (first block should be an orphan, labels are pushed by the `case` statement items)
			let blk_body = self.create_block();
			self.set_block(blk_body);
			self.handle_block(body);
			let labels = self.stack.pop().and_then(|v| v.switch).expect("Didn't pop a switch scope");
			self.push_term_goto(blk_end);
			// - Generate switch table
			self.set_block(blk_cond);
			{
				use ::std::fmt::Write;
				let mut term = String::new();
				write!(term, "SWITCHVALUE {} {{", val).unwrap();
				for (v,b) in &labels.case_labels
				{
					write!(term, "{} = {}, ", v, b).unwrap();
				}
				write!(term, "_ = {}", labels.case_default.unwrap_or(blk_end)).unwrap();
				write!(term, "}}").unwrap();
				self.push_term(term);
			}
			// - Finalise
			self.set_block(blk_end);
			},
		Statement::CaseDefault => {
			trace!("{}default:", self.indent());
			let blk = {	// TODO: if there's chanined cases, be more efficient
				let blk = self.create_block(); trace!("++{:?} (default)", blk);
				self.push_term_goto(blk);
				self.set_block(blk);
				blk
				};
			for e in self.stack.iter_mut().rev()
			{
				if let Some(ref mut sw) = e.switch {
					assert!(sw.case_default.is_none());
					sw.case_default = Some(blk);
					return;
				}
			}
			panic!("TODO: Error for case outside a switch");
			},
		Statement::CaseSingle(v) => {
			trace!("{}case {}:", self.indent(), v);
			let blk = {	// TODO: if there's chanined cases, be more efficient
				let blk = self.create_block(); trace!("++{:?} (case)", blk);
				self.push_term_goto(blk);
				self.set_block(blk);
				blk
				};
			for e in self.stack.iter_mut().rev()
			{
				if let Some(ref mut sw) = e.switch {
					sw.case_labels.push( (v, blk,) );
					return;
				}
			}
			panic!("TODO: Error for case outside a switch");
			},
		Statement::CaseRange(s, e) => {
			todo!("CaseRange({} ..= {})", s, e);
			},

		Statement::Goto(ref label) => {
			trace!("{}goto {:?}", self.indent(), label);
			// If the label is already defined, then insert a jump to that block.
			let blk = if let Some(b) = self.labels.get(label) {
					*b
				}
				// Otherwise, create a block and store it for when the label is created
				else {
					let blk = self.create_block();
					trace!("++{:?} (label)", blk);
					self.missed_labels.insert(label.clone(), blk);
					blk
				};
			self.push_term_goto(blk);
			self.orphaned_block("goto");
			},
		Statement::Label(ref label) => {
			trace!("{}{:?}:", self.indent(), label);
			// Make a new block
			let blk = if let Some(blk) = self.missed_labels.remove(label) {
					blk
				}
				else {
					let blk = self.create_block();
					trace!("++{:?} (label)", blk);
					blk
				};
			debug!("{}{:?} = {:?}", self.indent(), label, blk);
			self.push_term_goto(blk);
			self.set_block(blk);
			// Add the label to a list of labels
			self.labels.insert(label.clone(), blk);
			},
		}
	}

	fn handle_node(&mut self, node: &crate::ast::Node) -> ValueRef
	{
		let res_ty = &node.meta.as_ref().unwrap().ty;
		use crate::ast::NodeKind;
		match node.kind
		{
		NodeKind::StmtList(ref nodes) => {
			let (last, nodes) = nodes.split_last().unwrap();
			for n in nodes {
				self.handle_node(n);
			}
			self.handle_node(last)
			},
		NodeKind::Identifier(ref name, ref binding) => {
			let ty = &node.meta.as_ref().unwrap().ty;
			match binding
			{
			None => panic!("No binding on `NodeKind::Identifier`"),
			Some(crate::ast::IdentRef::Local(idx)) => ValueRef::Slot(self.vars[*idx].lvalue.clone()),
			Some(crate::ast::IdentRef::StaticItem) => ValueRef::Slot(format!("{}", name)),
			Some(crate::ast::IdentRef::Function) => ValueRef::Function(name.clone(), self.parent.fmt_type(ty).to_string()),
			Some(crate::ast::IdentRef::Enum(ref enm, idx)) => {
				let val = enm.borrow().get_item_val(*idx).expect("Enum index out of range?");
				let ty = crate::types::Type::new_ref_bare(BaseType::Integer(crate::types::IntClass::Int( crate::types::Signedness::Signed )));
				let ty = self.parent.fmt_type(&ty).to_string();
				ValueRef::Value(format!("{} {}", val, ty), ty)
				},
			}
			},
		NodeKind::Integer(val, ty) => {
			let ty = self.parent.fmt_type(&crate::types::Type::new_ref_bare(BaseType::Integer(ty)));
			ValueRef::Value(format!("{} {}", val, ty), ty.to_string())
			},
		NodeKind::Float(val, ty) => match ty.size()
			{
			4 => ValueRef::Value(format!("{:+} f32", val), "f32".to_owned()),
			8 => ValueRef::Value(format!("{:+} f64", val), "f64".to_owned()),
			sz => panic!("NodeKind::Float sz={:?}", sz),
			},
		NodeKind::String(ref val) => {
			ValueRef::Value(format!("{:?}", val.to_owned()+"\0"), "&str".to_owned())
			},

 		NodeKind::FcnCall(ref fcn, ref args) => {
			let ty = &node.meta.as_ref().unwrap().ty;
			let rv = self.alloc_local(ty);

			let fcn = self.handle_node(fcn);
			let (term,ret_block) = {
				use ::std::fmt::Write;
				let mut term = format!("CALL {} = ", rv);
				if let ValueRef::Function(ref name, _) = fcn {
					write!(term, "{}", name).unwrap();
				}
				else {
					write!(term, "({})", self.get_value(fcn)).unwrap();
				}
				term += "(";
				for v in args {
					let v = self.handle_node(v);
					write!(term, "{},", self.get_value(v)).unwrap();
				}
				let ret_block = self.create_block();
				write!(term, ") goto {} else {}", ret_block, 0).unwrap();
				(term, ret_block)
				};
			self.push_term(term);
			self.set_block(ret_block);
			ValueRef::Value(format!("={}", rv), self.parent.fmt_type(ty).to_string())
			},

		NodeKind::Assign(ref slot, ref val) => {
			let slot = match self.handle_node(slot)
				{
				ValueRef::Slot(s) => s,
				_ => node.span.error(format_args!("Assiging to a value")),
				};
			let val = self.handle_node(val);
			self.push_stmt_assign(slot.clone(), val);
			ValueRef::Slot(slot)
			},
		NodeKind::AssignOp(ref op, ref slot, ref val) => {
			let ty = &val.meta.as_ref().unwrap().ty;
			let slot = match self.handle_node(slot)
				{
				ValueRef::Slot(s) => s,
				_ => panic!("Assiging to a value"),
				};
			let val = self.handle_node(val);
			let val_r = self.get_value(val);
			let new_val = self.handle_binop(op, ty, slot.clone(), val_r);
			self.push_stmt_assign(slot.clone(), new_val);
			ValueRef::Slot(slot)
			},

		NodeKind::Cast(ref ty, ref val) => {
			let src_ty = &val.meta.as_ref().unwrap().ty;
			let val = self.handle_node(val);
			self.handle_cast(ty, val, src_ty, /*is_implicit=*/false)
			},
		NodeKind::ImplicitCast(ref ty, ref val) => {
			let src_ty = &val.meta.as_ref().unwrap().ty;
			let val = self.handle_node(val);
			self.handle_cast(ty, val, src_ty, /*is_implicit=*/true)
			},
		
		NodeKind::Ternary(ref cond, ref val_true, ref val_false) => {
			let is_lvalue = node.meta.as_ref().unwrap().is_lvalue;

			let cond_v = self.handle_node(cond);
			let cond_v = self.get_value(cond_v);
			let true_blk = self.create_block();	trace!("++{:?}", true_blk);
			let else_blk = self.create_block();	trace!("++{:?}", else_blk);
			let done_blk = self.create_block();	trace!("++{:?}", done_blk);
			self.push_term_if(cond_v, true_blk, else_blk);

			let dst = if is_lvalue {
					panic!("TODO: handle_node - Ternary (LValue)");
				}
				else {
					self.alloc_local(&node.meta.as_ref().unwrap().ty)
				};

			self.set_block(true_blk);
			let val_true = self.handle_node(val_true);
			if is_lvalue {
				panic!("TODO: handle_node - Ternary (LValue) - result true {:?}", val_true);
			}
			else {
				self.push_stmt_assign(dst.clone(), val_true);
			}
			self.push_term_goto(done_blk);

			self.set_block(else_blk);
			let val_false = self.handle_node(val_false);
			if is_lvalue {
				panic!("TODO: handle_node - Ternary (LValue) - result true {:?}", val_false);
			}
			else {
				self.push_stmt_assign(dst.clone(), val_false);
			};
			self.push_term_goto(done_blk);

			self.set_block(done_blk);

			// NOTE: Ternary an LValue. This needs to be handled
			if is_lvalue {
				panic!("TODO: handle_node - Ternary (LValue)");
			}
			else {
				ValueRef::Slot(dst)
			}
			},
		NodeKind::UniOp(ref op, ref val) => {
			let ty = &val.meta.as_ref().unwrap().ty;
			let val_in = self.handle_node(val);
			use crate::ast::UniOp;
			match op
			{
			UniOp::PostDec|UniOp::PostInc => {
				let rv = self.alloc_local(ty);
				// TODO: Pointers need different handling?
				self.push_stmt_assign(rv.clone(), val_in.clone());
				let ty = if let BaseType::Pointer(_) = ty.basetype { "usize".to_owned() } else { self.parent.fmt_type(ty).to_string() };
				let rvalue = format!("BINOP {} {} 1 {}", self.get_value(val_in.clone()), if let UniOp::PostDec = op { "-" } else { "+" }, ty);
				self.push_stmt_assign(val_in.clone().unwrap_slot(), ValueRef::Value(rvalue, ty.clone()));
				ValueRef::Value(format!("={}", rv), ty)
				},
			UniOp::PreDec|UniOp::PreInc => {
				// TODO: Pointers need different handling?
				let ty = if let BaseType::Pointer(_) = ty.basetype { "usize".to_owned() } else { self.parent.fmt_type(ty).to_string() };
				let rvalue = format!("BINOP {} {} 1 {}", self.get_value(val_in.clone()), if let UniOp::PreDec = op { "-" } else { "+" }, ty);
				self.push_stmt_assign(val_in.clone().unwrap_slot(), ValueRef::Value(rvalue, ty.clone()));
				val_in
				},
			UniOp::Deref => {
				let val = self.get_value(val_in);
				let _ity = match ty.basetype
					{
					BaseType::Pointer(ref ity) => ity.clone(),
					_ => panic!("Deref of bad type - {:?}", ty),
					};
				ValueRef::Slot(format!("(*{})", val))
				},
			UniOp::Address => match val_in
				{
				ValueRef::Value(_,_) => panic!("Taking address of temporary"),
				ValueRef::Slot(v) => ValueRef::Value(format!("& {}", v), self.parent.fmt_type(res_ty).to_string()),
				_ => todo!("handle_node - UniOp Address {:?}", val_in),
				},
			UniOp::Neg => {
				let ty = self.parent.fmt_type(ty).to_string();
				ValueRef::Value(format!("UNIOP - {}", self.get_value(val_in)), ty)
				},
			UniOp::BitNot => {
				match ty.basetype
				{
				//BaseType::Bool => ValueRef::Temporary(self.builder.ins().bnot(val)),
				BaseType::Integer(_)
				| BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. }))
					=> ValueRef::Value(format!("UNIOP ! {}", self.get_value(val_in)), self.parent.fmt_type(ty).to_string()),
				
				_ => todo!("BitNot on {:?}", ty),
				}
				},
			UniOp::LogicNot => {
				//let val = self.get_value(val_in.clone());
				match ty.basetype
				{
				BaseType::Bool => ValueRef::Value(format!("UNIOP ! {}", self.get_value(val_in)), "bool".to_owned()),
				BaseType::Integer(_)
				| BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. }))
					=> ValueRef::Value(format!("BINOP {} == 0 {}", self.get_value(val_in), self.parent.fmt_type(ty).to_string()), "bool".to_owned()),
				BaseType::Pointer(_) => {
					let v = ValueRef::Value("0 usize".into(), "usize".into());
					let t = ValueRef::Value(format!("CAST {} as {}", self.get_value(v), self.parent.fmt_type(ty)), self.parent.fmt_type(ty).to_string());
					// If equal to 0, return 1
					ValueRef::Value(format!("BINOP {} == {}", self.get_value(val_in), self.get_value(t)), "bool".to_owned())
					},
				_ => todo!("LogicNot on {:?}", ty),
				}
				},
			}
			},
		NodeKind::BinOp(ref op, ref val_l, ref val_r) => {
			let ty_l = &val_l.meta.as_ref().unwrap().ty;
			let val_l = self.handle_node(val_l);
			let val_r = self.handle_node(val_r);
			let val_l = self.get_value(val_l);
			let val_r = self.get_value(val_r);

			self.handle_binop(op, ty_l, val_l, val_r)
			},
			
		NodeKind::Index(..) => panic!("Unexpected Index op"),
		NodeKind::DerefMember(..) => panic!("Unexpected DerefMember op"),
		NodeKind::Member(ref val, ref name) => {
			let ty = &val.meta.as_ref().unwrap().ty;
			let val = self.handle_node(val);
			match ty.get_field(name)
			{
			Some((idx, _ofs, _ity)) => ValueRef::Slot(format!("{} .{}", self.get_value(val), idx)),
			None => panic!("No field {:?} on {:?}", name, ty),
			}
			},

		NodeKind::SizeofType(ref ty) => {
			let size = ty.get_size().expect("sizeof on opaque");
			let rty = self.parent.fmt_type(res_ty).to_string();
			ValueRef::Value(format!("{} {}", size, rty), rty)
			},
		NodeKind::SizeofExpr(ref node) => {
			let size = node.meta.as_ref().expect("No meta?").ty.get_size().expect("sizeof on opaque");
			let rty = self.parent.fmt_type(res_ty).to_string();
			ValueRef::Value(format!("{} {}", size, rty), rty)
			},
		NodeKind::Intrinsic(ref name, ref _types, ref values) => match &name[..]
			{
			"va_start" => {
				let list = self.handle_node(&values[0]);
				// TODO: Check the other argument (must be the last non `...` arg)
				let next_block = self.create_block();
				self.push_term(format!("CALL {} = \"va_start\"() goto {} else {}", list.unwrap_slot(), next_block, next_block));
				self.set_block(next_block);
				ValueRef::Value("()".to_owned(), "()".to_owned())
				},
			"va_copy" => {
				let dst = self.handle_node(&values[0]);
				let src = self.handle_node(&values[1]);
				let src = self.get_value(src);
				let next_block = self.create_block();
				self.push_term(format!("CALL {} = \"va_copy\"({}) goto {} else {}", dst.unwrap_slot(), src, next_block, next_block));
				self.set_block(next_block);
				ValueRef::Value("()".to_owned(), "()".to_owned())
				},
			"va_end" => {
				let list = self.handle_node(&values[0]);
				let next_block = self.create_block();
				let rv = self.alloc_local_raw("()".to_owned());
				self.push_term(format!("CALL {} = \"va_end\"({}) goto {} else {}", rv, list.unwrap_slot(), next_block, next_block));
				self.set_block(next_block);
				ValueRef::Slot(rv)
				},
			//"va_arg" => {
			//	let list = self.handle_node(&values[0]);
			//	let ty = cvt_ty(&types[0]);
			//	//todo!("handle_node - va_arg ty={:?} list={:?}", ty, list); 
			//	// TODO: This heavily depends on the specific ABI.
			//	match ty
			//	{
			//	cr_tys::I32 => ValueRef::Temporary(self.builder.ins().iconst(ty, 0 as i64)),
			//	cr_tys::I64 => ValueRef::Temporary(self.builder.ins().iconst(ty, 0 as i64)),
			//	_ => todo!("handle_node - va_arg ty={:?} list={:?}", ty, list),
			//	}
			//	},
			_ => node.span.todo(format_args!("TODO: handle_node - {:?}", node)),
			},
		}
	}

	fn handle_binop(&mut self, op: &crate::ast::BinOp, ty_l: &crate::types::TypeRef, val_l: String, val_r: String) -> ValueRef
	{
		if let BaseType::Pointer(ref inner) = ty_l.basetype {
			match op
			{
			BinOp::Add|BinOp::Sub => {
				let method = match op
					{
					BinOp::Add => "offset",
					BinOp::Sub => "ptr_diff",
					_ => panic!(""),
					};
				let dst = self.alloc_local(ty_l);
				let next_block = self.create_block();
				self.push_term(format!("CALL {} = \"{}\"<{}>({}, {}) goto {} else {}",
					dst, method, self.parent.fmt_type(inner), val_l, val_r, next_block, next_block));
				self.set_block(next_block);
				return ValueRef::Slot(dst);
				},
			_ => {},
			}
		}
		use crate::ast::BinOp;
		let ty_s = self.parent.fmt_type(ty_l).to_string();
		let (op,ty_s) = match op
			{
			BinOp::CmpLt   => ("<" ,"bool".to_owned()),
			BinOp::CmpGt   => (">" ,"bool".to_owned()),
			BinOp::CmpLtE  => ("<=","bool".to_owned()),
			BinOp::CmpGtE  => (">=","bool".to_owned()),
			BinOp::CmpEqu  => ("==","bool".to_owned()),
			BinOp::CmpNEqu => ("!=","bool".to_owned()),

			BinOp::Add => ("+",ty_s),
			BinOp::Sub => ("-",ty_s),
			BinOp::Mul => ("*",ty_s),
			BinOp::Div => ("/",ty_s),
			BinOp::Mod => ("%",ty_s),

			BinOp::ShiftLeft => ("<<",ty_s),
			BinOp::ShiftRight => (">>",ty_s),

			BinOp::BitAnd => ("&",ty_s),
			BinOp::BitOr => ("|",ty_s),
			BinOp::BitXor => ("^",ty_s),

			BinOp::LogicAnd|BinOp::LogicOr => {
				let dst = self.alloc_local_raw("bool".to_owned());
				let bb_end = self.create_block();
				let bb_check2 = self.create_block();
				let bb_true = self.create_block();
				let bb_false = self.create_block();

				if let BinOp::LogicAnd = op {
					self.push_term_if(val_l, bb_check2, bb_false);
					self.set_block(bb_check2);
					self.push_term_if(val_r, bb_true, bb_false);
				}
				else {
					self.push_term_if(val_l, bb_true, bb_check2);
					self.set_block(bb_check2);
					self.push_term_if(val_r, bb_true, bb_false);
				}

				self.set_block(bb_true);
				self.push_stmt_assign(dst.clone(), ValueRef::Value("true".to_owned(), "bool".to_owned()));
				self.push_term_goto(bb_end);
				self.set_block(bb_false);
				self.push_stmt_assign(dst.clone(), ValueRef::Value("false".to_owned(), "bool".to_owned()));
				self.push_term_goto(bb_end);
				self.set_block(bb_end);
				return ValueRef::Value(format!("={}", dst), "bool".to_owned())
				},
			//_ => panic!("TODO: handle_node - BinOp - {:?}", op),
			};	

		return ValueRef::Value(format!("BINOP {} {} {}", val_l, op, val_r), ty_s);
	}


	/// Common processing of cast operations (between `ImplicitCast` and `Cast`)
	fn handle_cast(&mut self, dst_ty: &crate::types::TypeRef, src_val: ValueRef, src_ty: &crate::types::TypeRef, is_implicit: bool) -> ValueRef
	{
		let cast_name = if is_implicit { "ImplicitCast" } else { "Cast" };
		if let BaseType::Void = dst_ty.basetype {
			let _ = self.get_value(src_val);
			ValueRef::Value("( )".to_owned(), "()".to_owned())
		}
		else if let BaseType::Bool = dst_ty.basetype {
			let v = self.get_value(src_val);
			match src_ty.basetype
			{
			BaseType::Integer(..)
			|BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. }))
				=> ValueRef::Value(format!("BINOP {} != 0 {}", v, self.parent.fmt_type(&src_ty)), "bool".into()),
			BaseType::Pointer(..) => {
				let src_ty_s = self.parent.fmt_type(src_ty).to_string();
				let zero = self.get_value(ValueRef::Value("0 usize".into(), "usize".into()));
				let zero = ValueRef::Value(format!("CAST {} as {}", zero, src_ty_s), src_ty_s);
				ValueRef::Value(format!("BINOP {} == {}", v, self.get_value(zero)), "bool".into())
				},
			_ => todo!("Cast {:?} to bool", src_ty),
			}
		}
		// Casting/decaying an array to a pointer
		else if let BaseType::Array(..) = src_ty.basetype {
			match dst_ty.basetype
			{
			BaseType::Pointer(..) => {},
			_ => panic!("Invalid {} from {:?} to {:?}", cast_name, src_ty, dst_ty),
			}
			let dst_ty_s = self.parent.fmt_type(dst_ty).to_string();
			let tmp_ty = crate::types::Type::new_ref(BaseType::Pointer(src_ty.clone()), crate::types::Qualifiers::new());
			let v = ValueRef::Value(format!("&{}", self.get_value(src_val)), self.parent.fmt_type(&tmp_ty).to_string());
			ValueRef::Value(format!("CAST {} as {}", self.get_value(v), dst_ty_s), dst_ty_s)
		}
		else if let BaseType::Pointer(_) = dst_ty.basetype {
			// Force integers to cast to usize first
			let src_val = if let BaseType::Integer(_) = src_ty.basetype {
					ValueRef::Value(format!("CAST {} as usize", self.get_value(src_val)), "usize".to_owned())
				}
				else {
					src_val
				};
			let dst_ty_s = self.parent.fmt_type(dst_ty).to_string();
			ValueRef::Value(format!("CAST {} as {}", self.get_value(src_val), dst_ty_s), dst_ty_s)
		}
		else {
			let dst_ty_s = self.parent.fmt_type(dst_ty).to_string();
			ValueRef::Value(format!("CAST {} as {}", self.get_value(src_val), dst_ty_s), dst_ty_s)
		}
	}
}
