
use super::ItemRef;

pub fn write(mut sink: impl ::std::io::Write, prog: &super::Program)
{
	PrettyPrinter { sink: &mut sink, prog: prog }.write_program();
}

struct PrettyPrinter<'a, 'b> {
	sink: &'a mut dyn (::std::io::Write),
	prog: &'b super::Program,
}

impl<'a, 'b> PrettyPrinter<'a, 'b>
{

	fn write_program(&mut self)
	{
		for item in &self.prog.item_order
		{
			match item
			{
			&ItemRef::ValueDecl(ref name) => {
				self.write_prototype(&self.prog.symbols[name]);
				},
			&ItemRef::Value(ref name) => {
				self.write_value(&self.prog.symbols[name]);
				},
			&ItemRef::Typedef(ref name) => {
				let td = &self.prog.typedefs[name];
				self.write_str("typedef "); self.write_type_d(td, &mut |s| s.write_str(name), false); self.write_str(";\n");
				},
			&ItemRef::Struct(ref name) => {
				self.write_struct_def(&self.prog.structs[name].borrow(), true);
				self.write_str(";\n");
				},
			_ => {},
			}
		}
	}

	fn find_typedef(&self, ty: &::types::TypeRef) -> Option<&'b str>
	{
		for (k,v) in &self.prog.typedefs {
			if v == ty || (ty.basetype == v.basetype && v.qualifiers.is_lesser_than(&ty.qualifiers)) {
				return Some(k)
			}
		}
		None
	}

	fn write_prototype(&mut self, sym: &::ast::Symbol)
	{
		self.write_type(&sym.symtype, |f| f.write_str(&sym.name));
		self.write_str(";\n");
	}
	fn write_value(&mut self, sym: &::ast::Symbol)
	{
		self.write_type(&sym.symtype, |f| f.write_str(&sym.name));
		match sym.value
		{
		Some(::ast::SymbolValue::Code(ref block)) => {
			self.write_str("\n");
			self.write_block(block, 0);
			self.write_str("\n");
			},
		Some(::ast::SymbolValue::Value(ref v)) => {
			self.write_initialiser(v);
			self.write_str(";\n");
			},
		None => panic!("Value with no value - {:?}", sym.name),
		}
	}


	fn write_struct_def(&mut self, s: &::types::Struct, use_newlines: bool)
	{
		let nl = if true || use_newlines { "\n" } else { " " };
		let indent = if true || use_newlines { "\t" } else { "" };
		self.write_str("struct "); self.write_str(&s.name); self.write_str(nl);
		if let Some(ref body) = s.items
		{
			self.write_str("{"); self.write_str(nl);
			for &(ref ty, ref name) in &body.fields
			{
				self.write_str(indent);
				self.write_type(ty, |s| s.write_str(name));
				self.write_str(";"); self.write_str(nl);
			}
			self.write_str("}");
			if body.attributes.gcc.len() > 0
			{
				self.write_str("__attribute__((");
				for a in &body.attributes.gcc {
					self.write_str(&a.0);
				}
				self.write_str("))");
			}
		}
	}

	fn write_initialiser(&mut self, init: &::ast::Initialiser)
	{
		match init
		{
		&::ast::Initialiser::None => {},
		&::ast::Initialiser::Value(ref v) => {
			self.write_str(" = ");
			self.write_node(v, super::NodePrecedence::CommaOperator.up());
			},
		&::ast::Initialiser::ListLiteral(ref vs) => {
			self.write_str(" = {");
			let mut is_first = true;
			for v in vs {
				if !is_first {
					self.write_str(", ");
				}
				else {
					self.write_str(" ");
				}
				is_first = false;
				self.write_node(v, super::NodePrecedence::CommaOperator.up());
			}
			self.write_str(" }");
			},
		&::ast::Initialiser::ArrayLiteral(_) => panic!("TODO: Pretty-print ArrayLiteral"),
		&::ast::Initialiser::StructLiteral(_) => panic!("TODO: Pretty-print StructLiteral"),
		}
	}


	//fn write_type(&mut self, ty: &::types::TypeRef, mut cb: impl FnOnce(&mut Self))
	fn write_type(&mut self, ty: &::types::TypeRef, mut cb: impl FnMut(&mut Self))
	{
		//self.write_type_d(ty, Box::new(cb));
		self.write_type_d(ty, &mut cb, true);
	}
	//fn write_type_d<'b>(&mut self, ty: &::types::TypeRef, cb: Box<::std::boxed::FnBox(&mut Self)+'b>)
	fn write_type_d(&mut self, ty: &::types::TypeRef, cb: &mut dyn FnMut(&mut Self), use_typedef: bool)
	{
		use ::types::BaseType;
		match ty.basetype
		{
		BaseType::Void => {
			self.write_type_qualifiers(&ty.qualifiers);
			self.write_str("void ");
			cb(self);
			},
		BaseType::Bool => {
			self.write_type_qualifiers(&ty.qualifiers);
			self.write_str("_Bool ");
			cb(self);
			},
		BaseType::Struct(ref r) => {
			if use_typedef {
				//if let Some(t) = self.find_typedef(ty) {
				//	write!(self, "{} ", t);
				//	cb(self);
				//}
			}
			self.write_type_qualifiers(&ty.qualifiers);
			if r.borrow().name != "" {
				write!(self, "struct {} ", r.borrow().name);
			}
			else {
				let print_body = if ! use_typedef {
						true
					}
					else if let Some(t) = self.find_typedef(ty) {
						write!(self, "{} ", t);
						false
					}
					else {
						true
					};
				if print_body
				{
					self.write_struct_def(&r.borrow(), false);
					self.write_str(" ");
				}
			}
			cb(self);
			},
		BaseType::Enum(ref r) => {
			self.write_type_qualifiers(&ty.qualifiers);
			if r.borrow().name != "" {
				write!(self, "enum {} ", r.borrow().name);
			}
			else {
				self.write_str("enum { ... } ");
			}
			cb(self);
			},
		BaseType::Union(ref r) => {
			self.write_type_qualifiers(&ty.qualifiers);
			if r.borrow().name != "" {
				write!(self, "union {} ", r.borrow().name);
			}
			else {
				self.write_str("union { ... } ");
			}
			cb(self);
			},
		BaseType::Integer(ref r) => {
			self.write_type_qualifiers(&ty.qualifiers);
			use ::types::IntClass;
			use ::types::Signedness::*;
			match r
			{
			IntClass::Bits(s,b) => write!(self, "{}int{}_t ", if s.is_unsigned() { "u" } else { "" }, b),
			IntClass::Char(s) => write!(self, "{}char ", match s { None => "", Some(Signed) => "signed ", Some(Unsigned) => "unsigned " }),
			IntClass::Short(s) => write!(self, "{}short ", if s.is_unsigned() { "unsigned " } else { "" }),
			IntClass::Int(s) => write!(self, "{}int ", if s.is_unsigned() { "unsigned " } else { "" }),
			IntClass::Long(s) => write!(self, "{}long ", if s.is_unsigned() { "unsigned " } else { "" }),
			IntClass::LongLong(s) => write!(self, "{}long long ", if s.is_unsigned() { "unsigned " } else { "" }),
			}
			cb(self);
			},
		BaseType::Float(ref r) => {
			self.write_type_qualifiers(&ty.qualifiers);
			use ::types::FloatClass;
			match r
			{
			FloatClass::Float => self.write_str("float "),
			FloatClass::Double => self.write_str("double "),
			FloatClass::LongDouble => self.write_str("long double "),
			}
			cb(self);
			},

		BaseType::MagicType(ref m) => {
			self.write_type_qualifiers(&ty.qualifiers);
			use ::types::MagicType;
			match *m
			{
			MagicType::Named(ref n, ref _repr) => { self.write_str(n); self.write_str(" "); },
			MagicType::VaList => self.write_str("va_list "),
			}
			cb(self);
			},

		BaseType::Pointer(ref ity) => {
			self.write_type(ity, |s| {
				s.write_str("*");
				s.write_type_qualifiers(&ty.qualifiers);
				(*cb)(s);
				});
			},
		BaseType::Array(ref ity, ref size) => {
			self.write_type(ity, |s| {
				cb(s);
				s.write_str("[");
				match size
				{
				&::types::ArraySize::None => {},
				&::types::ArraySize::Fixed(v) => { write!(s, "{}", v); },
				&::types::ArraySize::Expr(ref v) => { s.write_node(v, super::NodePrecedence::CommaOperator.up()); },
				}
				s.write_str("]");
				});
			},
		BaseType::Function(ref info) => {
			self.write_type(&info.ret, |_| {});
			(*cb)(self);
			self.write_str("(");
			let mut b = false;
			for (aty, aname) in &info.args {
				if b {
					self.write_str(", ");
				}
				b = true;
				if aname == "..." {
					self.write_str("...");
					break ;
				}
				self.write_type(aty, |f| f.write_str(aname));
			}
			self.write_str(")");
			self.write_type_qualifiers(&ty.qualifiers);
			},
		}
	}
	fn write_type_qualifiers(&mut self, qualifiers: &::types::Qualifiers)
	{
		if qualifiers.is_const() {
			self.write_str("const ");
		}
		if qualifiers.is_volatile() {
			self.write_str("volatile ");
		}
		if qualifiers.is_restrict() {
			self.write_str("restrict ");
		}
	}

	fn write_block(&mut self, block: &super::Block, indent: usize)
	{
		for _ in 0 .. indent {
			self.write_str("\t");
		}
		self.write_str("{\n");
		for sn in block
		{
			for _ in 0 .. indent {
				self.write_str("\t");
			}
			// Only indent again if the statement is NOT a label
			match sn
			{
			&super::Statement::Label(..) => { },
			&super::Statement::CaseDefault => { },
			&super::Statement::CaseSingle(..) => { },
			&super::Statement::CaseRange(..) => { },
			_ => self.write_str("\t"),
			}
			if self.write_stmt(sn, indent) {
				// No semicolon+newline needed
			}
			else {
				self.write_str(";\n");
			}
		}
		for _ in 0 .. indent {
			self.write_str("\t");
		}
		self.write_str("}\n");
	}
	fn write_stmt(&mut self, stmt: &super::Statement, indent: usize) -> bool
	{
		use super::Statement;
		match stmt
		{
		&Statement::Empty => { false },
		&Statement::VarDef(ref vd) => { self.write_vardef(vd); false },
		&Statement::Expr(ref e) => { self.write_node(e, super::NodePrecedence::Lowest); false },
		&Statement::Block(ref b) => { self.write_block(b, indent+1); true },
		&Statement::IfStatement { ref cond, ref true_arm, ref else_arm } => {
			self.write_str("if( ");
			self.write_defexpr(cond);
			self.write_str(" )\n");
			self.write_block(true_arm, indent+1);
			if let &Some(ref ea) = else_arm {
				for _ in 0 .. indent+1 {
					self.write_str("\t");
				}
				self.write_str("else\n");
				self.write_block(ea, indent+1);
			}
			true
			},
		&Statement::WhileLoop { ref cond, ref body } => {
			self.write_str("while( ");
			self.write_defexpr(cond);
			self.write_str(" )\n");
			self.write_block(body, indent+1);
			true
			},
		&Statement::DoWhileLoop { ref body, ref cond } => {
			self.write_str("do\n");
			self.write_block(body, indent+1);
			for _ in 0 .. indent+1 {
				self.write_str("\t");
			}
			self.write_str("while( ");
			self.write_node(cond, super::NodePrecedence::Lowest);
			self.write_str(" )");
			false
			},
		&Statement::ForLoop { ref init, ref cond, ref inc, ref body } => {
			self.write_str("for( ");
			if let Some(init) = init {
				self.write_defexpr(init);
			}
			self.write_str("; ");
			if let Some(cond) = cond {
				self.write_node(cond, super::NodePrecedence::Lowest);
			}
			self.write_str("; ");
			if let Some(inc) = inc {
				self.write_node(inc, super::NodePrecedence::Lowest);
			}
			self.write_str(" )\n");
			self.write_block(body, indent+1);
			true
			},
		&Statement::Continue => { self.write_str("continue"); false }
		&Statement::Break => { self.write_str("continue"); false }
		&Statement::Return(ref v) => {
			self.write_str("return");
			if let Some(ref e) = v {
				self.write_str(" ");
				self.write_node(e, super::NodePrecedence::Lowest);
			}
			false
			},
		&Statement::Goto(ref n) => { write!(self, "goto {}", n); false },

		&Statement::Switch(ref v, ref stmts) => {
			self.write_str("switch "); self.write_node(v, super::NodePrecedence::CommaOperator.up()); self.write_str("\n");
			self.write_block(stmts, indent+1);
			true
			},

		// NOTE: Labels have negative indentation (handled by caller)
		&Statement::Label(ref n) => { write!(self, "{}:\n", n); true },
		&Statement::CaseDefault => { self.write_str("default:\n"); true },
		&Statement::CaseSingle(v) => { write!(self, "case {}:\n", v); true },
		&Statement::CaseRange(v1, v2) => { write!(self, "case {} ... {}:\n", v1, v2); true },
		}
	}
	fn write_vardef(&mut self, defs: &super::VarDefList)
	{
		// TODO: Ideally this would re-create the original definition line (with commas)
		let mut first = true;
		for def in defs
		{
			if !first {
				// TODO: THIS IS WRONG. It'll break for multiple definitions in a for loop header
				self.write_str("; ");
			}
			self.write_type(&def.ty, |self_| self_.write_str(&def.name));
			self.write_initialiser(&def.value);	// TODO: Indent level?
			first = false;
		}
	}
	fn write_defexpr(&mut self, node: &super::ExprOrDef)
	{
		match node
		{
		&super::ExprOrDef::Expr(ref v) => self.write_node(v, super::NodePrecedence::CommaOperator.up()),
		&super::ExprOrDef::Definition(ref d) => self.write_vardef(d),
		}
	}
	fn write_node(&mut self, node: &super::Node, max_p: super::NodePrecedence)
	{
		let node_p = node.get_precedence();
		// TODO: Parenthesis around values if precence is lower than a specified minimum
		if node_p < max_p {
			self.write_str("(");
		}
		use super::Node;
		use super::{UniOp,BinOp};
		match node
		{
		&Node::StmtList(ref subnodes) => {
			self.write_node(&subnodes[0], node_p.down());
			for sn in &subnodes[1..] {
				self.write_str(", ");
				self.write_node(sn, node_p.down());
			}
			},

		&Node::Identifier(ref n) => self.write_str(n),
		&Node::String(ref s) => write!(self, "{:?}", s),
		&Node::Integer(v) => write!(self, "{}", v),
		&Node::Float(v) => write!(self, "{}", v),

		&Node::FcnCall(ref fcn, ref values) => {
			self.write_node(fcn, node_p);
			self.write_str("(");
			if values.len() > 0 {
				self.write_node(&values[0], super::NodePrecedence::CommaOperator.up());
				for sn in &values[1..] {
					self.write_str(", ");
					self.write_node(sn, super::NodePrecedence::CommaOperator.up());
				}
			}
			self.write_str(")");
			},

		&Node::Assign(ref dst, ref v) => {
			self.write_node(dst, node_p);
			self.write_str(" = ");
			self.write_node(v, node_p);
			},
		&Node::AssignOp(ref op, ref dst, ref v) => {
			self.write_node(dst, node_p);
			match op
			{
			BinOp::LogicAnd => self.write_str(" &&= "),
			BinOp::LogicOr  => self.write_str(" ||= "),

			BinOp::BitAnd => self.write_str(" &= "),
			BinOp::BitOr  => self.write_str(" |= "),
			BinOp::BitXor => self.write_str(" ^= "),

			BinOp::ShiftLeft  => self.write_str(" <<= "),
			BinOp::ShiftRight => self.write_str(" >>= "),

			BinOp::CmpEqu
			| BinOp::CmpNEqu
			| BinOp::CmpLt
			| BinOp::CmpLtE
			| BinOp::CmpGt
			| BinOp::CmpGtE
				=> panic!(""),

			BinOp::Add => self.write_str(" += "),
			BinOp::Sub => self.write_str(" -= "),

			BinOp::Mul => self.write_str(" *= "),
			BinOp::Div => self.write_str(" /= "),
			BinOp::Mod => self.write_str(" %= "),
			}
			self.write_node(v, node_p);
			},

		&Node::Cast(ref ty, ref v) => {
			self.write_str("(");
			self.write_type(ty, |_|{});
			self.write_str(")");
			self.write_node(v, node_p);
			},
		&Node::SizeofType(ref ty) => {
			self.write_str("sizeof ");
			self.write_type(ty, |_|{});
			},
		&Node::SizeofExpr(ref v) => {
			self.write_str("sizeof(");
			self.write_node(v, super::NodePrecedence::CommaOperator.up());
			self.write_str(")");
			},
		Node::Intrinsic(ref name, ref tys, ref vals) => {
			self.write_str("__magiccall__(");
			write!(self, "{:?} : ", name);
			for v in vals {
				self.write_node(v, super::NodePrecedence::CommaOperator.up());
				self.write_str(",");
			}
			self.write_str(" : ");
			for ty in tys {
				self.write_type(ty, |_|{});
				self.write_str(",");
			}
			self.write_str(")");
			},

		&Node::Ternary(ref c, ref t, ref f) => {
			self.write_node(c, node_p.down());
			self.write_str("?");
			self.write_node(t, node_p.down());
			self.write_str(":");
			self.write_node(f, node_p);
			},
		&Node::UniOp(ref op, ref v) => {
			match op
			{
			&UniOp::Neg => self.write_str("- "),
			&UniOp::BitNot => self.write_str("~"),
			&UniOp::LogicNot => self.write_str("!"),
			&UniOp::PreInc => self.write_str("++"),
			&UniOp::PreDec => self.write_str("--"),
			&UniOp::PostInc => { self.write_node(v, node_p); self.write_str("++"); return },
			&UniOp::PostDec => { self.write_node(v, node_p); self.write_str("--"); return },
			&UniOp::Address => self.write_str("&"),
			&UniOp::Deref => self.write_str("*"),
			}
			self.write_node(v, node_p);
			},
		&Node::BinOp(ref op, ref l, ref r) => {
			self.write_node(l, node_p);
			match op
			{
			&BinOp::LogicAnd => self.write_str(" && "),
			&BinOp::LogicOr  => self.write_str(" || "),

			&BinOp::BitAnd => self.write_str(" & "),
			&BinOp::BitOr  => self.write_str(" | "),
			&BinOp::BitXor => self.write_str(" ^ "),

			&BinOp::ShiftLeft  => self.write_str(" << "),
			&BinOp::ShiftRight => self.write_str(" >> "),

			&BinOp::CmpEqu  => self.write_str(" == "),
			&BinOp::CmpNEqu => self.write_str(" != "),
			&BinOp::CmpLt  => self.write_str(" < "),
			&BinOp::CmpLtE => self.write_str(" <= "),
			&BinOp::CmpGt  => self.write_str(" > "),
			&BinOp::CmpGtE => self.write_str(" >= "),

			&BinOp::Add => self.write_str(" + "),
			&BinOp::Sub => self.write_str(" - "),

			&BinOp::Mul => self.write_str(" * "),
			&BinOp::Div => self.write_str(" / "),
			&BinOp::Mod => self.write_str(" % "),
			}
			self.write_node(r, node_p);
			},
		&Node::Index(ref v, ref i) => {
			self.write_node(v, node_p);
			self.write_str("[");
			self.write_node(i, super::NodePrecedence::CommaOperator.up());
			self.write_str("]");
			},
		&Node::DerefMember(ref v, ref n) => {
			self.write_node(v, node_p);
			self.write_str("->");
			self.write_str(n);
			},
		&Node::Member(ref v, ref n) => {
			self.write_node(v, node_p);
			self.write_str(".");
			self.write_str(n);
			},
		}
		if node.get_precedence() < max_p {
			self.write_str(")");
		}
	}

	fn write_str(&mut self, s: &str) {
		let _ = self.sink.write_all(s.as_bytes());
	}
	fn write_fmt(&mut self, f: ::std::fmt::Arguments) {
		let _ = self.sink.write_fmt(f);
	}
}

