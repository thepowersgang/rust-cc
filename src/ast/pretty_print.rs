
use super::ItemRef;

pub fn write(mut sink: impl ::std::io::Write, prog: &super::Program)
{
	PrettyPrinter { sink: &mut sink }.write_program(prog);
}

struct PrettyPrinter<'a> {
	sink: &'a mut ::std::io::Write
}

impl<'a> PrettyPrinter<'a>
{

	fn write_program(&mut self, prog: &super::Program)
	{
		for item in &prog.item_order
		{
			match item
			{
			&ItemRef::ValueDecl(ref name) => {
				self.write_prototype(&prog.symbols[name]);
				},
			&ItemRef::Value(ref name) => {
				self.write_value(&prog.symbols[name]);
				},
			_ => {},
			}
		}
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
			self.write_str(" = ");
			self.write_node(v);
			self.write_str(";\n");
			},
		None => panic!("Value with no value"),
		}
	}


	//fn write_type(&mut self, ty: &::types::TypeRef, mut cb: impl FnOnce(&mut Self))
	fn write_type(&mut self, ty: &::types::TypeRef, mut cb: impl FnMut(&mut Self))
	{
		//self.write_type_d(ty, Box::new(cb));
		self.write_type_d(ty, &mut cb);
	}
	//fn write_type_d<'b>(&mut self, ty: &::types::TypeRef, cb: Box<::std::boxed::FnBox(&mut Self)+'b>)
	fn write_type_d(&mut self, ty: &::types::TypeRef, cb: &mut FnMut(&mut Self))
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
			self.write_type_qualifiers(&ty.qualifiers);
			if r.borrow().name != "" {
				write!(self, "struct {} ", r.borrow().name);
			}
			else {
				self.write_str("struct { ... } ");
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
				}
				s.write_str("]");
				});
			},
		BaseType::Function(ref ret, ref args) => {
			self.write_type(ret, |_| {});
			(*cb)(self);
			self.write_str("(");
			let mut b = false;
			for (aty, aname) in args {
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
			if self.write_stmt(sn, indent+1) {
			}
			else {
				self.write_str(";\n");
			}
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
		&Statement::Expr(ref e) => { self.write_node(e); false },
		&Statement::Block(ref b) => { self.write_block(b, indent+1); true },
		&Statement::IfStatement { ref cond, ref true_arm, ref else_arm } => {
			self.write_str("if( ");
			self.write_defexpr(cond);
			self.write_str(" )\n");
			self.write_block(true_arm, indent+1);
			if let &Some(ref ea) = else_arm {
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
			self.write_str("while( ");
			self.write_node(cond);
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
				self.write_node(cond);
			}
			self.write_str("; ");
			if let Some(inc) = inc {
				self.write_node(inc);
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
				self.write_node(e);
			}
			false
			},
		&Statement::Goto(ref n) => { write!(self, "goto {}", n); false },

		&Statement::Switch(ref v, ref stmts) => {
			self.write_str("switch "); self.write_node(v);
			self.write_block(stmts, indent+1);
			true
			},

		// TODO: Labels are usually negatively indented.
		&Statement::Label(ref n) => { write!(self, "{}:", n); true },
		&Statement::CaseDefault => { self.write_str("default:"); true },
		&Statement::CaseSingle(v) => { write!(self, "case {}:", v); true },
		&Statement::CaseRange(v1, v2) => { write!(self, "case {} ... {}:", v1, v2); true },
		}
	}
	fn write_vardef(&mut self, node: &super::VarDefList)
	{
		/* TODO */
	}
	fn write_defexpr(&mut self, node: &super::ExprOrDef)
	{
		match node
		{
		&super::ExprOrDef::Expr(ref v) => self.write_node(v),
		&super::ExprOrDef::Definition(ref d) => self.write_vardef(d),
		}
	}
	fn write_node(&mut self, node: &super::Node)
	{
		use super::Node;
		match node
		{
		&Node::StmtList(ref subnodes) => {
			self.write_node(&subnodes[0]);
			for sn in &subnodes[1..] {
				self.write_str(", ");
				self.write_node(sn);
			}
			},

		&Node::Identifier(ref n) => self.write_str(n),
		&Node::String(ref s) => write!(self, "{:?}", s),
		&Node::Integer(v) => write!(self, "{}", v),
		&Node::Float(v) => write!(self, "{}", v),
		//&Node::ListLiteral(Vec<Node>),	// {a, b, c}
		//&Node::ArrayLiteral(Vec<(usize,Node)>),	// {[0] = a, [1] = b, [2] = c}
		//&Node::StructLiteral(Vec<(String,Node)>),	// {.a = a, .b = b, .c = c}

		&Node::FcnCall(ref fcn, ref values) => {
			self.write_node(fcn);
			self.write_str("(");
			if values.len() > 0 {
				self.write_node(&values[0]);
				for sn in &values[1..] {
					self.write_str(", ");
					self.write_node(sn);
				}
			}
			self.write_str(")");
			},
		_ => {},
		}
	}

	fn write_str(&mut self, s: &str) {
		self.sink.write_all(s.as_bytes());
	}
	fn write_fmt(&mut self, f: ::std::fmt::Arguments) {
		self.sink.write_fmt(f);
	}
}

