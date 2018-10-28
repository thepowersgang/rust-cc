
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
		self.write_str(" {\n");
		self.write_str("\t...\n");
		self.write_str("}\n");
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


	fn write_str(&mut self, s: &str) {
		self.sink.write_all(s.as_bytes());
	}
	fn write_fmt(&mut self, f: ::std::fmt::Arguments) {
		self.sink.write_fmt(f);
	}
}

