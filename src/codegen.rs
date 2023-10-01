
mod cranelift;
mod mrustc_mmir;

pub struct Context
{
	inner: Inner,
}
pub enum BackendName {
	Cranelift,
	MrustcMmir,
}
enum Inner {
	Cranelift(self::cranelift::Context),
	Mmir(self::mrustc_mmir::Context),
}
impl Context
{
	pub fn new(backend: BackendName) -> Self
	{
		Context {
			inner: match backend
				{
				BackendName::Cranelift => Inner::Cranelift(self::cranelift::Context::new()),
				BackendName::MrustcMmir => Inner::Mmir(self::mrustc_mmir::Context::new()),
				}
			}
	}
	pub fn finish(self, sink: impl ::std::io::Write) -> Result<(), Box<dyn std::error::Error>>
	{
		self.inner.finish(sink)
	}

	/// Declare the existance of a function
	pub fn declare_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType)
	{
		self.inner.declare_function(name, ty)
	}
	pub fn declare_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef)
	{
		self.inner.declare_value(name, ty)
	}
	pub fn lower_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef, val: &crate::ast::Initialiser)
	{
		self.inner.lower_value(name, ty, val)
	}
	pub fn lower_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		self.inner.lower_function(name, ty, body)
	}
}

impl Inner {
	pub fn finish(self, sink: impl ::std::io::Write) -> Result<(), Box<dyn std::error::Error>>
	{
		match self {
		Inner::Cranelift(i) => i.finish(sink),
    	Inner::Mmir(i) => i.finish(sink),
		}
	}

	pub fn declare_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType)
	{
		match self {
		Inner::Cranelift(i) => i.declare_function(name, ty),
    	Inner::Mmir(i) => i.declare_function(name, ty),
		}
	}
	pub fn declare_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef)
	{
		match self {
		Inner::Cranelift(i) => i.declare_value(name, ty),
    	Inner::Mmir(i) => i.declare_value(name, ty),
		}
	}
	pub fn lower_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef, val: &crate::ast::Initialiser)
	{
		match self {
		Inner::Cranelift(i) => i.lower_value(name, ty, val),
    	Inner::Mmir(i) => i.lower_value(name, ty, val),
		}
	}
	pub fn lower_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		match self {
		Inner::Cranelift(i) => i.lower_function(name, ty, body),
    	Inner::Mmir(i) => i.lower_function(name, ty, body),
		}
	}

}