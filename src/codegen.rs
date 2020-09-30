
mod cranelift;


pub struct Context
{
	inner: self::cranelift::Context,
}
impl Context
{
	pub fn new() -> Self
	{
		Context {
			inner: self::cranelift::Context::new(),
			}
	}
	pub fn finish(self, sink: impl ::std::io::Write) -> Result<(), Box<dyn std::error::Error>>
	{
		self.inner.finish(sink)
	}

	pub fn declare_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType)
	{
		self.inner.declare_function(name, ty);
	}
	pub fn declare_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef)
	{
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
