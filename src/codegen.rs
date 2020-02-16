
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

	pub fn lower_function(&mut self, name: &str, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		self.inner.lower_function(name, ty, body)
	}
}
