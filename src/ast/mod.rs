/*
 */
use std::collections::HashMap;

#[deriving(Default)]
pub struct Program
{
	typedefs: HashMap<String,::types::TypeRef>,
	structs: HashMap<String, ::types::StructRef>,
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
		fail!("TODO: Define variable {} {} = {}", typeid, name, value);
	}
	
	pub fn set_typedef(&mut self, name: String, typeid: ::types::TypeRef) -> bool
	{
		self.typedefs.insert(name, typeid)
	}
	pub fn get_typedef(&self, name: &str) -> Option<::types::TypeRef>
	{
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
}

#[deriving(Show)]
pub enum Node
{
	NodeIdentifier(String),
	NodeInteger(u64),
	NodeFloat(f64),
	
	NodeFcnCall(Box<Node>, Vec<Box<Node>>),
	
	NodeAdd(Box<Node>, Box<Node>),
	NodeSub(Box<Node>, Box<Node>),
}

// vim: ft=rust
