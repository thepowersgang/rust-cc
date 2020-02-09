//!
use crate::ast;
use crate::types::TypeRef;
type Ident = String;

pub fn handle_function(name: &str, ty: &crate::types::FunctionType, code: &mut ast::Block)
{
	debug!("handle_function({}: {:?})", name, ty);
	let mut ctx = Context {
		scopes: Default::default(),
		variables: Default::default(),
		ret_ty: ty.ret.clone(),
		};
	ctx.scopes.push(Default::default());
	for (ty, name) in &ty.args
	{
		ctx.define_variable(name.clone(), ty.clone());
	}

	ctx.visit_block(code);
	// TODO: Save the variable list.
}

struct Context
{
	ret_ty: TypeRef,
	scopes: Vec<VarScope>,
	variables: Vec<VarDef>,
}
#[derive(Default)]
struct VarScope
{
	variables: ::std::collections::HashMap<Ident, ValDef>,
}
enum ValDef
{
	Variable(usize),
}
struct VarDef
{
	ty: TypeRef,
}

impl Context
{
	fn define_variable(&mut self, name: Ident, ty: TypeRef)
	{
		let idx = self.variables.len();
		self.variables.push(VarDef {
			ty: ty,
			});
		let scope = self.scopes.last_mut().unwrap();
		if let Some(v) = scope.variables.get(&name) {
			// TODO: This could be just a warning (C standard doesn't allow multiple definitions, but we could)
			panic!("Multiple defintions of `{}`", name);
		}
		scope.variables.insert(name, ValDef::Variable(idx));
	}

	fn visit_block(&mut self, blk: &mut ast::Block)
	{
		self.scopes.push(Default::default());
		for stmt in blk.iter_mut()
		{
			self.visit_stmt(stmt);
		}
		self.scopes.pop();
	}
	fn visit_stmt(&mut self, stmt: &mut ast::Statement)
	{
		use crate::ast::Statement;
		match *stmt
		{
		Statement::Empty => {},
		Statement::VarDef(ref mut list) => {
			for var_def in list
			{
				self.define_variable(var_def.name.clone(), var_def.ty.clone());
				self.visit_init(&var_def.ty, &mut var_def.value);
			}
			},
		Statement::Expr(ref mut e) => {
			self.visit_node(e, false);
			},
		Statement::Block(ref mut stmts) => {
			self.visit_block(stmts);
			},

		// TODO: if and loops
		Statement::IfStatement { ref mut cond, ref mut true_arm, ref mut else_arm } => {
			self.scopes.push(Default::default());
			self.visit_expr_def(cond);
			// TODO: Ensure that this is bool-like
			self.visit_block(true_arm);
			if let Some(else_arm) = else_arm {
				self.visit_block(else_arm);
			}
			self.scopes.pop();
			},
		Statement::WhileLoop { ref mut cond, ref mut body } => {
			self.scopes.push(Default::default());
			self.visit_expr_def(cond);
			// TODO: Ensure that this is bool-like
			self.visit_block(body);
			self.scopes.pop();
			},

		// TODO: Break/continue
		Statement::Return(ref mut opt_val) => {
			if let Some(val) = opt_val
			{
				// TODO: If type is void, use a different error
				self.visit_node(val, false);
				self.coerce_ty(&self.ret_ty, val);
			}
			else
			{
				// TODO: Check type is void
			}
			},

		// TODO: Rest

		_ => todo!("visit_stmt(): {:?}", stmt),
		}
	}

	fn visit_expr_def(&mut self, ed: &mut ast::ExprOrDef)
	{
		match ed
		{
		ast::ExprOrDef::Expr(ref mut node) => self.visit_node(node, false),
		ast::ExprOrDef::Definition(ref mut defs) => {
			for var_def in defs
			{
				self.define_variable(var_def.name.clone(), var_def.ty.clone());
				self.visit_init(&var_def.ty, &mut var_def.value);
			}
			},
		}
	}

	fn visit_node(&mut self, node: &mut ast::Node, req_lvalue: bool)
	{
		let ty = self.visit_node_inner(&mut node.kind, req_lvalue);
		node.is_lvalue = Some(req_lvalue);
		node.ty = Some(ty);
	}
	fn visit_node_inner(&mut self, node_kind: &mut ast::NodeKind, req_lvalue: bool) -> TypeRef
	{
		use crate::ast::NodeKind;
		match *node_kind
		{
		NodeKind::StmtList(ref mut nodes) => {
			let (last, nodes) = nodes.split_last_mut().unwrap();
			for n in nodes {
				self.visit_node(n, false);
			}
			self.visit_node(last, true);
			last.ty.clone().unwrap()
			},
		NodeKind::Identifier(ref name, ref mut binding) => {
			for s in self.scopes.iter().rev()
			{
				if let Some(v) = s.variables.get(name)
				{
					let ty;
					*binding = Some(match *v
						{
						ValDef::Variable(idx) => {
							ty = self.variables[idx].ty.clone();
							crate::ast::IdentRef::Local(idx)
							},
						});
					return ty;
				}
			}
			// TODO: Search global scope (wait, shouldn't this have happened during parse?)
			panic!("Unable to find '{}'", name);
			},
		NodeKind::Integer(_val, ty) => {
			if req_lvalue {
				self.err_no_lvalue();
			}
			crate::types::Type::new_ref_bare(crate::types::BaseType::Integer(ty))
			},
		// ...
		NodeKind::FcnCall(ref mut fcn, ref mut args) => {
			if req_lvalue {
				self.err_no_lvalue();
			}
			self.visit_node(fcn, false);	// Does it need to be addressable?
			let fcn_ty = fcn.ty.as_ref().unwrap();
			let fcn_ty = match fcn_ty.basetype
				{
				crate::types::BaseType::Function(ref f) => f,
				_ => panic!("TODO: Error when calling a non-function. {:?}", fcn_ty),
				};
			for (arg_val, arg_ty_tup) in Iterator::zip( args.iter_mut(), fcn_ty.args.iter() )  {
				self.visit_node(arg_val, false);
				self.coerce_ty(&arg_ty_tup.0, arg_val);
			}

			fcn_ty.ret.clone()
			},

		NodeKind::Assign(ref mut slot, ref mut val) => {
			// NOTE: Allows lvalue
			self.visit_node(slot, true);
			self.visit_node(val, false);
			self.coerce_ty(slot.ty.as_ref().unwrap(), val);
			slot.ty.clone().unwrap()
			},
		NodeKind::AssignOp(ref _op, ref mut slot, ref mut val) => {
			// NOTE: Allows lvalue
			self.visit_node(slot, true);
			self.visit_node(val, false);
			if slot.ty != val.ty {
				self.coerce_ty(slot.ty.as_ref().unwrap(), val);
			}
			// TODO: Ensure that the operation is valid for the type
			slot.ty.clone().unwrap()
			},
		NodeKind::Intrinsic(..) => todo!("NodeKind::Intrinsic - {:?}", node_kind),

		NodeKind::ImplicitCast(..) => panic!("Unexpected ImplicitCast in typecheck"),
		NodeKind::Cast(ref ty, ref mut val) => {
			if req_lvalue {
				self.err_no_lvalue();
			}
			self.visit_node(val, false);
			// TODO: Check cast validity
			ty.clone()
			},

		// ...
		NodeKind::Ternary(ref mut cond, ref mut val_true, ref mut val_false) => {
			self.visit_node(cond, false);
			// TODO: Ensure that this is bool-like
			self.visit_node(val_true, req_lvalue);
			self.visit_node(val_false, req_lvalue);
			if val_true.ty != val_false.ty {
				todo!("Handle ternary type mismatch using promotion");
			}
			else {
				val_true.ty.clone().unwrap()
			}
			},
		NodeKind::UniOp(ref op, ref mut val) => {
			match op
			{
			// Pre/Post Inc/Dec require a lvalue
			ast::UniOp::PostDec
			| ast::UniOp::PreDec
			| ast::UniOp::PostInc
			| ast::UniOp::PreInc
				=> {
				// NOTE: Is LValue
				self.visit_node(val, true);	// needs a LValue
				val.ty.clone().unwrap()
				},
			ast::UniOp::Deref => {
				// NOTE: Is LValue
				self.visit_node(val, false);
				match val.ty.as_ref().unwrap().deref()
				{
				Some(v) => v,
				None => panic!("Unable to deref {:?}", val.ty),
				}
				},
			ast::UniOp::Neg => {
				if req_lvalue {
					self.err_no_lvalue();
				}
				self.visit_node(val, false);
				// TODO: Check type (signed/float)
				val.ty.clone().unwrap()
				},
			ast::UniOp::BitNot => {
				if req_lvalue {
					self.err_no_lvalue();
				}
				self.visit_node(val, false);
				// TODO: Check type (unsigned only)
				val.ty.clone().unwrap()
				},
			ast::UniOp::LogicNot => {
				if req_lvalue {
					self.err_no_lvalue();
				}
				self.visit_node(val, false);
				// TODO: Check for bool-able
				crate::types::Type::new_ref_bare(crate::types::BaseType::Bool)
				},
			ast::UniOp::Address => {
				if req_lvalue {
					self.err_no_lvalue();
				}
				self.visit_node(val, true);	// Needs a lvalue
				crate::types::Type::new_ref_bare( crate::types::BaseType::Pointer(val.ty.clone().unwrap()) )
				},
			}
			},
		NodeKind::BinOp(ref op, ref mut val_l, ref mut val_r) => {
			if req_lvalue {
				self.err_no_lvalue();
			}
			self.visit_node(val_l, false);
			self.visit_node(val_r, false);
			if val_l.ty != val_r.ty {
				todo!("Handle bin-op type mismatch using promotion: {:?} and {:?}: {:?} and {:?}", val_l.ty, val_r.ty, val_l, val_r);
			}
			use crate::ast::BinOp;
			match *op
			{
			BinOp::CmpEqu
			| BinOp::CmpNEqu
			| BinOp::CmpLt
			| BinOp::CmpLtE
			| BinOp::CmpGt
			| BinOp::CmpGtE
				=> crate::types::Type::new_ref_bare(crate::types::BaseType::Bool),
			BinOp::LogicAnd
			| BinOp::LogicOr
				=> crate::types::Type::new_ref_bare(crate::types::BaseType::Bool),
			BinOp::ShiftLeft
			| BinOp::ShiftRight
				=> val_l.ty.clone().unwrap(),
			BinOp::Mul
			| BinOp::Div
			| BinOp::Mod
			| BinOp::Add
			| BinOp::Sub
				=> val_l.ty.clone().unwrap(),
			BinOp::BitAnd
			| BinOp::BitOr
			| BinOp::BitXor
				=> val_l.ty.clone().unwrap(),
			_ => todo!("visit_node_inner(): BinOp {:?} {:?} {:?}", val_l, op, val_r),
			}
			},

		// TODO: This is actually `*(val + idx)` - should implement it as so
		NodeKind::Index(ref mut val, ref mut idx) => {
			// NOTE: Is always an LValue
			self.visit_node(val, false);	// Already a pointer, so will be LValue output
			self.visit_node(idx, false);
			val.ty.as_ref().unwrap().deref().expect("Can't index")
			},
		// Implemented as `(*val).NAME` (using replacement)
		NodeKind::DerefMember(..) => {
			// - Get the value and field name
			let (val, name) = match *node_kind {
				NodeKind::DerefMember(ref mut val, ref mut name) => {
					(
						::std::mem::replace(val, Box::new( null_node() ) ),
						::std::mem::replace(name, Ident::new()),
						)
					},
				_ => unreachable!(),
				};
			// - Update the node to be `(*val).NAME`
			*node_kind = NodeKind::Member(
				Box::new( ast::Node::new(NodeKind::UniOp(ast::UniOp::Deref, val)) ),
				name
				);
			// - Recurse
			self.visit_node_inner(node_kind, req_lvalue)
			},
		NodeKind::Member(ref mut val, ref name) => {
			self.visit_node(val, req_lvalue);
			match val.ty.as_ref().unwrap().get_field(name)
			{
			None => panic!("Unable to find field"),
			Some( (_ofs, ty) ) => ty,
			}
			},

		_ => todo!("visit_node_inner(): {:?}", node_kind),
		}
	}

	fn visit_init(&mut self, exp_ty: &TypeRef, init: &mut ast::Initialiser)
	{
		use crate::ast::Initialiser;
		match *init
		{
		Initialiser::None => {},
		Initialiser::Value(ref mut node) => {
			self.visit_node(node, false);
			// TODO: Check expected type
			},
		_ => todo!("visit_init({:?}):, {:?}", exp_ty, init),
		}
	}

	fn coerce_ty(&self, req_ty: &TypeRef, node: &mut ast::Node)
	{
		if req_ty != node.ty.as_ref().unwrap() {
			use crate::types::BaseType;
			let inner_node = ::std::mem::replace(node, null_node());
			*node = ast::Node::new(ast::NodeKind::ImplicitCast(req_ty.clone(), Box::new(inner_node)));
			node.is_lvalue = Some(false);
			node.ty = Some(req_ty.clone());
			let inner_ty = match node.kind
				{
				ast::NodeKind::ImplicitCast(_, ref node) => node.ty.as_ref().unwrap(),
				_ => unreachable!(),
				};
			match req_ty.basetype
			{
			BaseType::Integer(ic) => match inner_ty.basetype
				{
				BaseType::Bool => {},
				BaseType::Integer(ici) => {},	// TODO: Warn on signed-ness?
				_ => todo!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty),
				},
			_ => todo!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty),
			}
		}
	}

	fn err_no_lvalue(&self)
	{
		panic!("Unexpected node in lvalue");
	}
}


fn null_node() -> ast::Node {
	ast::Node::new(ast::NodeKind::StmtList(vec![]))
}
