//!
//!
use crate::ast::{self, Ident};
use crate::types::{TypeRef,BaseType};

pub fn handle_function(program: &ast::Program, name: &str, ty: &crate::types::FunctionType, fcn_body: &mut ast::FunctionBody)
{
	debug!("handle_function({}: {:?})", name, ty);
	let mut ctx = Context {
		program: program,
		scopes: Default::default(),
		variables: Default::default(),
		ret_ty: ty.ret.clone(),
		};
	ctx.scopes.push(Default::default());
	for (ty, name) in &ty.args
	{
		ctx.define_variable(&ast::Span { layers: Default::default() }, name.clone(), ty.clone());
	}

	ctx.visit_block(&mut fcn_body.code);

	// Save the variable list.
	for e in ctx.variables
	{
		fcn_body.var_table.push(ast::VarTableEnt {
			//span: e.span,
			name: e.name,
			ty: e.ty,
			});
	}
}
pub fn handle_global(program: &ast::Program, name: &str, ty: &crate::types::TypeRef, val: Option<&mut ast::Initialiser>)
{
	debug!("handle_global({}: {:?}): val={:?}", name, ty, val);

	if let Some(init) = val {
		let mut ctx = Context {
			program: program,
			scopes: Default::default(),
			variables: Default::default(),
			ret_ty: ty.clone(),
			};
		ctx.visit_init(ty, init);
	}
}

struct Context<'a>
{
	program: &'a ast::Program,
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
	//span: crate::ast::Span,
	name: Ident,
	ty: TypeRef,
}

impl<'a> Context<'a>
{
	fn define_variable(&mut self, span: &crate::ast::Span, name: Ident, ty: TypeRef) -> usize
	{
		self.visit_ty(span, &ty, true);

		let idx = self.variables.len();
		self.variables.push(VarDef {
			//span: span.clone(),
			name: name.clone(),
			ty: ty,
			});
		let scope = self.scopes.last_mut().unwrap();
		if let Some(_v) = scope.variables.get(&name) {
			// TODO: This could be just a warning (C standard doesn't allow multiple definitions, but we could)
			span.error(format_args!("Multiple defintions of `{}`", name));
		}
		scope.variables.insert(name, ValDef::Variable(idx));
		idx
	}

	fn visit_ty(&mut self, span: &crate::ast::Span, ty: &TypeRef, allow_dyn_arrays: bool)
	{
		match &ty.basetype
		{
		BaseType::Void
		|BaseType::Bool
		|BaseType::Struct(_)
		|BaseType::Enum(_)
		|BaseType::Union(_)
		|BaseType::Float(_)
		|BaseType::Integer(_)
		|BaseType::MagicType(_)
			=> {},
		BaseType::TypeOf(inner) => {
			inner.resolve(|node| self.visit_node(node, false));
			},
		BaseType::Pointer(inner) => self.visit_ty(span, inner, false),
		BaseType::Array(inner, size) => {
			self.visit_ty(span, inner, false);
			match size
			{
			crate::types::ArraySize::None => if allow_dyn_arrays {} else { span.error(format_args!("Unexpected unsized array"))},
			crate::types::ArraySize::Fixed(_) => {},
			crate::types::ArraySize::Expr(e) => e.resolve(|node| {
				self.visit_node(node, false);
				let t = node_ty(node);
				match t.basetype {
				BaseType::Integer(_) => {},
				BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => {},
				_ => {},
				}
				})
			}
			},
		BaseType::Function(_) => {},
		}
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
				var_def.index = Some(self.define_variable(&var_def.span, var_def.name.clone(), var_def.ty.clone()));
				if let Some(init) = &mut var_def.value {
					self.visit_init(&var_def.ty, init);
					if let crate::ast::Initialiser::ArrayLiteral(ref v) = init {
						let mut max_size = 0;
						let mut size_known = true;
						for (node, _) in v {
							match node.const_eval_opt() {
							ast::ConstVal::Integer(v) => max_size = max_size.max(v),
							ast::ConstVal::None => { size_known = false; },
							_ => {},
							}
						}
						if size_known {
							if let BaseType::Array(ref ity, crate::types::ArraySize::None) = var_def.ty.basetype {
								var_def.ty = ::types::Type::new_ref(::types::BaseType::Array(ity.clone(), ::types::ArraySize::Fixed(max_size+1)), var_def.ty.qualifiers.clone());
							}
						}
					}
				}
			}
			},
		Statement::Expr(ref mut e) => {
			debug!("{:?} {:?}", e.span.layers.last(), e);
			self.visit_node(e, false);
			},
		Statement::Block(ref mut stmts) => {
			self.visit_block(stmts);
			},

		// TODO: if and loops
		Statement::IfStatement { ref mut cond, ref mut true_arm, ref mut else_arm } => {
			trace!("if( {:?} )", cond);
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
			trace!("while( {:?} )", cond);
			self.scopes.push(Default::default());
			self.visit_expr_def(cond);
			// TODO: Ensure that this is bool-like
			self.visit_block(body);
			self.scopes.pop();
			},
		Statement::DoWhileLoop { ref mut body, ref mut cond } => {
			trace!("do {{ ... }} while( {:?} )", cond);
			self.scopes.push(Default::default());
			self.visit_block(body);
			self.visit_node(cond, false);
			// TODO: Ensure that this is bool-like
			self.scopes.pop();
			},
		Statement::ForLoop { ref mut init, ref mut cond, ref mut inc, ref mut body } => {
			trace!("for( {:?}; {:?}; {:?} )", init, cond, inc);
			self.scopes.push(Default::default());
			if let Some(init) = init {
				self.visit_expr_def(init);
			}
			if let Some(cond) = cond {
				self.visit_node(cond, false);
				// TODO: Ensure that this is bool-like
			}
			if let Some(inc) = inc {
				self.visit_node(inc, false);
			}
			self.visit_block(body);
			self.scopes.pop();
			},

		Statement::Break => {
			// TODO: Check that there's a loop available
			},
		Statement::Continue => {
			// TODO: Check that there's a loop available
			},
		Statement::Return(ref mut opt_val) => {
			trace!("Return {:?}", opt_val);
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

		Statement::Switch(ref mut val, ref mut body) => {
			trace!("switch {:?}", val);
			self.visit_node(val, false);
			// TODO: Check that the value is an int?
			// TODO: Push the value's type for match values?
			self.visit_block(body);
			},
		Statement::CaseDefault => {},
		Statement::CaseSingle(_v) => {},
		Statement::CaseRange(_v1, _v2) => {},

		Statement::Goto(ref _lbl) => {},
		Statement::Label(ref _lbl) => {},
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
				var_def.index = Some(self.define_variable(&var_def.span, var_def.name.clone(), var_def.ty.clone()));
				if let Some(init) = &mut var_def.value {
					self.visit_init(&var_def.ty, init);
				}
			}
			},
		}
	}

	fn visit_node(&mut self, node: &mut ast::Node, req_lvalue: bool)
	{
		let ty = self.visit_node_inner(&node.span, &mut node.kind, req_lvalue);
		node.meta = Some(ast::NodeMeta {
			is_lvalue: req_lvalue,
			ty: ty,
			});
	}
	fn visit_node_inner(&mut self, span: &crate::ast::Span, node_kind: &mut ast::NodeKind, req_lvalue: bool) -> TypeRef
	{
		use crate::ast::NodeKind;
		match *node_kind
		{
		NodeKind::StmtList(ref mut nodes) => {
			let (last, nodes) = nodes.split_last_mut().unwrap();
			for n in nodes {
				self.visit_node(n, false);
			}
			self.visit_node(last, req_lvalue);
			node_ty(last).clone()
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
			// Search global scope (wait, shouldn't this have happened during parse?)
			if let Some(v) = self.program.get_symbol(name) {
				if let BaseType::Function(_) = v.symtype.basetype {
					// Special type for functions, as they have strange typecheck decay rules
					*binding = Some(ast::IdentRef::Function);
				}
				else {
					*binding = Some(ast::IdentRef::StaticItem);
				}
				return v.symtype.clone();
			}
			if let Some( (enm, idx) ) = self.program.find_enum_var(name) {
				*binding = Some(ast::IdentRef::Enum( enm.clone(), idx ));
				return crate::types::Type::new_ref_bare(BaseType::Enum(enm));
				//return crate::types::Type::new_ref_bare(BaseType::Integer(crate::types::IntClass::int()));
			}
			span.error(format_args!("Unable to find '{}'", name));
			},
		NodeKind::String(_) => {
			if req_lvalue {
				self.err_no_lvalue(span, node_kind);
			}
			crate::types::Type::new_ref_bare(BaseType::Pointer(
				crate::types::Type::new_ref(
					BaseType::Integer(crate::types::IntClass::Char(None)),
					{ let mut q = crate::types::Qualifiers::new(); q.set_const(); q }
					)
				))
			},
		NodeKind::Integer(_val, ty) => {
			if req_lvalue {
				self.err_no_lvalue(span, node_kind);
			}
			crate::types::Type::new_ref_bare(BaseType::Integer(ty))
			},
		NodeKind::Float(_val, ty) => {
			if req_lvalue {
				self.err_no_lvalue(span, node_kind);
			}
			crate::types::Type::new_ref_bare(BaseType::Float(ty))
			},

		NodeKind::FcnCall(ref mut fcn, ref mut args) => {
			if req_lvalue {
				self.err_no_lvalue(span, node_kind);
			}
			self.visit_node(fcn, false);	// Does it need to be addressable?
			let fcn_ty = node_ty(&fcn);
			let fcn_ty = match fcn_ty.basetype
				{
				BaseType::Pointer(ref inner) => inner,
				_ => fcn_ty,
				};
			let fcn_ty = match fcn_ty.basetype
				{
				BaseType::Function(ref f) => f,
				_ => span.todo(format_args!("TODO: Error when calling a non-function. {:?}", fcn_ty)),
				};
			for arg_val in args.iter_mut() {
				self.visit_node(arg_val, false);
			}
			for (arg_val, arg_ty_tup) in Iterator::zip( args.iter_mut(), fcn_ty.args.iter() )  {
				self.coerce_ty(&arg_ty_tup.0, arg_val);
			}
			// Variadic functions have `void` as the last arg
			if fcn_ty.is_variadic {
				for arg_val in args.iter_mut().skip(fcn_ty.args.len()) {
					// Any restriction on values?
					// - float must be double?
					// - integers must be `int` or larger
					let src_ty = &arg_val.meta.as_ref().unwrap().ty;
					if let BaseType::Array(ref inner, _) = src_ty.basetype {
						let ty = crate::types::Type::new_ref(BaseType::Pointer(inner.clone()), src_ty.qualifiers.clone());
						self.coerce_ty(&ty, arg_val);
					}
				}
			}

			fcn_ty.ret.clone()
			},

		NodeKind::Assign(ref mut slot, ref mut val) => {
			// NOTE: Allows lvalue
			self.visit_node(slot, true);
			self.visit_node(val, false);
			self.coerce_ty( node_ty(&slot), val );
			node_ty(&slot).clone()
			},
		NodeKind::AssignOp(ref _op, ref mut slot, ref mut val) => {
			// NOTE: Allows lvalue
			self.visit_node(slot, true);
			self.visit_node(val, false);
			let slot_ty = node_ty(&slot);
			if let BaseType::Pointer(..) = slot_ty.basetype
			{
				// TODO: Only add/sub allowed, and must be an integer (signed?)
			}
			else
			{
				self.coerce_ty(slot_ty, val);
			}
			// TODO: Ensure that the operation is valid for the type
			node_ty(&slot).clone()
			},
		NodeKind::Intrinsic(ref op, ref tys, ref mut vals) => match &op[..]
			{
			"va_start"|"va_end"|"va_copy" => {
				if req_lvalue {
					self.err_no_lvalue(span, node_kind);
				}
				for n in vals {
					self.visit_node(n, false);
				}
				crate::types::Type::new_ref_bare(BaseType::Void)
				},
			"va_arg" => {
				if req_lvalue {
					self.err_no_lvalue(span, node_kind);
				}
				for n in vals {
					self.visit_node(n, false);
				}
				tys[0].clone()
				},
			_ => span.todo(format_args!("NodeKind::Intrinsic - {:?}", node_kind)),
			}

		NodeKind::ImplicitCast(ref dst_ty, _) => {
			// This can happen if the same node is visited multiple times
			// - E.g. with `static const` globals
			::log::warn!("Unexpected ImplicitCast in typecheck (created by typecheck)");
			dst_ty.clone()
		},
		NodeKind::Cast(ref ty, ref mut val) => {
			if req_lvalue {
				self.err_no_lvalue(span, node_kind);
			}
			self.visit_node(val, false);
			// TODO: Check cast validity
			ty.clone()
			},
		NodeKind::SizeofType(ref _ty) => {
			crate::types::Type::new_ref_bare(BaseType::Integer(
					crate::types::IntClass::Long(crate::types::Signedness::Unsigned)
					))
			},
		NodeKind::SizeofExpr(ref mut val) => {
			self.visit_node(val, false);
			crate::types::Type::new_ref_bare(BaseType::Integer(
					crate::types::IntClass::Long(crate::types::Signedness::Unsigned)
					))
			},

		// ...
		NodeKind::Ternary(ref mut cond, ref mut val_true, ref mut val_false) => {
			self.visit_node(cond, false);
			let ty_bool = crate::types::Type::new_ref_bare(BaseType::Bool);
			self.coerce_ty(&ty_bool, cond);
			self.visit_node(val_true, req_lvalue);
			self.visit_node(val_false, req_lvalue);
			if node_ty(&val_true) != node_ty(&val_false) {
				let max = match self.max_ty( span, node_ty(&val_true), node_ty(&val_false))
					{
					Some(v) => v.clone(),
					None => span.todo(format_args!("Error with mismatched ternary types: {:?} and {:?}", val_true, val_false)),
					};
				self.coerce_ty(&max, val_true);
				self.coerce_ty(&max, val_false);
				max.clone()
			}
			else {
				node_ty(&val_true).clone()
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
				node_ty(&val).clone()
				},
			ast::UniOp::Deref => {
				// NOTE: Is LValue
				self.visit_node(val, false);
				match node_ty(&val).deref()
				{
				Some(v) => v,
				None => panic!("Unable to deref {:?}", node_ty(&val)),
				}
				},
			ast::UniOp::Neg => {
				if req_lvalue {
					self.err_no_lvalue(span, node_kind);
				}
				self.visit_node(val, false);
				// TODO: Check type (signed/float)
				node_ty(&val).clone()
				},
			ast::UniOp::BitNot => {
				if req_lvalue {
					self.err_no_lvalue(span, node_kind);
				}
				self.visit_node(val, false);
				// TODO: Check type (unsigned only)
				node_ty(&val).clone()
				},
			ast::UniOp::LogicNot => {
				if req_lvalue {
					self.err_no_lvalue(span, node_kind);
				}
				self.visit_node(val, false);
				// TODO: Check for bool-able
				crate::types::Type::new_ref_bare(BaseType::Bool)
				},
			ast::UniOp::Address => {
				if req_lvalue {
					self.err_no_lvalue(span, node_kind);
				}
				self.visit_node(val, true);	// Needs a lvalue
				crate::types::Type::new_ref_bare( BaseType::Pointer(node_ty(&val).clone()) )
				},
			}
			},
		NodeKind::BinOp(ref op, ref mut val_l, ref mut val_r) => {
			if req_lvalue {
				self.err_no_lvalue(span, node_kind);
			}
			self.visit_node(val_l, false);
			self.visit_node(val_r, false);

			if let BaseType::Array(ref inner, _) = node_ty(&val_l).basetype {
				let ptr_ty = crate::types::Type::new_ref(
					BaseType::Pointer(inner.clone()),
					node_ty(&val_l).qualifiers.clone(),
					);
				self.coerce_ty(&ptr_ty, val_l);
			}
			if let BaseType::Array(ref inner, _) = node_ty(&val_r).basetype {
				let ptr_ty = crate::types::Type::new_ref(
					BaseType::Pointer(inner.clone()),
					node_ty(&val_r).qualifiers.clone(),
					);
				self.coerce_ty(&ptr_ty, val_r);
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
				=> {
					if node_ty(&val_l).basetype != node_ty(&val_r).basetype {
						let max = match self.max_ty(span, node_ty(&val_l), node_ty(&val_r))
							{
							Some(v) => v,
							None => span.todo(format_args!("Error with mismatched cmp types: {:?} {:?} and {:?}", op, val_l, val_r)),
							};
						self.coerce_ty(&max, val_l);
						self.coerce_ty(&max, val_r);
					}
					crate::types::Type::new_ref_bare(BaseType::Bool)
				},
			BinOp::LogicAnd
			| BinOp::LogicOr
				=> {
					let ty_bool = crate::types::Type::new_ref_bare(BaseType::Bool);
					self.coerce_ty(&ty_bool, val_l);
					self.coerce_ty(&ty_bool, val_r);

					fn make_int_node(span: &ast::Span, v: u64) -> ast::Node {
						ast::Node {	
							kind: NodeKind::Integer(v, crate::types::IntClass::Char(Some(crate::types::Signedness::Unsigned))),
							span: span.clone(),
							meta: Some(ast::NodeMeta {
								ty: crate::types::Type::new_ref_bare(BaseType::Bool),
								is_lvalue: false,
							}),
						}
					}
					// Semi-hack - Transform into a ternary to handle short-circuiting
					if true {
						let val_l = ::std::mem::replace(val_l, Box::new(make_int_node(span, 99)));
						let val_r = ::std::mem::replace(val_r, Box::new(make_int_node(span, 99)));
						if let BinOp::LogicAnd = *op {
							let node_false = Box::new(make_int_node(span, 0));
							*node_kind = NodeKind::Ternary(val_l, val_r, node_false)
						}
						else {
							let node_true = Box::new(make_int_node(span, 1));
							*node_kind = NodeKind::Ternary(val_l, node_true, val_r)
						}
					}

					ty_bool
				},
			BinOp::ShiftLeft
			| BinOp::ShiftRight
				=> {
					if node_ty(&val_l).basetype != node_ty(&val_r).basetype {
						self.coerce_ty(node_ty(&val_l), val_r);
					}
					// TODO: Check that type is an integer
					node_ty(&val_l).clone()
				},
			BinOp::Mul
			| BinOp::Div
			| BinOp::Mod
			| BinOp::Add
			| BinOp::Sub
				=> {
					// If one side is a pointer, things get funny
					let ty_l = node_ty(&val_l);
					let ty_r = node_ty(&val_r);
					if let BaseType::Pointer(_) = ty_l.basetype {
						let ptrdiff_t = crate::types::Type::new_ref_bare(BaseType::Integer(
								crate::types::IntClass::Long(crate::types::Signedness::Signed)
								));
						if let BaseType::Pointer(_) = ty_r.basetype {
							ptrdiff_t
						}
						else {
							self.coerce_ty(&ptrdiff_t, val_r);
							ty_l.clone()
						}
					}
					else if let BaseType::Pointer(_) = ty_r.basetype {
						todo!("Handle RHS being a pointer but LHS not");
					}
					else if ty_l.basetype != ty_r.basetype {
						let max = match self.max_ty(span, ty_l, ty_r)
							{
							Some(v) => v.clone(),
							None => span.todo(format_args!("Error with mismatched binop types: {:?} {:?} and {:?}", op, val_l, val_r)),
							};
						self.coerce_ty(&max, val_l);
						self.coerce_ty(&max, val_r);
						max
					}
					else {
						node_ty(&val_l).clone()
					}
				},
			BinOp::BitAnd
			| BinOp::BitOr
			| BinOp::BitXor
				=> {
					let ty_l = node_ty(&val_l);
					let ty_r = node_ty(&val_r);
					if ty_l.basetype != ty_r.basetype {
						let max = match self.max_ty(span, ty_l, ty_r)
							{
							Some(v) => v.clone(),
							None => span.todo(format_args!("Error with mismatched bitop types: {:?} {:?} and {:?}", op, val_l, val_r)),
							};
						self.coerce_ty(&max, val_l);
						self.coerce_ty(&max, val_r);
						max
					}
					else {
						ty_l.clone()
					}
				},
			}
			},

		// NOTE: This is actually `*(val + idx)` - should implement it as so
		// - I would like to keep it as an operation, so the mmir backend can be neater
		NodeKind::Index(ref mut val, ref mut idx) => if false {
				// NOTE: Is always an LValue
				self.visit_node(val, false);
				self.visit_node(idx, false);
				let ty_val = node_ty(&val);
				let ty_idx = node_ty(&idx);
				match ty_val.deref()
				{
				Some(rv) => rv,
				None => match ty_idx.deref()
					{
					Some(rv) => rv,
					None => span.error(format_args!("Indexing using invalid types - {:?} and {:?}", ty_val, ty_idx)),
					},
				}
			}
			else {
				// - Get the value and field name
				let (val, idx) = (
					::std::mem::replace(val, Box::new( null_node(span) ) ),
					::std::mem::replace(idx, Box::new( null_node(span) ) ),
					);
				// - Update the node to be `*(a + b)`
				*node_kind = NodeKind::UniOp(ast::UniOp::Deref,
					Box::new(ast::Node::new(span.clone(), NodeKind::BinOp(ast::BinOp::Add, val, idx)))
					);
				// - Recurse
				self.visit_node_inner(span, node_kind, req_lvalue)
			},
		// Implemented as `(*val).NAME` (using replacement)
		NodeKind::DerefMember(ref mut val, ref mut name) => {
			// - Get the value and field name
			let (val, name) = (
				::std::mem::replace(val, Box::new( null_node(span) ) ),
				::std::mem::replace(name, Ident::new()),
				);
			// - Update the node to be `(*val).NAME`
			*node_kind = NodeKind::Member(
				Box::new( ast::Node::new(span.clone(), NodeKind::UniOp(ast::UniOp::Deref, val)) ),
				name
				);
			// - Recurse
			self.visit_node_inner(span, node_kind, req_lvalue)
			},
		NodeKind::Member(ref mut val, ref name) => {
			self.visit_node(val, req_lvalue);
			loop {
				if let Some((anon_name,ty)) = node_ty(&val).get_anon_field(name) {
					*val = Box::new(ast::Node::new(val.span.clone(),
						NodeKind::Member(::std::mem::replace(val, Box::new( null_node(span) ) ), anon_name)
						) );
					val.meta = Some(ast::NodeMeta {
						is_lvalue: req_lvalue,
						ty: ty,
						});
				}
				else {
					break
				}
			}
			match node_ty(&val).get_field(name)
			{
			None => span.error(format_args!("Unable to find field `{}`", name)),
			Some( (_idx, _ofs, ty, _bit_mask) ) => ty,
			}
			},
		}
	}

	fn visit_init(&mut self, exp_ty: &TypeRef, init: &mut ast::Initialiser)
	{
		use crate::ast::Initialiser;
		match *init
		{
		Initialiser::Value(ref mut node) => {
			self.visit_node(node, false);
			match exp_ty.basetype {
			BaseType::Array(ref inner, _)
				if matches!(inner.basetype, BaseType::Integer(crate::types::IntClass::Char(_)))
				&& matches!(node.kind, crate::ast::NodeKind::String(_)) => {
				return;
				},
			_ => {
				self.coerce_ty(exp_ty, node);
				},
			}
			},
		ast::Initialiser::ListLiteral(ref mut ents) => {
			for (i,e) in Iterator::enumerate(ents.iter_mut())
			{
				let exp_ty = match exp_ty.basetype
					{
					BaseType::Array(ref inner, _) => inner.clone(),
					BaseType::Struct(ref s) =>
						match s.borrow().get_field_idx(i)
						{
						Some( (_, _, ty, _) ) => ty.clone(),
						None => panic!("Too many initialisers for struct"),
						},
					_ => todo!("List literal {:?}", exp_ty),
					};
				self.visit_init(&exp_ty, e);
			}
			},
		ast::Initialiser::ArrayLiteral(ref mut ents) => {
			for (node, e) in ents.iter_mut()
			{
				self.visit_node(node, false);
				let exp_ty = match exp_ty.basetype
					{
					BaseType::Array(ref inner, _) => inner.clone(),
					_ => todo!("List literal {:?}", exp_ty),
					};
				self.visit_init(&exp_ty, e);
			}
			},
		ast::Initialiser::StructLiteral(ref mut ents) => {
			for (name, e) in ents.iter_mut()
			{
				let exp_ty = match exp_ty.basetype
					{
					BaseType::Struct(ref s) =>
						match s.borrow().iter_fields().find(|v| v.1 == name)
						{
						Some( (_, _, ty, _) ) => ty.clone(),
						None => panic!("Unknown struct entry: {} in {:?}", name, exp_ty),
						},
					_ => todo!("Struct literal {:?}", exp_ty),
					};
				self.visit_init(&exp_ty, e);
			}
			},
		}
	}

	fn max_ty(&self, span: &crate::ast::Span, ty1: &TypeRef, ty2: &TypeRef) -> Option<TypeRef> {
		use crate::types::{Signedness,IntClass};
		use crate::types::{MagicType,MagicTypeRepr};
		fn sgn(s1: &Signedness, s2: &Signedness) -> Signedness {
			match (s1,s2)
			{
			(Signedness::Unsigned, Signedness::Unsigned) => Signedness::Unsigned,
			_ => Signedness::Signed,
			}
		}
		if let BaseType::TypeOf(ref inner) = ty1.basetype {
			return self.max_ty(span, inner.get(), ty2);
		}
		if let BaseType::TypeOf(ref inner) = ty2.basetype {
			return self.max_ty(span, ty1, inner.get());
		}
		Some(crate::types::Type::new_ref_bare(match (&ty1.basetype, &ty2.basetype)
		{
		// Bool with any integer just uses the other integer
		(BaseType::Bool, BaseType::Integer(i), )
		| (BaseType::Integer(i), BaseType::Bool, )
			=> BaseType::Integer(i.clone()),
		(BaseType::Bool, t @ BaseType::MagicType(MagicType::Named(_, MagicTypeRepr::Integer { .. })), )
		| (t @ BaseType::MagicType(MagicType::Named(_, MagicTypeRepr::Integer { .. })), BaseType::Bool, )
			=> t.clone(),
		
		(BaseType::Integer(i1), BaseType::Integer(i2)) => BaseType::Integer(match i1
			{
			//IntClass::Bits(_s1, _n) => todo!("max_ty Integers {:?} {:?}", i1, i2),
			IntClass::Char(_s1) => match i2
				{
				//IntClass::Bits(_s2, _n) => todo!("max_ty Integers {:?} {:?}", i1, i2),
				_ => i2.clone_with_sgn( sgn(&i1.signedness(), &i2.signedness()) ),
				},
			IntClass::Short(s1) => match i2
				{
				//IntClass::Bits(_s2, _n) => todo!("max_ty Integers {:?} {:?}", i1, i2),
				IntClass::LongLong(s2) => i2.clone_with_sgn( sgn(s1, s2) ),
				IntClass::Long(s2) => i2.clone_with_sgn( sgn(s1, s2) ),
				IntClass::Int(s2) => i2.clone_with_sgn( sgn(s1, s2) ),
				_ => i1.clone_with_sgn( sgn(s1, &i2.signedness()) ),
				},
			IntClass::Int(s1) => match i2
				{
				//IntClass::Bits(_s2, _n) => todo!("max_ty Integers {:?} {:?}", i1, i2),
				IntClass::LongLong(s2) => i2.clone_with_sgn( sgn(s1, s2) ),
				IntClass::Long(s2) => i2.clone_with_sgn( sgn(s1, s2) ),
				_ => i1.clone_with_sgn( sgn(s1, &i2.signedness()) ),
				},
			IntClass::Long(s1) => match i2
				{
				//IntClass::Bits(_s2, _n) => todo!("max_ty Integers {:?} {:?}", i1, i2),
				IntClass::LongLong(s2) => i2.clone_with_sgn( sgn(s1, s2) ),
				_ => i1.clone_with_sgn( sgn(s1, &i2.signedness()) ),
				},
			IntClass::LongLong(s1) => match i2
				{
				//IntClass::Bits(_s2, _n) => todo!("max_ty Integers {:?} {:?}", i1, i2),
				_ => i1.clone_with_sgn( sgn(s1, &i2.signedness()) ),
				},
			}),
		  (BaseType::Integer(i1), &BaseType::MagicType(MagicType::Named(_, MagicTypeRepr::Integer { signed, bits })),)
		| (&BaseType::MagicType(MagicType::Named(_, MagicTypeRepr::Integer { signed, bits })), BaseType::Integer(i1), )
			=> {
			let (ic_s,ic_bits) = match i1
				{
				IntClass::Char(None) => (false, 7),
				IntClass::Char(Some(s)) => (!s.is_unsigned(), 8),
				IntClass::Short(s) => (!s.is_unsigned(), 16),
				IntClass::Int(s) => (!s.is_unsigned(), 16),
				IntClass::Long(s) => (!s.is_unsigned(), 32),
				IntClass::LongLong(s) => (!s.is_unsigned(), 64),
				};
			// `int` and `u16` needs `long` (must be signed, but `signed int` can't fit `u16`)
			let out_sign = ic_s | signed;
			let req_bits = bits.max(ic_bits) + if ic_s != signed { out_sign as u8 } else { 0 };
			BaseType::Integer(if req_bits <= 8 {
					IntClass::Char(Some(Signedness::from_bool_signed(out_sign)))
				}
				else if req_bits <= 16 {
					IntClass::Int(Signedness::from_bool_signed(out_sign))
				}
				else if req_bits <= 32 {
					IntClass::Long(Signedness::from_bool_signed(out_sign))
				}
				else if req_bits <= 64 {
					IntClass::LongLong(Signedness::from_bool_signed(out_sign))
				}
				else {
					//span.todo(format_args!("Pick 'max' of {:?} and {:?} (req_bits={})", ty1, ty2, req_bits))
					IntClass::LongLong(Signedness::from_bool_signed(true))
				})
			},
		(BaseType::MagicType(MagicType::Named(_, MagicTypeRepr::Integer { signed: s1, bits: b1 })),
			BaseType::MagicType(MagicType::Named(_, MagicTypeRepr::Integer { signed: s2, bits: b2 })),
			) => {
			if s1 != s2 {
				span.todo(format_args!("Pick 'max' of {:?} and {:?}", ty1, ty2))
			}
			if b1 > b2 {
				ty1.basetype.clone()
			}
			else {
				ty2.basetype.clone()
			}
			},
		
		(BaseType::Float(fc), BaseType::Integer(_), )
		| (BaseType::Integer(_), BaseType::Float(fc), )
			=> BaseType::Float(fc.clone()),
		(BaseType::Float(fc1), BaseType::Float(fc2), )
			=> BaseType::Float(match (fc1,fc2)
				{
				(crate::types::FloatClass::Float, fc) => *fc,
				(fc @ crate::types::FloatClass::Double, crate::types::FloatClass::Float) => *fc,
				(fc @ crate::types::FloatClass::Double, crate::types::FloatClass::Double) => *fc,
				(crate::types::FloatClass::Double, fc @ crate::types::FloatClass::LongDouble) => *fc,
				(fc @ crate::types::FloatClass::LongDouble, _) => *fc,
				}),
		
		(BaseType::Array(i1, _), BaseType::Array(i2, _)) => if i1 != i2 {
				span.todo(format_args!("Pick 'max' of {:?} and {:?} - Mismatched pointer inner", ty1, ty2));
			}
			else {
				BaseType::Pointer(i1.clone())
			}
		(BaseType::Pointer(i1), BaseType::Pointer(i2)) => BaseType::Pointer({
			let bt = if i1.basetype != i2.basetype {
					if let BaseType::Void = i2.basetype {
						i1.basetype.clone()
					}
					else if let BaseType::Void = i1.basetype {
						i2.basetype.clone()
					}
					else {
						span.todo(format_args!("Pick 'max' of {:?} and {:?} - Mismatched pointer inner", ty1, ty2));
					}
				}
				else {
					i1.basetype.clone()
				};
			let mut q = i1.qualifiers.clone();
			q.merge_from(&i2.qualifiers);
			crate::types::Type::new_ref(bt, q)
			}),
		(BaseType::Enum(_), BaseType::Integer(i), )
		| (BaseType::Integer(i), BaseType::Enum(_), )
			=> {
				// TODO: Check the required size of the enum
				BaseType::Integer(i.clone())
			},
		_ => span.todo(format_args!("Pick 'max' of {:?} and {:?}", ty1, ty2)),
		}))
	}
	fn coerce_ty(&self, req_ty: &TypeRef, node: &mut ast::Node)
	{
		fn expand_typeof(mut req_ty: &TypeRef) -> &TypeRef {
			while let BaseType::TypeOf(ref inner) = req_ty.basetype {
				req_ty = inner.get();
			}
			req_ty
		}
		let req_ty = expand_typeof(req_ty);
		if req_ty.basetype != node_ty(&node).basetype {
			trace!("coerce({:?}) from {:?}", req_ty, node_ty(node));
			let inner_node = ::std::mem::replace(node, null_node(&node.span));
			*node = ast::Node::new(node.span.clone(), ast::NodeKind::ImplicitCast(req_ty.clone(), Box::new(inner_node)));
			node.meta = Some(ast::NodeMeta {
				is_lvalue: false,
				ty: req_ty.clone(),
				});
			let (inner_ty, node) = match node.kind
				{
				ast::NodeKind::ImplicitCast(_, ref node) => (node_ty(&node), &**node),
				_ => unreachable!(),
				};
			let inner_ty = expand_typeof(inner_ty);
			match req_ty.basetype
			{
			BaseType::Bool => match inner_ty.basetype
				{
				BaseType::Bool => {},
				BaseType::Integer(_) => {},
				BaseType::Float(_) => {},
				BaseType::Pointer(..) => {},
				BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => {},
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty)),
				},
			BaseType::Float(_fc) => match inner_ty.basetype
				{
				BaseType::Bool => {},
				BaseType::Integer(_ici) => {},	// TODO: Warn on signed-ness?
				BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => {},
				BaseType::Float(_) => {},
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty)),
				}
			BaseType::Integer(_ic) => match inner_ty.basetype
				{
				BaseType::Bool => {},
				BaseType::Integer(_ici) => {},	// TODO: Warn on signed-ness?
				BaseType::Enum(_) => {},	// TODO: Any range checks needed?
				BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => {},
				BaseType::Float(_) => {},
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty)),
				},
			BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => match inner_ty.basetype
				{
				BaseType::Bool => {},
				BaseType::Enum(_) => {},	// TODO: Any range checks needed?
				BaseType::Integer(_ici) => {},	// TODO: Warn on signed-ness?
				BaseType::MagicType(crate::types::MagicType::Named(_, crate::types::MagicTypeRepr::Integer { .. })) => {},
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value:\n {:?}\n from\n {:?}", req_ty, inner_ty)),
				},
			BaseType::Pointer(ref i1) => match inner_ty.basetype
				{
				// Integer literal zero is allowed
				BaseType::Integer(_) if matches!(node.kind, crate::ast::NodeKind::Integer(0, _)) => {},
				BaseType::Pointer(ref _i2) => {},	// TODO: Const/restrict/etc warnings
				BaseType::Array(_, _) => {},	// TODO: Const/restrict/etc warnings
				BaseType::Function(ref ft_s) => {
					let BaseType::Function(ref ft_d) = i1.basetype else {
						node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty))
						};
					if ft_s.ret != ft_d.ret {
					}
					// TODO: Check signature
					}
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty)),
				},
			BaseType::Enum(_) => match inner_ty.basetype
				{
				BaseType::Integer(_) => {
					node.span.warning(format_args!("Coercing integer to enum ({:?} to {:?})", inner_ty, req_ty));
					},
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty)),
				},
			// TODO: unsized Arrays (for arguments)
			BaseType::Array(ref inner, crate::types::ArraySize::None) => match inner_ty.basetype
				{
				BaseType::Pointer(ref inner_src) if inner_src.basetype == inner.basetype => {},
				_ => node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty)),
				},
			_ => {
				node.span.todo(format_args!("Handle type mismatch using promotion/demotion of value: {:?} from {:?}", req_ty, inner_ty));
				},
			}
		}
	}

	fn err_no_lvalue(&self, span: &ast::Span, node_kind: &ast::NodeKind) -> !
	{
		span.error(format_args!("Unexpected node in lvalue - {:?}", node_kind));
	}
}

fn node_ty(n: &ast::Node) -> &TypeRef {
	let rv = &n.meta.as_ref().unwrap().ty;
	if let BaseType::TypeOf(ref ty) = rv.basetype {
		ty.get()
	}
	else {
		rv
	}
}

fn null_node(span: &ast::Span) -> ast::Node {
	ast::Node::new(span.clone(), ast::NodeKind::StmtList(vec![]))
}
