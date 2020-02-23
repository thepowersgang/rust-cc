
use std::collections::HashMap;
use cranelift_codegen::ir::entities as cr_e;
use cranelift_codegen::ir::condcodes as cr_cc;
use cranelift_codegen::ir::types as cr_tys;
use cranelift_codegen::ir::InstBuilder;
use crate::types::{TypeRef,BaseType};
use crate::ast::Ident;

extern crate target_lexicon;

pub struct Context
{
	module: ::cranelift_module::Module<::cranelift_object::ObjectBackend>,
	functions: HashMap<Ident, ::cranelift_codegen::ir::ExternalName>,
}
impl Context
{
	pub fn new() -> Self
	{
		let isa = {
			use std::str::FromStr;
			let shared_builder = ::cranelift_codegen::settings::builder();
			let shared_flags = ::cranelift_codegen::settings::Flags::new(shared_builder);
			let b = ::cranelift_codegen::isa::lookup( target_lexicon::triple!("x86_64-elf") ).unwrap();
			b.finish(shared_flags)
			};
		Context {
			module: ::cranelift_module::Module::new(::cranelift_object::ObjectBuilder::new(
				isa,
				"unknown_object.o".into(),
				::cranelift_object::ObjectTrapCollection::Disabled,
				::cranelift_module::default_libcall_names(),
				).expect("Can't create object builder")),
			functions: Default::default(),
			}
	}

	fn get_function(&mut self, name: &Ident) -> ::cranelift_codegen::ir::ExternalName
	{
		let idx = self.functions.len();
		self.functions.entry(name.clone())
			.or_insert_with(|| ::cranelift_codegen::ir::ExternalName::User { namespace: 0, index: idx as u32, })
			.clone()
	}

	pub fn lower_function(&mut self, name: &Ident, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		debug!("lower_function({}: {:?})", name, ty);
		use cranelift_codegen::entity::EntityRef;	// provides ::new on Variable
		use cranelift_codegen::ir::Function;
		use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

		let sig = make_sig(ty);
		// - Arguments
		let mut vars = Vec::<ValueRef>::new();
		for (idx,(_arg_ty, _arg_name)) in Iterator::enumerate(ty.args.iter()) {
			let var = ::cranelift_frontend::Variable::new(idx);
			vars.push( ValueRef::Variable(var) );
		}

		let mut fn_builder_ctx = FunctionBuilderContext::new();
		let mut func = Function::with_name_signature(self.get_function(name), sig);
		let mut b = Builder {
			context: self,
			builder: FunctionBuilder::new(&mut func, &mut fn_builder_ctx),
			stack: vec![ Scope::new() ],
			vars: vars,
			fcn_imports: Default::default(),
			};

		// Define variables
		for var in body.var_table.iter().skip( ty.args.len() )
		{
			use ::cranelift_codegen::ir::stackslot as ss;
			let size = match var.ty.get_size()
				{
				Some(s) => s,
				None => panic!("Type {:?} has no size", var.ty),
				};
			let slot = b.builder.create_stack_slot(ss::StackSlotData::new(ss::StackSlotKind::ExplicitSlot, size));
			b.vars.push( ValueRef::StackSlot(slot, 0, var.ty.clone()) );
		}

		// Block 0 - Sets up arguments
		let block0 = b.builder.create_block();
		b.builder.append_block_params_for_function_params(block0);
		b.builder.switch_to_block(block0);
		b.builder.seal_block(block0);
		for (idx,(arg_ty, _arg_name)) in Iterator::enumerate(ty.args.iter())
		{
			match b.vars[idx]
			{
			ValueRef::Variable(ref v) => {
				b.builder.declare_var(*v, cvt_ty(arg_ty));
				let tmp = b.builder.block_params(block0)[idx];
				b.builder.def_var(*v, tmp);
				},
			ref e => panic!("TODO: Arg class - {:?}", e),
			}
		}
		
		b.handle_block(&body.code);
		if !b.builder.is_filled() {
			b.builder.ins().return_(&[]);
		}

		debug!("{}", b.builder.display(None));
		b.builder.finalize();
	}
}

// Variable: A primitive value (ties together SSA values under one name)
// Stack slot: larger value stored on the stack (or anything with a pointer taken)

/// Reference to a value
#[derive(Debug,Clone)]
enum ValueRef
{
	/// No value
	Void,
	/// Temporary value
	Temporary(cr_e::Value),
	/// A non-stack variable
	Variable(::cranelift_frontend::Variable),
	/// An item on the stack
	StackSlot(cr_e::StackSlot, u32, TypeRef),
	/// A pointer to an item on the stack
	StackSlotAddr(cr_e::StackSlot, u32, TypeRef),
	/// A value in global/static storage
	Global(crate::ast::Ident, u32, TypeRef),
	/// A pointer to an item in global/static storage
	GlobalAddr(crate::ast::Ident, u32, TypeRef),
	/// By-value use of a function (decays to a pointer)
	Function(crate::ast::Ident, cr_e::FuncRef),
	/// Pointer dereference
	// pointer, offset, type
	Pointer(cr_e::Value, u32, TypeRef),
}

struct Builder<'a>
{
	context: &'a mut Context,
	builder: ::cranelift_frontend::FunctionBuilder<'a>,
	stack: Vec<Scope>,
	vars: Vec<ValueRef>,

	fcn_imports: HashMap<Ident, cr_e::FuncRef>,
}
struct Scope
{
	blk_break: Option<cr_e::Block>,
	blk_continue: Option<cr_e::Block>,
}

impl Builder<'_>
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
			let _v = self.handle_node(e);
			},
		Statement::Block(ref stmts) => {
			self.handle_block(stmts);
			},
		Statement::IfStatement { ref cond, ref true_arm, ref else_arm } => {
			trace!("{}if {:?}", self.indent(), cond);
			let cond_v = self.handle_expr_def(cond);
			let cond_v = self.get_value(cond_v);
			let true_blk = self.builder.create_block();
			let else_blk = self.builder.create_block();
			let done_blk = self.builder.create_block();
			self.builder.ins().brz(cond_v, else_blk, &[]);
			self.builder.ins().jump(true_blk, &[]);

			self.builder.switch_to_block(true_blk);
			self.builder.seal_block(true_blk);
			self.handle_block(true_arm);
			self.builder.ins().jump(done_blk, &[]);

			self.builder.switch_to_block(else_blk);
			self.builder.seal_block(else_blk);
			if let Some(else_arm) = else_arm
			{
				self.stack.push(Scope::new());
				for stmt in else_arm {
					self.handle_stmt(stmt);
				}
				self.stack.pop();
			}
			self.builder.ins().jump(done_blk, &[]);

			self.builder.switch_to_block(done_blk);
			self.builder.seal_block(done_blk)
			},

		Statement::WhileLoop { ref cond, ref body } => {
			trace!("{}while {:?}", self.indent(), cond);
			let blk_top = self.builder.create_block();
			let blk_body = self.builder.create_block();
			let blk_exit = self.builder.create_block();
			self.builder.ins().jump(blk_top, &[]);

			self.builder.switch_to_block(blk_top);
			// NOTE: no seal yet, reverse jumps happen.

			self.stack.push(Scope::new_loop(blk_top, blk_exit));
			let cond_v = self.handle_expr_def(cond);
			let cond_v = self.get_value(cond_v);
			self.builder.ins().brz(cond_v, blk_exit, &[]);
			self.builder.ins().jump(blk_body, &[]);

			self.builder.switch_to_block(blk_body);
			self.builder.seal_block(blk_body);
			self.stack.push(Scope::new());
			self.handle_block(body);
			self.stack.pop();	// Body scope

			self.builder.ins().jump(blk_top, &[]);
			self.stack.pop();	// Loop scope
			self.builder.seal_block(blk_top);	// Seal the loop body, jumps now known.

			self.builder.switch_to_block(blk_exit);
			self.builder.seal_block(blk_exit);
			},

		Statement::DoWhileLoop { ref body, ref cond } => {
			let blk_body = self.builder.create_block();
			let blk_foot = self.builder.create_block();	// target of continue
			let blk_exit = self.builder.create_block();	// target of break
			self.builder.ins().jump(blk_body, &[]);

			self.stack.push(Scope::new_loop(blk_foot, blk_exit));
			self.builder.switch_to_block(blk_body);
			// NOTE: no seal yet, reverse jumps happen.
			self.stack.push(Scope::new());
			self.handle_block(body);
			self.stack.pop();	// Body scope
			
			self.builder.ins().jump(blk_foot, &[]);
			self.builder.switch_to_block(blk_foot);
			self.builder.seal_block(blk_foot);
			self.stack.pop();	// Loop scope

			{
				let cond_v = self.handle_node(cond);
				let cond_v = self.get_value(cond_v);
				self.builder.ins().brz(cond_v, blk_exit, &[]);
			}

			self.builder.ins().jump(blk_body, &[]);
			self.builder.seal_block(blk_body);	// Seal the loop body, jumps now known.

			self.builder.switch_to_block(blk_exit);
			self.builder.seal_block(blk_exit);
			},
		Statement::ForLoop { ref init, ref cond, ref inc, ref body } => {
			if let Some(init) = init {
				self.handle_expr_def(init);
			}

			let blk_top = self.builder.create_block();	// loop back
			let blk_body = self.builder.create_block();
			let blk_foot = self.builder.create_block();	// target of continue
			let blk_exit = self.builder.create_block();	// target of break
			self.builder.ins().jump(blk_top, &[]);

			self.builder.switch_to_block(blk_top);
			// NOTE: no seal yet, reverse jumps happen.

			if let Some(cond) = cond {
				let cond_v = self.handle_node(cond);
				let cond_v = self.get_value(cond_v);
				self.builder.ins().brz(cond_v, blk_exit, &[]);
			}
			self.builder.ins().jump(blk_body, &[]);

			self.stack.push(Scope::new_loop(blk_foot, blk_exit));
			self.builder.switch_to_block(blk_body);
			self.builder.seal_block(blk_body);
			self.stack.push(Scope::new());
			self.handle_block(body);
			self.stack.pop();	// Body scope

			self.builder.ins().jump(blk_foot, &[]);
			self.builder.switch_to_block(blk_foot);
			self.builder.seal_block(blk_foot);
			self.stack.pop();	// Loop scope

			if let Some(inc) = inc {
				self.handle_node(inc);
			}

			self.builder.ins().jump(blk_top, &[]);
			self.builder.seal_block(blk_top);	// Seal the loop body, jumps now known.

			self.builder.switch_to_block(blk_exit);
			self.builder.seal_block(blk_exit);
			},

		Statement::Continue => {
			trace!("{}continue", self.indent());
			for e in self.stack.iter().rev()
			{
				if let Some(blk) = e.blk_continue {
					self.builder.ins().jump(blk, &[]);

					let blk_orphan = self.builder.create_block();
					self.builder.switch_to_block(blk_orphan);
					self.builder.seal_block(blk_orphan);
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
					self.builder.ins().jump(blk, &[]);

					let blk_orphan = self.builder.create_block();
					self.builder.switch_to_block(blk_orphan);
					self.builder.seal_block(blk_orphan);
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
				let val = self.get_value(val);
				self.builder.ins().return_(&[val]);
			}
			else
			{
				// Void return - easy
				self.builder.ins().return_(&[]);
			}
			let blk_orphan = self.builder.create_block();
			self.builder.switch_to_block(blk_orphan);
			self.builder.seal_block(blk_orphan);
			},

		_ => panic!("TODO: {:?}", stmt),
		}
	}

	fn handle_node(&mut self, node: &crate::ast::Node) -> ValueRef
	{
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
			Some(crate::ast::IdentRef::Local(idx)) => self.vars[*idx].clone(),
			Some(crate::ast::IdentRef::StaticItem) => ValueRef::Global(name.clone(), 0, ty.clone()),
			Some(crate::ast::IdentRef::Function) => {
				let context = &mut self.context;
				let builder = &mut self.builder;
				let v = self.fcn_imports.entry(name.clone())
					.or_insert_with(|| {
						let func_data = ::cranelift_codegen::ir::ExtFuncData {
							name: context.get_function(&name),
							signature: builder.import_signature(make_sig(match ty.basetype { BaseType::Function(ref f) => f, ref t => panic!("Function not function type {:?}", t), })),
							colocated: false,
							};
						builder.import_function(func_data)
						})
					.clone()
					;
				ValueRef::Function(name.clone(), v)
				},
			Some(crate::ast::IdentRef::Enum(ref enm, idx)) => {
				let val = enm.borrow().get_item_val(*idx).expect("Enum index out of range?");
				let ty = crate::types::Type::new_ref_bare(BaseType::Integer(crate::types::IntClass::Int( crate::types::Signedness::Signed )));
				ValueRef::Temporary(self.builder.ins().iconst(cvt_ty(&ty), val as i64))
				},
			}
			},
		NodeKind::Integer(val, ty) => ValueRef::Temporary(self.builder.ins().iconst(cvt_ty(&crate::types::Type::new_ref_bare(BaseType::Integer(ty))), val as i64)),
		NodeKind::Float(val, ty) => match ty.size()
			{
			4 => ValueRef::Temporary(self.builder.ins().f32const(val as f32)),
			8 => ValueRef::Temporary(self.builder.ins().f64const(val)),
			sz => panic!("NodeKind::Float sz={:?}", sz),
			},
		NodeKind::String(ref val) => {
			// Declare
			let did = self.context.module.declare_data("", ::cranelift_module::Linkage::Local, /*writeable*/false, /*align*/None)
				.expect("Failed to declare");
			// Define
			let mut data_ctx = ::cranelift_module::DataContext::new();
			data_ctx.define( (val.clone() + "\0").into_bytes().into_boxed_slice() );
			self.context.module.define_data(did, &data_ctx);
			// Get value
			let gv = self.context.module.declare_data_in_func(did, self.builder.func);
			ValueRef::Temporary(self.builder.ins().symbol_value( cr_tys::I8, gv ))
			},

		NodeKind::FcnCall(ref fcn, ref args) => {
			let fcn = self.handle_node(fcn);
			let args: Vec<_> = args.iter()
				.map(|v| {
					let v = self.handle_node(v);
					let v = self.get_value(v);
					v
					})
				.collect()
				;
			let inst = if let ValueRef::Function(ref name, fcn_ref) = fcn {
					self.builder.ins().call(fcn_ref, &args)
				}
				else {
					todo!("FcnCall {:?} {:?}", fcn, args);
				};
			let res = self.builder.inst_results(inst);
			match res
			{
			[] => ValueRef::Void,
			[v] => ValueRef::Temporary(*v),
			_ => panic!("Multiple return values from {:?}", fcn),
			}
			},

		NodeKind::Assign(ref slot, ref val) => {
			let slot = self.handle_node(slot);
			let val = self.handle_node(val);
			self.assign_value(slot.clone(), val);
			slot
			},
		NodeKind::AssignOp(ref op, ref slot, ref val) => {
			let ty = &val.meta.as_ref().unwrap().ty;
			let slot = self.handle_node(slot);
			let val = self.handle_node(val);
			let val_l = self.get_value(slot.clone());
			let val_r = self.get_value(val);
			let new_val = self.handle_binop(op, ty, val_l, val_r);
			self.assign_value(slot.clone(), new_val);
			slot
			},

		NodeKind::Cast(ref ty, ref val) => {
			let src_ty = &val.meta.as_ref().unwrap().ty;
			let val = self.handle_node(val);
			self.handle_cast(ty, val, src_ty, /*is_implicit=*/false)
			},
		NodeKind::ImplicitCast(ref ty, ref val) => {
			let src_ty = &val.meta.as_ref().unwrap().ty;
			let val = self.handle_node(val);
			self.handle_cast(ty, val, src_ty, /*is_implicit=*/false)
			},
		
		NodeKind::Ternary(ref cond, ref val_true, ref val_false) => {
			let is_lvalue = node.meta.as_ref().unwrap().is_lvalue;

			let cond_v = self.handle_node(cond);
			let cond_v = self.get_value(cond_v);
			let true_blk = self.builder.create_block();
			let else_blk = self.builder.create_block();
			let done_blk = self.builder.create_block();
			self.builder.ins().brz(cond_v, else_blk, &[]);
			self.builder.ins().jump(true_blk, &[]);

			self.builder.switch_to_block(true_blk);
			self.builder.seal_block(true_blk);
			let val_true = self.handle_node(val_true);
			let val_true = if is_lvalue {
					panic!("TODO: handle_node - Ternary (LValue) - result true {:?}", val_true);
				}
				else {
					self.get_value(val_true)
				};
			self.builder.ins().jump(done_blk, &[]);

			self.builder.switch_to_block(else_blk);
			self.builder.seal_block(else_blk);
			let val_false = self.handle_node(val_false);
			let val_false = if is_lvalue {
					panic!("TODO: handle_node - Ternary (LValue) - result true {:?}", val_false);
				}
				else {
					self.get_value(val_false)
				};
			self.builder.ins().jump(done_blk, &[]);

			self.builder.switch_to_block(done_blk);
			self.builder.seal_block(done_blk);

			// NOTE: Ternary an LValue. This needs to be handled
			if is_lvalue {
				panic!("TODO: handle_node - Ternary (LValue) - result {:?} and {:?}", val_true, val_false);
			}
			else {
				ValueRef::Temporary( self.builder.ins().select( cond_v, val_true, val_false ) )
			}
			},
		NodeKind::UniOp(ref op, ref val) => {
			let ty = &val.meta.as_ref().unwrap().ty;
			let val_in = self.handle_node(val);
			let val = self.get_value(val_in.clone());
			use crate::ast::UniOp;
			match op
			{
			UniOp::PostDec => {
				let ov = self.builder.ins().iadd_imm(val, -1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(val)
				},
			UniOp::PostInc => {
				let ov = self.builder.ins().iadd_imm(val, 1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(val)
				},
			UniOp::PreDec => {
				let ov = self.builder.ins().iadd_imm(val, -1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(ov)
				},
			UniOp::PreInc => {
				let ov = self.builder.ins().iadd_imm(val, 1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(ov)
				},
			UniOp::Deref => {
				let ity = match ty.basetype
					{
					BaseType::Pointer(ref ity) => ity.clone(),
					_ => panic!("Deref of bad type - {:?}", ty),
					};
				ValueRef::Pointer(val, 0, ity)
				},
			UniOp::Address => todo!("handle_node - UniOp Address {:?}", val),
			UniOp::Neg =>
				match ty.basetype
				{
				BaseType::Integer(_) => ValueRef::Temporary(self.builder.ins().ineg(val)),
				_ => todo!("Neg on {:?}", ty),
				},
			UniOp::BitNot =>
				match ty.basetype
				{
				//BaseType::Bool => ValueRef::Temporary(self.builder.ins().bnot(val)),
				BaseType::Integer(_) => ValueRef::Temporary(self.builder.ins().bnot(val)),
				_ => todo!("BitNot on {:?}", ty),
				},
			UniOp::LogicNot =>
				match ty.basetype
				{
				BaseType::Bool => ValueRef::Temporary(self.builder.ins().icmp_imm(cr_cc::IntCC::Equal, val, 0)),
				BaseType::Integer(_) => ValueRef::Temporary(self.builder.ins().icmp_imm(cr_cc::IntCC::Equal, val, 0)),
				BaseType::Pointer(_) => ValueRef::Temporary(self.builder.ins().icmp_imm(cr_cc::IntCC::Equal, val, 0)),
				_ => todo!("LogicNot on {:?}", ty),
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
			Some((ofs, ity)) =>
				match val
				{
				ValueRef::StackSlot(ss, bofs, _) => {
					ValueRef::StackSlot(ss, bofs + ofs, ity)
					},
				ValueRef::Pointer(bv, bofs, _) => {
					ValueRef::Pointer(bv, bofs + ofs, ity)
					},
				_ => todo!("Get field {} from {:?}: +{} {:?}", name, val, ofs, ity),
				},
			None => panic!("No field {:?} on {:?}", name, ty),
			}
			},

		_ => panic!("TODO: handle_node - {:?}", node),
		}
	}
	fn handle_expr_def(&mut self, node: &crate::ast::ExprOrDef) -> ValueRef
	{
		use crate::ast::ExprOrDef;
		match node
		{
		ExprOrDef::Expr(ref e) => self.handle_node(e),
		_ => panic!("TODO: handle_expr_def - {:?}", node),
		}
	}

	/// Common processing of cast operations (between `ImplicitCast` and `Cast`)
	fn handle_cast(&mut self, dst_ty: &TypeRef, src_val: ValueRef, src_ty: &TypeRef, is_implicit: bool) -> ValueRef
	{
		let cast_name = if is_implicit { "ImplicitCast" } else { "Cast" };
		if let BaseType::Array(..) = src_ty.basetype {
			match dst_ty.basetype
			{
			BaseType::Pointer(..) => {},
			_ => panic!("Invalid {} from {:?} to {:?}", cast_name, src_ty, dst_ty),
			}
			match src_val
			{
			ValueRef::StackSlot(ss, ofs, _ty) => {
				ValueRef::Temporary(self.builder.ins().stack_addr(cr_tys::I32, ss, ofs as i32))
				},
			ValueRef::Pointer(base, ofs, _ty) => {
				ValueRef::Temporary(self.builder.ins().iadd_imm(base, ofs as i64))
				},
			_ => todo!("{} {:?} {:?} to {:?}", cast_name, src_val, src_ty, dst_ty),
			}
		}
		else {
			let val = self.get_value(src_val);
			let src_is_signed = match src_ty.basetype
				{
				BaseType::Integer(ic) => !ic.signedness().is_unsigned(),
				_ => false,
				};
			let src_cty = cvt_ty(src_ty);
			let dst_cty = cvt_ty(dst_ty);

			ValueRef::Temporary(match (src_cty, dst_cty)
				{
				// equal - No-op
				(cr_tys::B8 , cr_tys::B8 ) => val,
				(cr_tys::I8 , cr_tys::I8 ) => val,
				(cr_tys::I16, cr_tys::I16) => val,
				(cr_tys::I32, cr_tys::I32) => val,
				(cr_tys::I64, cr_tys::I64) => val,
				// bool -> * : uextend
				(cr_tys::B8, cr_tys::I8) => val,
				(cr_tys::B8, cr_tys::I16)
				| (cr_tys::B8, cr_tys::I32)
				| (cr_tys::B8, cr_tys::I64)
					=> self.builder.ins().uextend(dst_cty, val),
				// * -> bool : non-zero
				(cr_tys::I8, cr_tys::B8)
				| (cr_tys::I16, cr_tys::B8)
				| (cr_tys::I32, cr_tys::B8)
				| (cr_tys::I64, cr_tys::B8)
					=> self.builder.ins().icmp_imm(cr_cc::IntCC::NotEqual, val, 0),
				// (u)int -> (u)int : ireduce/uextend/sextend
				// - Reduction
				(cr_tys::I16, cr_tys::I8)
				| (cr_tys::I32, cr_tys::I8)
				| (cr_tys::I32, cr_tys::I16)
				| (cr_tys::I64, cr_tys::I8)
				| (cr_tys::I64, cr_tys::I16)
				| (cr_tys::I64, cr_tys::I32)
					=> self.builder.ins().ireduce(dst_cty, val),
				// - Extension (signedness matters)
				(cr_tys::I8, cr_tys::I16, )
				| (cr_tys::I8 , cr_tys::I32, )
				| (cr_tys::I16, cr_tys::I32, )
				| (cr_tys::I8 , cr_tys::I64, )
				| (cr_tys::I16, cr_tys::I64, )
				| (cr_tys::I32, cr_tys::I64, )
					=> if src_is_signed {
						self.builder.ins().sextend(dst_cty, val)
					}
					else {
						self.builder.ins().uextend(dst_cty, val)
					},
				// float -> float : fdemote/fpromote
				// (u)int -> float : 
				// float -> (u)int : fcvt_to_[us]int(_sat)
				_ => todo!("{} {:?} {:?} to {:?} ({:?} to {:?})", cast_name, val, src_cty, dst_cty, src_ty, dst_ty),
				})
		}
	}

	/// Common processing of binary operations (for both `BinOp` and `AssignOp`)
	fn handle_binop(&mut self, op: &crate::ast::BinOp, ty_l: &TypeRef, val_l: cr_e::Value, val_r: cr_e::Value) -> ValueRef
	{
		enum TyC {
			Float,
			Unsigned,
			Signed,
		}
		let ty = match ty_l.basetype
			{
			BaseType::Float(_) => TyC::Float,
			BaseType::Pointer(_) => TyC::Unsigned,
			BaseType::Integer(ref ic) => match ic.signedness()
				{
				crate::types::Signedness::Unsigned => TyC::Unsigned,
				crate::types::Signedness::Signed => TyC::Signed,
				},
			BaseType::Bool => return ValueRef::Temporary(match op
				{
				BinOp::LogicOr => self.builder.ins().bor(val_l, val_r),
				BinOp::LogicAnd => self.builder.ins().band(val_l, val_r),
				_ => panic!("TODO: handle_node - BinOp Bool {:?}", op),
				}),
			_ => panic!("Invalid type for bin-op: {:?}", ty_l),
			};

		use crate::ast::BinOp;
		ValueRef::Temporary(match ty
			{
			TyC::Signed => match op
				{
				BinOp::CmpLt => self.builder.ins().icmp(cr_cc::IntCC::SignedLessThan, val_l, val_r),
				BinOp::CmpGt => self.builder.ins().icmp(cr_cc::IntCC::SignedGreaterThan, val_l, val_r),
				BinOp::CmpLtE => self.builder.ins().icmp(cr_cc::IntCC::SignedLessThanOrEqual, val_l, val_r),
				BinOp::CmpGtE => self.builder.ins().icmp(cr_cc::IntCC::SignedGreaterThanOrEqual, val_l, val_r),
				BinOp::CmpEqu => self.builder.ins().icmp(cr_cc::IntCC::Equal, val_l, val_r),
				BinOp::CmpNEqu => self.builder.ins().icmp(cr_cc::IntCC::NotEqual, val_l, val_r),

				BinOp::Add => self.builder.ins().iadd(val_l, val_r),
				BinOp::Sub => self.builder.ins().isub(val_l, val_r),
				BinOp::Mul => self.builder.ins().imul(val_l, val_r),
				BinOp::Div => self.builder.ins().sdiv(val_l, val_r),
				BinOp::Mod => self.builder.ins().srem(val_l, val_r),

				BinOp::ShiftLeft => self.builder.ins().ishl(val_l, val_r),
				BinOp::ShiftRight => self.builder.ins().sshr(val_l, val_r),

				_ => panic!("TODO: handle_node - BinOp Signed - {:?}", op),
				},
			TyC::Unsigned => match op
				{
				BinOp::CmpLt => self.builder.ins().icmp(cr_cc::IntCC::UnsignedLessThan, val_l, val_r),
				BinOp::CmpGt => self.builder.ins().icmp(cr_cc::IntCC::UnsignedGreaterThan, val_l, val_r),
				BinOp::CmpLtE => self.builder.ins().icmp(cr_cc::IntCC::UnsignedLessThanOrEqual, val_l, val_r),
				BinOp::CmpGtE => self.builder.ins().icmp(cr_cc::IntCC::UnsignedGreaterThanOrEqual, val_l, val_r),
				BinOp::CmpEqu => self.builder.ins().icmp(cr_cc::IntCC::Equal, val_l, val_r),
				BinOp::CmpNEqu => self.builder.ins().icmp(cr_cc::IntCC::NotEqual, val_l, val_r),

				BinOp::Add => self.builder.ins().iadd(val_l, val_r),
				BinOp::Sub => self.builder.ins().isub(val_l, val_r),
				BinOp::Mul => self.builder.ins().imul(val_l, val_r),
				BinOp::Div => self.builder.ins().udiv(val_l, val_r),
				BinOp::Mod => self.builder.ins().urem(val_l, val_r),

				BinOp::ShiftLeft => self.builder.ins().ishl(val_l, val_r),
				BinOp::ShiftRight => self.builder.ins().ushr(val_l, val_r),

				BinOp::BitAnd => self.builder.ins().band(val_l, val_r),
				BinOp::BitOr => self.builder.ins().bor(val_l, val_r),
				_ => panic!("TODO: handle_node - BinOp Unsigned - {:?}", op),
				},
			TyC::Float => match op
				{
				BinOp::CmpLt => self.builder.ins().fcmp(cr_cc::FloatCC::LessThan, val_l, val_r),
				BinOp::CmpGt => self.builder.ins().fcmp(cr_cc::FloatCC::GreaterThan, val_l, val_r),
				_ => panic!("TODO: handle_node - BinOp Float - {:?}", op),
				},
			})
	}

	fn define_var(&mut self, var_def: &crate::ast::VariableDefinition)
	{
		use crate::ast::Initialiser;
		let idx = var_def.index.unwrap();
		let slot = self.vars[idx].clone();
		match var_def.value
		{
		Initialiser::None => {},
		Initialiser::Value(ref node) => {
			let v = self.handle_node(node);
			self.assign_value(slot, v)
			},
		_ => todo!("${} = {:?}", idx, var_def.value),
		}
	}

	fn get_value(&mut self, vr: ValueRef) -> cr_e::Value {
		match vr
		{
		ValueRef::Temporary(val) => val,
		ValueRef::Variable(var) => self.builder.use_var(var),
		ValueRef::StackSlot(ref ss, ofs, ref ty) => self.builder.ins().stack_load(cvt_ty(ty), *ss, ofs as i32),
		ValueRef::Pointer(ref pv, ofs, ref ty) => self.builder.ins().load(cvt_ty(ty), ::cranelift_codegen::ir::MemFlags::new(), *pv, ofs as i32),
		_ => panic!("TODO: get_value {:?}", vr),
		}
	}
	fn assign_value(&mut self, slot: ValueRef, val: ValueRef)
	{
		match slot
		{
		ValueRef::Variable(ref var) => {
			let val = self.get_value(val);
			self.builder.def_var(*var, val);
			},
		ValueRef::StackSlot(ref ss, ofs, ref _ty) =>
			match val
			{
			ValueRef::Variable(..)
			| ValueRef::Temporary(..)
				=> {
					let val = self.get_value(val);
					self.builder.ins().stack_store(val,  *ss, ofs as i32);
				},
			_ => todo!("{:?} = {:?}", slot, val),
			},
		ValueRef::Pointer(base, ofs, ref _ty) =>
			match val
			{
			ValueRef::Variable(..)
			| ValueRef::Temporary(..)
				=> {
					let val = self.get_value(val);
					self.builder.ins().store( ::cranelift_codegen::ir::MemFlags::new(), val,  base, ofs as i32 );
				},
			ValueRef::StackSlot(.., ref ty)
			| ValueRef::Pointer(.., ref ty)
			if cvt_ty_opt(ty).is_some()
				=> {
					let val = self.get_value(val);
					self.builder.ins().store( ::cranelift_codegen::ir::MemFlags::new(), val,  base, ofs as i32 );
				},
			_ => todo!("{:?} = {:?}", slot, val),
			},
		_ => todo!("{:?} = {:?}", slot, val),
		}
	}
}
impl Scope
{
	fn new() -> Self {
		Scope {
			blk_break: None,
			blk_continue: None,
			}
	}
	fn new_loop(blk_break: cr_e::Block, blk_continue: cr_e::Block) -> Self {
		Scope {
			blk_break: Some(blk_break),
			blk_continue: Some(blk_continue),
			}
	}
}

fn cvt_ty(ty: &TypeRef) -> cr_tys::Type
{
	match cvt_ty_opt(ty)
	{
	Some(t) => t,
	None => panic!("{:?} isn't valid as a cranelift type", ty),
	}
}
fn cvt_ty_opt(ty: &TypeRef) -> Option<cr_tys::Type>
{
	Some(match ty.basetype
	{
	BaseType::Void => panic!("Attempting to convert `void` to a cranelift type"),
	BaseType::Bool => cr_tys::B8,
	//BaseType::Pointer(..) => cr_tys::R32,
	BaseType::Pointer(..) => cr_tys::I32,
	BaseType::Integer(ic) => match ty.get_size().unwrap()
		{
		8 => cr_tys::I64,
		4 => cr_tys::I32,
		2 => cr_tys::I16,
		1 => cr_tys::I8,
		sz => todo!("Convert integer {:?} ({}) to cranelift", ic, sz),
		},
	BaseType::Struct(_) => return None,
	_ => todo!("Convert {:?} to cranelift", ty),
	})
}

fn make_sig(ty: &crate::types::FunctionType) -> ::cranelift_codegen::ir::Signature
{
	use cranelift_codegen::isa::CallConv;
	use cranelift_codegen::ir::{AbiParam, Signature};

	let mut sig = Signature::new(CallConv::SystemV);
	// - Return
	if let BaseType::Void = ty.ret.basetype {
	}
	else {
		sig.returns.push( AbiParam::new(cvt_ty(&ty.ret)) );
	}
	// - Arguments
	sig.params.reserve( ty.args.len() );
	for (arg_ty, _arg_name) in &ty.args {
		sig.params.push( AbiParam::new(cvt_ty(&arg_ty)) );
	}
	if ty.is_variadic {
		// TODO: Encode variadic types (cranelif can't do that yet)
	}
	sig
}
