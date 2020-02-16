
use cranelift_codegen::ir::entities as cr_e;
use cranelift_codegen::ir::condcodes as cr_cc;
use cranelift_codegen::ir::types as cr_tys;
use cranelift_codegen::ir::InstBuilder;
use crate::types::{TypeRef,BaseType};

pub struct Context
{
}
impl Context
{
	pub fn new() -> Self
	{
		Context {
			}
	}

	pub fn lower_function(&mut self, name: &str, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		debug!("lower_function({}: {:?})", name, ty);
		use cranelift_codegen::entity::EntityRef;	// provides ::new on Variable
		use cranelift_codegen::isa::CallConv;
		use cranelift_codegen::ir::{AbiParam, ExternalName, Function, Signature};
		use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

		// 1. Signature
		let mut sig = Signature::new(CallConv::SystemV);
		// - Return
		if let BaseType::Void = ty.ret.basetype {
		}
		else {
			sig.returns.push(AbiParam::new(cvt_ty(&ty.ret)));
		}
		// - Arguments
		let mut vars = Vec::<ValueRef>::new();
		for (idx,(arg_ty, arg_name)) in Iterator::enumerate(ty.args.iter()) {
			let var = ::cranelift_frontend::Variable::new(idx);
			sig.params.push( AbiParam::new(cvt_ty(&arg_ty)) );
			vars.push( ValueRef::Variable(var) );
		}

		let mut fn_builder_ctx = FunctionBuilderContext::new();
		let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
		let mut b = Builder {
			builder: FunctionBuilder::new(&mut func, &mut fn_builder_ctx),
			stack: vec![ Scope::new() ],
			vars: vars,
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
		for (idx,(arg_ty, arg_name)) in Iterator::enumerate(ty.args.iter())
		{
			match b.vars[idx]
			{
			ValueRef::Variable(ref v) => {
				b.builder.declare_var(*v, cr_tys::I32);
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
	Temporary(cr_e::Value),
	Variable(::cranelift_frontend::Variable),
	StackSlot(cr_e::StackSlot, u32, TypeRef),
	// pointer, offset, type
	Pointer(cr_e::Value, u32, TypeRef),
}

struct Builder<'a>
{
	builder: ::cranelift_frontend::FunctionBuilder<'a>,
	stack: Vec<Scope>,
	vars: Vec<ValueRef>,
}
struct Scope
{
	blk_break: Option<cr_e::Block>,
	blk_continue: Option<cr_e::Block>,
}

impl Builder<'_>
{
	fn handle_block(&mut self, stmts: &crate::ast::StatementList)
	{
		self.stack.push(Scope::new());
		for stmt in stmts {
			self.handle_stmt(stmt);
		}
		self.stack.pop();
	}
	fn handle_stmt(&mut self, stmt: &crate::ast::Statement)
	{
		use crate::ast::Statement;
		match *stmt
		{
		Statement::Empty => {},
		Statement::VarDef(ref list) => {
			trace!("{:?}", stmt);
			for var_def in list
			{
				self.define_var(var_def);
			}
			},
		Statement::Expr(ref e) => {
			trace!("{:?}", stmt);
			let _v = self.handle_node(e);
			},
		Statement::Block(ref stmts) => {
			self.handle_block(stmts);
			},
		Statement::IfStatement { ref cond, ref true_arm, ref else_arm } => {
			trace!("if {:?}", cond);
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
			trace!("while {:?}", cond);
			let blk_top = self.builder.create_block();
			let blk_exit = self.builder.create_block();
			self.builder.ins().jump(blk_top, &[]);

			self.builder.switch_to_block(blk_top);
			// NOTE: no seal yet, reverse jumps happen.

			self.stack.push(Scope::new_loop(blk_top, blk_exit));
			let cond_v = self.handle_expr_def(cond);
			let cond_v = self.get_value(cond_v);
			self.builder.ins().brz(cond_v, blk_exit, &[]);

			self.stack.push(Scope::new());
			for stmt in body {
				self.handle_stmt(stmt);
			}
			self.stack.pop();	// Body scope

			self.builder.ins().jump(blk_top, &[]);
			self.stack.pop();	// Loop scope
			self.builder.seal_block(blk_top);	// Seal the loop body, jumps now known.

			self.builder.switch_to_block(blk_exit);
			self.builder.seal_block(blk_exit);
			},

		// DoWhileLoop
		// For

		Statement::Continue => {
			trace!("continue");
			for e in self.stack.iter().rev()
			{
				if let Some(blk) = e.blk_continue {
					self.builder.ins().jump(blk, &[]);
					// TODO: Close the current block?
					return ;
				}
			}
			panic!("Continue without a loop");
			},
		Statement::Break => {
			trace!("break");
			for e in self.stack.iter().rev()
			{
				if let Some(blk) = e.blk_break {
					self.builder.ins().jump(blk, &[]);
					// TODO: Close the current block?
					return ;
				}
			}
			panic!("Break without a loop");
			},
		Statement::Return(ref opt_val) => {
			trace!("return {:?}", opt_val);
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
			match binding
			{
			Some(crate::ast::IdentRef::Local(idx)) => self.vars[*idx].clone(),
			_ => panic!("TODO: Ident {:?} {:?}", name, binding)
			}
			},
		NodeKind::Integer(val, ty) => ValueRef::Temporary(self.builder.ins().iconst(cr_tys::I32, val as i64)),

		NodeKind::Assign(ref slot, ref val) => {
			let slot = self.handle_node(slot);
			let val = self.handle_node(val);
			self.assign_value(slot.clone(), val);
			slot
			},

		NodeKind::ImplicitCast(ref ty, ref val) => {
			let src_ty = &val.meta.as_ref().unwrap().ty;
			let val = self.handle_node(val);
			if let BaseType::Array(..) = src_ty.basetype {
				match ty.basetype
				{
				BaseType::Pointer(..) => {},
				_ => panic!("Invalid ImplicitCast from {:?} to {:?}", src_ty, ty),
				}
				match val
				{
				ValueRef::StackSlot(ss, ofs, _ty) => {
					ValueRef::Temporary(self.builder.ins().stack_addr(cr_tys::I32, ss, ofs as i32))
					},
				ValueRef::Pointer(base, ofs, _ty) => {
					ValueRef::Temporary(self.builder.ins().iadd_imm(base, ofs as i64))
					},
				_ => todo!("ImplicitCast {:?} {:?} to {:?}", val, src_ty, ty),
				}
			}
			else {
				let val = self.get_value(val);
				let src_cty = cvt_ty(src_ty);
				let dst_cty = cvt_ty(ty);

				ValueRef::Temporary(match (src_cty, dst_cty)
					{
					// equal - No-op
					(cr_tys::I8 , cr_tys::I8 ) => val,
					(cr_tys::I16, cr_tys::I16) => val,
					(cr_tys::I32, cr_tys::I32) => val,
					(cr_tys::I64, cr_tys::I64) => val,
					// (u)int -> (u)int : ireduce/uextend/sextend
					(cr_tys::I16, cr_tys::I8)
					| (cr_tys::I32, cr_tys::I8)
					| (cr_tys::I32, cr_tys::I16)
					| (cr_tys::I64, cr_tys::I8)
					| (cr_tys::I64, cr_tys::I16)
					| (cr_tys::I64, cr_tys::I32)
						=> self.builder.ins().ireduce(dst_cty, val),
					// (u)int -> float : ireduce/uextend/sextend
					// float -> float : fdemote/fpromote
					// float -> (u)int : fcvt_to_[us]int(_sat)
					_ => todo!("ImplicitCast {:?} {:?} to {:?} ({:?} to {:?})", val, src_cty, dst_cty, src_ty, ty),
					})
			}
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
			_ => panic!("TODO: handle_node - UniOp - {:?} {:?} {:?}", op, ty, val),
			}
			},
		NodeKind::BinOp(ref op, ref val_l, ref val_r) => {
			let ty_l = &val_l.meta.as_ref().unwrap().ty;
			let val_l = self.handle_node(val_l);
			let val_r = self.handle_node(val_r);
			let val_l = self.get_value(val_l);
			let val_r = self.get_value(val_r);

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
				_ => panic!("Invalid type for bin-op: {:?}", ty_l),
				};

			use crate::ast::BinOp;
			ValueRef::Temporary(match ty
				{
				TyC::Signed => match op
					{
					BinOp::CmpLt => self.builder.ins().icmp(cr_cc::IntCC::SignedLessThan, val_l, val_r),
					BinOp::CmpGt => self.builder.ins().icmp(cr_cc::IntCC::SignedGreaterThan, val_l, val_r),
					BinOp::Div => self.builder.ins().sdiv(val_l, val_r),
					_ => panic!("TODO: handle_node - BinOp Signed - {:?}", op),
					},
				TyC::Unsigned => match op
					{
					BinOp::CmpLt => self.builder.ins().icmp(cr_cc::IntCC::UnsignedLessThan, val_l, val_r),
					BinOp::CmpGt => self.builder.ins().icmp(cr_cc::IntCC::UnsignedGreaterThan, val_l, val_r),
					BinOp::Add => self.builder.ins().iadd(val_l, val_r),
					_ => panic!("TODO: handle_node - BinOp Unsigned - {:?}", op),
					},
				TyC::Float => match op
					{
					BinOp::CmpLt => self.builder.ins().fcmp(cr_cc::FloatCC::LessThan, val_l, val_r),
					BinOp::CmpGt => self.builder.ins().fcmp(cr_cc::FloatCC::GreaterThan, val_l, val_r),
					_ => panic!("TODO: handle_node - BinOp Float - {:?}", op),
					},
				})
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
		_ => panic!("TODO: get_value {:?}", vr),
		}
	}
	fn assign_value(&mut self, slot: ValueRef, val: ValueRef)
	{
		match slot
		{
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
	use crate::types::{IntClass, Signedness};
	match ty.basetype
	{
	BaseType::Void => panic!("Attempting to convert `void` to a cranelift type"),
	BaseType::Bool => cr_tys::B8,
	//BaseType::Pointer(..) => cr_tys::R32,
	BaseType::Pointer(..) => cr_tys::I32,
	BaseType::Integer(ic) => match ty.get_size().unwrap()
		{
		4 => cr_tys::I32,
		2 => cr_tys::I16,
		sz => todo!("Convert integer {:?} ({}) to cranelift", ic, sz),
		},
	_ => todo!("Convert {:?} to cranelift", ty),
	}
}
