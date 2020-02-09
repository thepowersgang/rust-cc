
use cranelift_codegen::ir::entities as cr_e;
use cranelift_codegen::ir::condcodes as cr_cc;
use cranelift_codegen::ir::types as cr_tys;
use cranelift_codegen::ir::InstBuilder;
use crate::types::TypeRef;
use std::collections::hash_map::HashMap;

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

	pub fn lower_function(&mut self, name: &str, ty: &crate::types::FunctionType, code: &crate::ast::Block)
	{
		debug!("lower_function({}: {:?})", name, ty);
		use cranelift_codegen::entity::EntityRef;	// provides ::new on Variable
		use cranelift_codegen::isa::CallConv;
		use cranelift_codegen::ir::{AbiParam, ExternalName, Function, Signature};
		use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

		let mut outer_scope = Scope::new();
		// 1. Signature
		let mut sig = Signature::new(CallConv::SystemV);
		// - Return
		sig.returns.push(AbiParam::new(cr_tys::I32));
		// - Arguments
		for (idx,(arg_ty, arg_name)) in Iterator::enumerate(ty.args.iter()) {
			let var = ::cranelift_frontend::Variable::new(idx);
			sig.params.push( AbiParam::new(cr_tys::I32) );
			outer_scope.locals.insert( arg_name.clone(), ValueRef::Variable(var) );
		}

		let mut fn_builder_ctx = FunctionBuilderContext::new();
		let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
		let mut b = Builder {
			builder: FunctionBuilder::new(&mut func, &mut fn_builder_ctx),
			stack: Vec::new(),
			};
		b.stack.push(outer_scope);

		// Block 0 - Sets up arguments
		let block0 = b.builder.create_block();
		b.builder.append_block_params_for_function_params(block0);
		b.builder.switch_to_block(block0);
		b.builder.seal_block(block0);
		for (idx,(arg_ty, arg_name)) in Iterator::enumerate(ty.args.iter())
		{
			match b.stack[0].locals[arg_name]
			{
			ValueRef::Variable(ref v) => {
				b.builder.declare_var(*v, cr_tys::I32);
				let tmp = b.builder.block_params(block0)[idx];
				b.builder.def_var(*v, tmp);
				},
			ref e => panic!("TODO: Arg class - {:?}", e),
			}
		}
		
		b.handle_block(code);

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
}
struct Scope
{
	locals: HashMap<String, ValueRef>,
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
			for var_def in list
			{
				self.define_var(var_def);
			}
			},
		Statement::Expr(ref e) => {
			let _v = self.handle_node(e);
			},
		Statement::Block(ref stmts) => {
			self.handle_block(stmts);
			},
		Statement::IfStatement { ref cond, ref true_arm, ref else_arm } => {
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
			self.stack.pop();
			self.builder.ins().jump(blk_top, &[]);
			self.stack.pop();	// Loop scope
			self.builder.seal_block(blk_top);	// Seal the loop body, jumps now known.

			self.builder.switch_to_block(blk_exit);
			self.builder.seal_block(blk_exit);
			},

		// DoWhileLoop
		// For

		Statement::Continue => {
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
			if let Some(val) = opt_val
			{
				let val = self.handle_node(val);
				panic!("TODO: Convert value reference into a return value");
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
			for s in &self.stack
			{
				if let Some(v) = s.locals.get(name)
				{
					return v.clone();
				}
			}
			panic!("TODO: Ident")
			},
		NodeKind::Integer(val, ty) => ValueRef::Temporary(self.builder.ins().iconst(cr_tys::I32, val as i64)),
		
		NodeKind::Ternary(ref cond, ref val_true, ref val_false) => {
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
			// TODO: Define a position for the result based on the type of `val_true`
			self.builder.ins().jump(done_blk, &[]);

			self.builder.switch_to_block(else_blk);
			self.builder.seal_block(else_blk);
			let val_false = self.handle_node(val_false);
			// TODO: Save the value
			self.builder.ins().jump(done_blk, &[]);

			self.builder.switch_to_block(done_blk);
			self.builder.seal_block(done_blk);

			// NOTE: Ternary an LValue. This needs to be handled
			panic!("TODO: handle_node - Ternary - result {:?} and {:?}", val_true, val_false);
			},
		NodeKind::UniOp(ref op, ref val) => panic!("TODO: handle_node - UniOp"),
		NodeKind::BinOp(ref op, ref val_l, ref val_r) => {
			let val_l = self.handle_node(val_l);
			let val_r = self.handle_node(val_r);
			let val_l = self.get_value(val_l);
			let val_r = self.get_value(val_r);

			use crate::ast::BinOp;
			ValueRef::Temporary(match op
				{
				BinOp::CmpLt => self.builder.ins().icmp(cr_cc::IntCC::SignedLessThan, val_l, val_r),
				_ => panic!("TODO: handle_node - BinOp"),
				})
			},

		_ => panic!("TODO: handle_node - {:?}", node),
		}
	}
	fn handle_expr_def(&mut self, node: &crate::ast::ExprOrDef) -> ValueRef
	{
		//use crate::ast::Node;
		panic!("TODO: handle_node");
	}

	fn define_var(&mut self, var_def: &crate::ast::VariableDefinition)
	{
	}

	fn get_value(&mut self, vr: ValueRef) -> cr_e::Value {
		match vr
		{
		ValueRef::Temporary(val) => val,
		ValueRef::Variable(var) => self.builder.use_var(var),
		_ => panic!("TODO: get_value {:?}", vr),
		}
	}
}
impl Scope
{
	fn new() -> Self {
		Scope {
			locals: Default::default(),
			blk_break: None,
			blk_continue: None,
			}
	}
	fn new_loop(blk_break: cr_e::Block, blk_continue: cr_e::Block) -> Self {
		Scope {
			locals: Default::default(),
			blk_break: Some(blk_break),
			blk_continue: Some(blk_continue),
			}
	}
}
