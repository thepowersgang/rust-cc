//!
use std::collections::HashMap;
use cranelift_codegen::ir::entities as cr_e;
use cranelift_codegen::ir::condcodes as cr_cc;
use cranelift_codegen::ir::types as cr_tys;
use cranelift_codegen::ir::InstBuilder;
use cranelift_module::Linkage;
use cranelift_module::Module;

use crate::types::{TypeRef,BaseType};
use crate::ast::Ident;
use crate::ast::Initialiser;

extern crate target_lexicon;

pub struct Context
{
	module: ::cranelift_object::ObjectModule,
	functions: HashMap<Ident, FunctionRecord>,
	globals: HashMap<Ident, ::cranelift_module::DataId>,
	string_count: usize,
}
struct FunctionRecord
{
	name: ::cranelift_codegen::ir::UserExternalNameRef,
	id: ::cranelift_module::FuncId,
	sig: ::cranelift_codegen::ir::Signature,
}
impl Context
{
	pub fn new() -> Self
	{
		let isa = {
			let shared_builder = ::cranelift_codegen::settings::builder();
			let shared_flags = ::cranelift_codegen::settings::Flags::new(shared_builder);
			let b = ::cranelift_codegen::isa::lookup_by_name("x86_64-elf").unwrap();
			b.finish(shared_flags).expect("Failed to create TargetIsa")
			};
		Context {
			module: ::cranelift_object::ObjectModule::new(::cranelift_object::ObjectBuilder::new(
				isa,
				b"unknown_object.o"[..].to_owned(),
				::cranelift_module::default_libcall_names(),
				).expect("Can't create object builder")),
			functions: Default::default(),
			globals: Default::default(),
			string_count: 0,
			}
	}

	pub fn finish(self, mut sink: impl std::io::Write) -> Result<(), Box<dyn std::error::Error>>
	{
		let bytes = self.module.finish().emit()?;
		sink.write_all(&bytes)?;
		Ok( () )
	}

	fn get_function(&mut self, name: &Ident, ty: &crate::types::FunctionType) -> &FunctionRecord
	{
		let idx = self.functions.len();
		let module = &mut self.module;
		self.functions.entry(name.clone())
			.or_insert_with(|| {
				let sig = make_sig(ty);
				FunctionRecord {
					name: ::cranelift_codegen::ir::entities::UserExternalNameRef::from_u32(idx as u32),
					id: module.declare_function(&name, Linkage::Export, &sig).expect("get_function"),
					sig: sig,
					}
				})
	}
	fn create_string(&mut self, val: Vec<u8>) -> ::cranelift_module::DataId
	{
		let string_name = format!("str#{}", self.string_count);
		self.string_count += 1;
		// Declare
		let did = self.module.declare_data(&string_name, Linkage::Local, /*writeable*/false, /*tls*/false)
			.expect("Failed to declare");
		// Define
		let mut data_ctx = ::cranelift_module::DataDescription::new();
		data_ctx.define({ let mut val = val; val.push(0); val.into_boxed_slice() });
		self.module.define_data(did, &data_ctx).expect("create_string - define_data");
		did
	}

	pub fn declare_value(&mut self, _name: &crate::ast::Ident, _ty: &crate::types::TypeRef)
	{
	}
	pub fn lower_value(&mut self, name: &crate::ast::Ident, ty: &crate::types::TypeRef, val: &crate::ast::Initialiser)
	{
		debug!("lower_value({}: {:?} = {:?})", name, ty, val);

		let did = match self.module.declare_data(name.as_str(), Linkage::Export, /*writable*/!ty.qualifiers.is_const(), /*tls*/false)
			{
			Ok(did) => did,
			Err(e) => panic!("lower_value: {:?} - Error {:?}", name, e),
			};
		self.globals.insert(name.clone(), did);

		let size = ty.get_size().expect("Global with unknown size") as usize;
		let mut data_ctx = ::cranelift_module::DataDescription::new();
		if let Initialiser::None = val {
			data_ctx.define_zeroinit( size );
		}
		else {
			let mut data = vec![0; size].into_boxed_slice();
			self.init_data_ctx(&mut data_ctx, &mut data, 0, ty, val);
			data_ctx.define(data);
		}
		self.module.define_data(did, &data_ctx).expect("lower_value - define_data");
	}

	fn init_data_ctx(&mut self, data_ctx: &mut ::cranelift_module::DataDescription, data: &mut [u8], offset: usize, ty: &TypeRef, init: &Initialiser)
	{
		match init
		{
		Initialiser::None => {
			},
		Initialiser::ListLiteral(ref vals) =>
			match ty.basetype
			{
			BaseType::Array(ref inner_ty, _) => {
				let inner_size = inner_ty.get_size().unwrap() as usize;
				for (ofs, val) in Iterator::zip( (0 .. ).map(|i| i * inner_size), vals.iter() )
				{
					self.init_data_ctx_node(data_ctx, data, offset + ofs, inner_ty, val);
				}
				},
			BaseType::Struct(ref s) => {
				for (val, (ofs, _name, inner_ty)) in Iterator::zip( vals.iter(), s.borrow().iter_fields() )
				{
					self.init_data_ctx_node(data_ctx, data, offset + ofs as usize, inner_ty, val);
				}
				},
			_ => todo!("init_data_ctx: ListLiteral with {:?}", ty),
			},
		Initialiser::Value(ref val) => self.init_data_ctx_node(data_ctx, data, offset, ty, val),
		_ => todo!("init_data_ctx: init={:?}", init),
		}
	}
	fn init_data_ctx_node(&mut self, data_ctx: &mut ::cranelift_module::DataDescription, data: &mut [u8], offset: usize, ty: &TypeRef, val: &crate::ast::Node)
	{
		use crate::ast::NodeKind;
		match val.kind
		{
		NodeKind::String(ref val) => {
			let did = self.create_string(val.clone().into_bytes());
			let gv = self.module.declare_data_in_data(did, data_ctx);
			data_ctx.write_data_addr(offset as u32, gv, /*addend*/0);	// TODO: What controls the size of this write?
			},
		// TODO: Const eval into float/integer, require actually const
		_ => match val.const_eval_req()
			{
			crate::ast::ConstVal::Integer(v) =>
				match cvt_ty(ty)
				{
				cr_tys::I32 => {
					let p = &mut data[offset..][..4];
					p[0] = (v >>  0 & 0xFF) as u8;
					p[1] = (v >>  8 & 0xFF) as u8;
					p[2] = (v >> 16 & 0xFF) as u8;
					p[3] = (v >> 24 & 0xFF) as u8;
					},
				cty => todo!("init_data_ctx_node: v={:?} cty={:?}", v, cty),
				},
			crate::ast::ConstVal::Address(ref name, _ofs) =>
				match cvt_ty(ty)
				{
				CRTY_PTR => {
					if let Some(fr) = self.functions.get(name) {
						let name = cranelift_module::ModuleExtName::User { namespace: 0, index: fr.name.as_u32() };
						let fcn = data_ctx.import_function(name);
						data_ctx.write_function_addr(offset as u32, fcn);
					}
					else {
						todo!("init_data_ctx_node: &{:?} crty=PTR", name);
					}
					},
				cty => todo!("init_data_ctx_node: &{:?} cty={:?}", name, cty),
				}
			v => todo!("init_data_ctx_node: v={:?}", v),
			},
		}
	}

	pub fn declare_function(&mut self, name: &crate::ast::Ident, ty: &crate::types::FunctionType)
	{
		self.get_function(name, ty);
	}
	pub fn lower_function(&mut self, name: &Ident, ty: &crate::types::FunctionType, body: &crate::ast::FunctionBody)
	{
		debug!("lower_function({}: {:?})", name, ty);
		use cranelift_codegen::entity::EntityRef;	// provides ::new on Variable
		use cranelift_codegen::ir::Function;
		use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

		// - Arguments
		let mut fn_builder_ctx = FunctionBuilderContext::new();
		let mut func = {
			let fr = self.get_function(name, ty);
			let name = cranelift_codegen::ir::UserExternalName { namespace: 0, index: fr.name.as_u32() };
			Function::with_name_signature(cranelift_codegen::ir::UserFuncName::User(name), fr.sig.clone())
			};
		let mut b = Builder {
			context: self,
			builder: FunctionBuilder::new(&mut func, &mut fn_builder_ctx),
			stack: vec![ Scope::new() ],
			vars: vec![],
			fcn_imports: Default::default(),
			global_imports: Default::default(),

			labels: Default::default(),
			missed_labels: Default::default(),
			};

		// Define variables
		for (idx,(_arg_ty, _arg_name)) in Iterator::enumerate(ty.args.iter()) {
			let var = ::cranelift_frontend::Variable::new(idx);
			b.vars.push( ValueRef::Variable(var) );
		}
		for var in body.var_table.iter().skip( ty.args.len() )
		{
			use ::cranelift_codegen::ir::stackslot as ss;
			let size = match var.ty.get_size()
				{
				Some(s) => s,
				None => panic!("Type {:?} has no size", var.ty),
				};
			let slot = b.builder.create_sized_stack_slot(ss::StackSlotData::new(ss::StackSlotKind::ExplicitSlot, size));
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
		if /* !b.builder.is_filled() && */*ty.ret == (crate::types::Type { qualifiers: crate::types::Qualifiers::new(), basetype: BaseType::Void}) {
			b.builder.ins().return_(&[]);
		}

		for (_lbl, blk) in b.labels {
			b.builder.seal_block(blk);
		}
		for (lbl, _blk) in b.missed_labels {
			panic!("TODO: Error for missing label {:?}", lbl);
		}

		//debug!("{}", b.builder.display(None));

		b.builder.finalize();

		let mut c = ::cranelift_codegen::Context::new();
		c.func = func;
		//c.compile().expect("Unable to compile?");
		let func_id = self.get_function(name, ty).id;
		match self.module.define_function(func_id, &mut c)
		{
		Ok(_) => {},
		Err(::cranelift_module::ModuleError::Compilation(e)) => match e
			{
			::cranelift_codegen::CodegenError::Verifier(errors) => {
				panic!("Failed to define function (verifier errors):\n{}", errors);
				},
			e => panic!("Failed to define function code: {:?}", e),
			},
		Err(e) => panic!("Failed to define function code: {:?}", e),
		}
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
	///// A pointer to an item on the stack
	//StackSlotAddr(cr_e::StackSlot, u32, TypeRef),
	/// A value in global/static storage
	Global(crate::ast::Ident, u32, TypeRef),
	///// A pointer to an item in global/static storage
	//GlobalAddr(crate::ast::Ident, u32, TypeRef),
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
	global_imports: HashMap<Ident, cr_e::GlobalValue>,

	labels: HashMap<Ident, cr_e::Block>,
	missed_labels: HashMap<Ident, cr_e::Block>,
}
struct Scope
{
	blk_break: Option<cr_e::Block>,
	blk_continue: Option<cr_e::Block>,
	switch: Option<SwitchScope>,
}
#[derive(Default)]
struct SwitchScope
{
	case_default: Option<cr_e::Block>,
	case_labels: Vec<(u64, cr_e::Block)>,
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
			let true_blk = self.builder.create_block(); trace!("++{:?}", true_blk);
			let else_blk = self.builder.create_block(); trace!("++{:?}", else_blk);
			let done_blk = self.builder.create_block(); trace!("++{:?}", done_blk);
			self.builder.ins().brif(cond_v, true_blk, &[], else_blk, &[]);

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
			let blk_top = self.builder.create_block(); trace!("++{:?}", blk_top);
			let blk_body = self.builder.create_block(); trace!("++{:?}", blk_body);
			let blk_exit = self.builder.create_block(); trace!("++{:?}", blk_exit);
			self.builder.ins().jump(blk_top, &[]);

			self.builder.switch_to_block(blk_top);
			// NOTE: no seal yet, reverse jumps happen.

			self.stack.push(Scope::new_loop(blk_top, blk_exit));
			let cond_v = self.handle_expr_def(cond);
			let cond_v = self.get_value(cond_v);
			self.builder.ins().brif(cond_v, blk_body, &[], blk_exit, &[]);

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
			let blk_body = self.builder.create_block(); trace!("++{:?}", blk_body);
			let blk_foot = self.builder.create_block(); trace!("++{:?}", blk_foot);	// target of continue
			let blk_exit = self.builder.create_block(); trace!("++{:?}", blk_exit);	// target of break
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
				self.builder.ins().brif(cond_v, blk_body, &[], blk_exit, &[]);
			}

			self.builder.seal_block(blk_body);	// Seal the loop body, jumps now known.

			self.builder.switch_to_block(blk_exit);
			self.builder.seal_block(blk_exit);
			},
		Statement::ForLoop { ref init, ref cond, ref inc, ref body } => {
			if let Some(init) = init {
				self.handle_expr_def(init);
			}

			let blk_top = self.builder.create_block(); trace!("++{:?} (for top)", blk_top);	// loop back
			let blk_body = self.builder.create_block(); trace!("++{:?} (for body)", blk_body);
			let blk_foot = self.builder.create_block(); trace!("++{:?} (for foot)", blk_foot);	// target of continue
			let blk_exit = self.builder.create_block(); trace!("++{:?} (for exit)", blk_exit);	// target of break
			self.builder.ins().jump(blk_top, &[]);

			self.builder.switch_to_block(blk_top);
			// NOTE: no seal yet, reverse jumps happen.

			if let Some(cond) = cond {
				let cond_v = self.handle_node(cond);
				let cond_v = self.get_value(cond_v);
				self.builder.ins().brif(cond_v, blk_body, &[], blk_exit, &[]);
			}
			else {
				self.builder.ins().jump(blk_body, &[]);
			}

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

					let blk_orphan = self.builder.create_block(); trace!("++{:?} (continue orphan)", blk_orphan);
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

					let blk_orphan = self.builder.create_block(); trace!("++{:?} (break orphan)", blk_orphan);
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
			let blk_orphan = self.builder.create_block(); trace!("++{:?} (return orphan)", blk_orphan);
			self.builder.switch_to_block(blk_orphan);
			self.builder.seal_block(blk_orphan);
			},

		Statement::Switch(ref val, ref body) => {
			trace!("{}switch {:?}", self.indent(), val);
			let val = self.handle_node(val);
			let val = self.get_value(val);
			// - Make a block to contain the condition table (don't start it yet), and for the break target
			let blk_cond = self.builder.create_block();
			self.builder.ins().jump(blk_cond, &[]);
			let blk_end = self.builder.create_block();
			let blk_body = self.builder.create_block();
			self.builder.switch_to_block(blk_body);
			self.builder.seal_block(blk_body);
			// - Convert the body (first block should be an orphan)
			self.stack.push(Scope::new_switch(blk_end));
			self.handle_block(body);
			let labels = self.stack.pop().and_then(|v| v.switch).expect("Didn't pop a switch scope");
			self.builder.ins().jump(blk_end, &[]);
			// - Generate table (using ::cranelift_frontend::Switch)
			self.builder.switch_to_block(blk_cond);
			self.builder.seal_block(blk_cond);
			// TODO: cranelift_frontend::Switch doesn't seal its blocks
			let mut switch = ::cranelift_frontend::Switch::new();
			for (v,b) in &labels.case_labels
			{
				switch.set_entry(*v as u128, *b);
			}
			switch.emit(&mut self.builder, val, labels.case_default.unwrap_or(blk_end));
			// Seal target blocks
			for (_v,b) in &labels.case_labels {
				self.builder.seal_block(*b);
			}
			if let Some(b) = labels.case_default {
				self.builder.seal_block(b);
			}
			// - Finalise
			self.builder.switch_to_block(blk_end);
			self.builder.seal_block(blk_end);
			},
		Statement::CaseDefault => {
			trace!("{}default:", self.indent());
			let blk = {	// TODO: if there's chanined cases, be more efficient
				let blk = self.builder.create_block(); trace!("++{:?}", blk);
				self.builder.ins().jump(blk, &[]);
				self.builder.switch_to_block(blk);
				// - No seal, it's a target
				blk
				};
			for e in self.stack.iter_mut().rev()
			{
				if let Some(ref mut sw) = e.switch {
					assert!(sw.case_default.is_none());
					sw.case_default = Some(blk);
					return;
				}
			}
			panic!("TODO: Error for case outside a switch");
			},
		Statement::CaseSingle(v) => {
			trace!("{}case {}:", self.indent(), v);
			let blk = {	// TODO: if there's chanined cases, be more efficient
				let blk = self.builder.create_block(); trace!("++{:?}", blk);
				self.builder.ins().jump(blk, &[]);
				self.builder.switch_to_block(blk);
				// - No seal, it's a target
				blk
				};
			for e in self.stack.iter_mut().rev()
			{
				if let Some(ref mut sw) = e.switch {
					sw.case_labels.push( (v, blk,) );
					return;
				}
			}
			panic!("TODO: Error for case outside a switch");
			},
		Statement::CaseRange(s, e) => {
			todo!("CaseRange({} ..= {})", s, e);
			},

		Statement::Goto(ref label) => {
			trace!("{}goto {:?}", self.indent(), label);
			// If the label is already defined, then insert a jump to that block.
			if let Some(b) = self.labels.get(label) {
				self.builder.ins().jump(b.clone(), &[]);
			}
			// Otherwise, create a block and store it for when the label is created
			else {
				let blk = self.builder.create_block(); trace!("++{:?}", blk);
				self.missed_labels.insert(label.clone(), blk);
				self.builder.ins().jump(blk, &[]);
			}
			// Start a new block (orphaned)
			let blk_orphan = self.builder.create_block(); trace!("++{:?}", blk_orphan);
			self.builder.switch_to_block(blk_orphan);
			self.builder.seal_block(blk_orphan);
			},
		Statement::Label(ref label) => {
			trace!("{}{:?}:", self.indent(), label);
			// Make a new block
			let blk = if let Some(blk) = self.missed_labels.remove(label) {
					blk
				}
				else {
					let blk = self.builder.create_block(); trace!("++{:?}", blk);
					blk
				};
			debug!("{}{:?} = {:?}", self.indent(), label, blk);
			self.builder.ins().jump(blk, &[]);
			self.builder.switch_to_block(blk);
			// - No seal, it's a bi-directional target
			// Add the label to a list of labels
			self.labels.insert(label.clone(), blk);
			},
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
			Some(crate::ast::IdentRef::StaticItem) => {
				//let context = &mut self.context;
				//let builder = &mut self.builder;
				ValueRef::Global(name.clone(), 0, ty.clone())
				},
			Some(crate::ast::IdentRef::Function) => {
				let context = &mut self.context;
				let builder = &mut self.builder;
				// TODO: For variadic functions, we need to make a new entry/signature every time they're called
				let v = self.fcn_imports.entry(name.clone())
					.or_insert_with(|| {
						let fr = context.get_function(&name, match ty.basetype { BaseType::Function(ref f) => f, ref t => panic!("Function not function type {:?}", t), });
						let func_data = ::cranelift_codegen::ir::ExtFuncData {
							name: cranelift_codegen::ir::ExternalName::User(fr.name.clone()),
							signature: builder.import_signature(fr.sig.clone()),
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
			let did = self.context.create_string(val.clone().into_bytes());
			// Get value
			let gv = self.context.module.declare_data_in_func(did, self.builder.func);
			ValueRef::Temporary(self.builder.ins().symbol_value( CRTY_PTR, gv ))
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
			let inst = if let ValueRef::Function(ref _name, fcn_ref) = fcn {
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
			self.handle_cast(ty, val, src_ty, /*is_implicit=*/true)
			},
		
		NodeKind::Ternary(ref cond, ref val_true, ref val_false) => {
			let is_lvalue = node.meta.as_ref().unwrap().is_lvalue;

			let cond_v = self.handle_node(cond);
			let cond_v = self.get_value(cond_v);
			let true_blk = self.builder.create_block();	trace!("++{:?}", true_blk);
			let else_blk = self.builder.create_block();	trace!("++{:?}", else_blk);
			let done_blk = self.builder.create_block();	trace!("++{:?}", done_blk);
			self.builder.ins().brif(cond_v, true_blk, &[], else_blk, &[]);

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
			use crate::ast::UniOp;
			match op
			{
			UniOp::PostDec => {
				let val = self.get_value(val_in.clone());
				let ov = self.builder.ins().iadd_imm(val, -1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(val)
				},
			UniOp::PostInc => {
				let val = self.get_value(val_in.clone());
				let ov = self.builder.ins().iadd_imm(val, 1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(val)
				},
			UniOp::PreDec => {
				let val = self.get_value(val_in.clone());
				let ov = self.builder.ins().iadd_imm(val, -1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(ov)
				},
			UniOp::PreInc => {
				let val = self.get_value(val_in.clone());
				let ov = self.builder.ins().iadd_imm(val, 1);
				self.assign_value(val_in, ValueRef::Temporary(ov));
				ValueRef::Temporary(ov)
				},
			UniOp::Deref => {
				let val = self.get_value(val_in.clone());
				let ity = match ty.basetype
					{
					BaseType::Pointer(ref ity) => ity.clone(),
					_ => panic!("Deref of bad type - {:?}", ty),
					};
				ValueRef::Pointer(val, 0, ity)
				},
			UniOp::Address => match val_in
				{
				ValueRef::Temporary(_) => panic!("Taking address of temporary"),
				ValueRef::StackSlot(ref ss, ofs, _) => ValueRef::Temporary(self.builder.ins().stack_addr(CRTY_PTR, *ss, ofs as i32)),
				ValueRef::Pointer(ref base, ofs, _) => ValueRef::Temporary(self.builder.ins().iadd_imm(*base, ofs as i64)),
				_ => todo!("handle_node - UniOp Address {:?}", val_in),
				},
			UniOp::Neg => {
				let val = self.get_value(val_in.clone());
				match ty.basetype
				{
				BaseType::Integer(_) => ValueRef::Temporary(self.builder.ins().ineg(val)),
				_ => todo!("Neg on {:?}", ty),
				}
				},
			UniOp::BitNot => {
				let val = self.get_value(val_in.clone());
				match ty.basetype
				{
				//BaseType::Bool => ValueRef::Temporary(self.builder.ins().bnot(val)),
				BaseType::Integer(_) => ValueRef::Temporary(self.builder.ins().bnot(val)),
				_ => todo!("BitNot on {:?}", ty),
				}
				},
			UniOp::LogicNot => {
				let val = self.get_value(val_in.clone());
				match ty.basetype
				{
				BaseType::Bool => ValueRef::Temporary(self.builder.ins().icmp_imm(cr_cc::IntCC::Equal, val, 0)),
				BaseType::Integer(_) => ValueRef::Temporary(self.builder.ins().icmp_imm(cr_cc::IntCC::Equal, val, 0)),
				BaseType::Pointer(_) => ValueRef::Temporary(self.builder.ins().icmp_imm(cr_cc::IntCC::Equal, val, 0)),
				_ => todo!("LogicNot on {:?}", ty),
				}
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
			Some((_idx, ofs, ity)) =>
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

		NodeKind::SizeofType(ref ty) => {
			ValueRef::Temporary(self.builder.ins().iconst(cr_tys::I32, ty.get_size().expect("sizeof on opaque") as i64))
			},
		NodeKind::SizeofExpr(..) => panic!("TODO: handle_node - {:?}", node),
		NodeKind::Intrinsic(ref name, ref types, ref values) => match &name[..]
			{
			"va_arg" => {
				let list = self.handle_node(&values[0]);
				let ty = cvt_ty(&types[0]);
				//todo!("handle_node - va_arg ty={:?} list={:?}", ty, list); 
				// TODO: This heavily depends on the specific ABI.
				match ty
				{
				cr_tys::I32 => ValueRef::Temporary(self.builder.ins().iconst(ty, 0 as i64)),
				cr_tys::I64 => ValueRef::Temporary(self.builder.ins().iconst(ty, 0 as i64)),
				_ => todo!("handle_node - va_arg ty={:?} list={:?}", ty, list),
				}
				},
			_ => panic!("TODO: handle_node - {:?}", node),
			},
		}
	}
	fn handle_expr_def(&mut self, node: &crate::ast::ExprOrDef) -> ValueRef
	{
		use crate::ast::ExprOrDef;
		match node
		{
		ExprOrDef::Expr(ref e) => self.handle_node(e),
		ExprOrDef::Definition(ref list) => {
			for var_def in list
			{
				self.define_var(var_def);
			}
			self.vars[list.last().unwrap().index.unwrap()].clone()
			},
		}
	}

	/// Common processing of cast operations (between `ImplicitCast` and `Cast`)
	fn handle_cast(&mut self, dst_ty: &TypeRef, src_val: ValueRef, src_ty: &TypeRef, is_implicit: bool) -> ValueRef
	{
		let cast_name = if is_implicit { "ImplicitCast" } else { "Cast" };
		if let BaseType::Void = dst_ty.basetype {
			let _ = self.get_value(src_val);
			ValueRef::Void
		}
		else if let BaseType::Array(..) = src_ty.basetype {
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
			ValueRef::Global(name, ofs, _ty) => {
				let ctxt = &*self.context;
				let func = &mut *self.builder.func;
				let gv = self.global_imports.entry(name.clone()).or_insert_with(|| {
					let did = match ctxt.globals.get(&name)
						{
						Some(r) => r.clone(),
						None => panic!("TODO: Error when global not defined - {}", name),
						};
						ctxt.module.declare_data_in_func(did, func)
					})
					.clone();
				if ofs != 0 {
					todo!("ValueRef::Global array cast to pointer - non-zero offset");
				}
				ValueRef::Temporary(self.builder.ins().symbol_value(CRTY_PTR, gv))
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
				//(cr_tys::B , cr_tys::B8 ) => val,
				(cr_tys::I8 , cr_tys::I8 ) => val,
				(cr_tys::I16, cr_tys::I16) => val,
				(cr_tys::I32, cr_tys::I32) => val,
				(cr_tys::I64, cr_tys::I64) => val,
				// bool -> * : uextend
				//(cr_tys::B8, cr_tys::I8) => val,
				//(cr_tys::B8, cr_tys::I16)
				//| (cr_tys::B8, cr_tys::I32)
				//| (cr_tys::B8, cr_tys::I64)
				//	=> self.builder.ins().uextend(dst_cty, val),
				// * -> bool : non-zero
				//(cr_tys::I8, cr_tys::B8)
				//| (cr_tys::I16, cr_tys::B8)
				//| (cr_tys::I32, cr_tys::B8)
				//| (cr_tys::I64, cr_tys::B8)
				//	=> self.builder.ins().icmp_imm(cr_cc::IntCC::NotEqual, val, 0),
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

				BinOp::BitAnd => self.builder.ins().band(val_l, val_r),
				BinOp::BitOr => self.builder.ins().bor(val_l, val_r),

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
			switch: None,
			}
	}
	fn new_switch(blk_break: cr_e::Block) -> Self {
		Scope {
			blk_break: Some(blk_break),
			blk_continue: None,
			switch: Some(Default::default()),
			}
	}
	fn new_loop(blk_break: cr_e::Block, blk_continue: cr_e::Block) -> Self {
		Scope {
			blk_break: Some(blk_break),
			blk_continue: Some(blk_continue),
			switch: None,
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
const CRTY_PTR: cr_tys::Type = cr_tys::I64;	//	cr_tys::R32,
fn cvt_ty_opt(ty: &TypeRef) -> Option<cr_tys::Type>
{
	Some(match ty.basetype
	{
	BaseType::Void => panic!("Attempting to convert `void` to a cranelift type"),
	BaseType::Bool => cr_tys::I8,
	BaseType::Pointer(..) => CRTY_PTR,
	BaseType::Integer(ic) => match ty.get_size().unwrap()
		{
		8 => cr_tys::I64,
		4 => cr_tys::I32,
		2 => cr_tys::I16,
		1 => cr_tys::I8,
		sz => todo!("Convert integer {:?} ({}) to cranelift", ic, sz),
		},
	BaseType::Struct(_) => return None,
	BaseType::Enum(_) => cr_tys::I32,
	BaseType::MagicType(crate::types::MagicType::VaList) => CRTY_PTR,
	BaseType::MagicType(crate::types::MagicType::Named(_,crate::types::MagicTypeRepr::VoidPointer)) => CRTY_PTR,
	BaseType::Array(ref _ty, crate::types::ArraySize::None) => CRTY_PTR,
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
		sig.params.push( AbiParam::new(
			if let BaseType::Array(..) = arg_ty.basetype {
				// Arrays are pointers in arguments
				CRTY_PTR
			}
			else {
				cvt_ty(&arg_ty)
			}) );
	}
	if ty.is_variadic {
		// TODO: Encode variadic types (cranelift can't do that yet)
		// Workaround suggested in https://github.com/bytecodealliance/wasmtime/issues/1030#issuecomment-549111736
		// - Create a new function entry/signature for each time a variadic is called
	}
	sig
}
