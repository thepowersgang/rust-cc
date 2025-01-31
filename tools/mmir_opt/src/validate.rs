use crate::mir::Terminator;
use crate::mir::Statement;
use crate::mir::Value;
use crate::logger::Logger;

pub fn validate_function(logger: &mut Logger, fcn: &mut crate::mir::Function, _sig: &crate::types::FcnTy)
{
    // Check 
    set_before_use(logger, fcn, _sig);
}

trait Vistior {
    fn set_location(&mut self, location: crate::LineRef) {let _ = location;}
    fn slot_write(&mut self, slot: &crate::mir::Slot);
    fn slot_borrow(&mut self, slot: &crate::mir::Slot, mutablity: crate::types::Mutability);
    fn slot_read(&mut self, slot: &crate::mir::Slot);
}
fn visit_slot_wrappers<V: Vistior>(slot: &crate::mir::Slot, visitor: &mut V)
{
    for (i,w) in slot.wrappers.iter().enumerate().rev() {
        match w {
        crate::mir::SlotWrapper::Deref => {
            visitor.slot_read(&crate::mir::Slot { root: slot.root.clone(), wrappers: slot.wrappers[..i].to_owned() });
        }
        crate::mir::SlotWrapper::Index(var) => {
            visitor.slot_read(&crate::mir::Slot { root: crate::mir::SlotRoot::Local(*var), wrappers: vec![] });
        }
        crate::mir::SlotWrapper::Field(_) => {},
        crate::mir::SlotWrapper::Downcast(_) => {},
        }
    }
}
impl<V: Vistior> Vistior for &mut V {
    
    fn set_location(&mut self, location: crate::LineRef) {
        (**self).set_location(location);
    }
    
    fn slot_write(&mut self, slot: &crate::mir::Slot) {
        (**self).slot_write(slot)
    }

    fn slot_borrow(&mut self, slot: &crate::mir::Slot, mutablity: crate::types::Mutability) {
        (**self).slot_borrow(slot, mutablity)
    }

    fn slot_read(&mut self, slot: &crate::mir::Slot) {
        (**self).slot_read(slot)
    }
}
fn visit_slot_read<V: Vistior>(slot: &crate::mir::Slot, visitor: &mut V)
{
    visit_slot_wrappers(slot, visitor);
    visitor.slot_read(slot)
}
fn visit_param<V: Vistior>(param: &crate::mir::Param, visitor: &mut V)
{
    match param {
    crate::mir::Param::Const(_) => {},
    crate::mir::Param::Slot(slot) => visit_slot_read(slot, visitor),
    }
}
fn visit_function<V>(fcn: &mut crate::mir::Function, visitor: &mut V)
where
    V: Vistior
{
    for (bb_idx, block) in fcn.blocks.iter().enumerate()
    {
        for (stmt_idx, stmt) in block.statements.iter().enumerate()
        {
            visitor.set_location(crate::LineRef { bb_idx, stmt_idx: crate::StmtIdx::Stmt(stmt_idx as u16) });
            match stmt {
            Statement::SpanComment(_) => {},
            Statement::Assign(dst, src) => {
                match src {
                Value::Constant(_) => {},
                Value::Use(slot) => {
                    visit_slot_read(slot, visitor);
                },
                Value::Borrow(mutability, slot) => {
                    visit_slot_wrappers(slot, visitor);
                    visitor.slot_borrow(slot, *mutability);
                },
                Value::BinOp(param1, _, param2) => {
                    visit_param(param1, visitor);
                    visit_param(param2, visitor);
                },
                Value::UniOp(_, slot) => {
                    visit_slot_read(slot, visitor);
                },
                Value::Cast(slot, _) => {
                    visit_slot_read(slot, visitor);
                }
                Value::DstPtr(slot) => {
                    visit_slot_wrappers(slot, visitor);
                    visitor.slot_read(slot/*, true*/);
                },
                Value::DstMeta(slot) => {
                    visit_slot_wrappers(slot, visitor);
                    visitor.slot_read(slot/*, true*/);
                },
                Value::Array(vals)
                | Value::Tuple(vals)
                | Value::Struct(_, vals)
                | Value::EnumVariant(_, _, vals)
                => {
                    for param in vals {
                        visit_param(param, visitor);
                    }
                    },
                Value::UnionVariant(_, _, param) => visit_param(param, visitor),
                }
                visit_slot_wrappers(dst, visitor);
                visitor.slot_write(dst);
                },
            }
        }
        visitor.set_location(crate::LineRef { bb_idx, stmt_idx: crate::StmtIdx::Term });
        match &block.terminator {
        Terminator::Removed => {},
        Terminator::Invalid => {},
        Terminator::Return => {},
        Terminator::Diverge => {},
        Terminator::Call(tc) => {
            match &tc.target {
            crate::mir::CallTarget::Path(_) => {},
            crate::mir::CallTarget::Intrinsic(_name, _tys) => {},
            crate::mir::CallTarget::Value(slot) => visit_slot_read(slot, visitor),
            }
            visit_slot_wrappers(&tc.dst, visitor);
            visitor.slot_write(&tc.dst);
        },
        Terminator::Goto(_) => {},
        Terminator::If(slot, _, _) => visit_slot_read(slot, visitor),
        Terminator::SwitchValue(slot, _switch_values, _vec, _) => visit_slot_read(slot, visitor),
        }
    }
}

fn set_before_use(logger: &mut Logger, fcn: &mut crate::mir::Function, _sig: &crate::types::FcnTy)
{
    struct GetSet {
        set_locals: Vec<bool>,
    }
    impl Vistior for GetSet {
        fn slot_write(&mut self, slot: &crate::mir::Slot) {
            if let Some(v) = slot.is_local() {
                self.set_locals[v] = true;
            }
            // Just assume any write makes the slot valid
            if ! slot.wrappers.iter().any(|w| matches!(w, crate::mir::SlotWrapper::Deref)) {
                if let crate::mir::SlotRoot::Local(v) = slot.root {
                    self.set_locals[v] = true;
                }
            }
        }
    
        fn slot_borrow(&mut self, _slot: &crate::mir::Slot, _mutablity: crate::types::Mutability) {
        }
    
        fn slot_read(&mut self, _slot: &crate::mir::Slot) {
        }
    }
    let mut visitor_get_set = GetSet {
        set_locals: vec![false; fcn.locals.len()],
    };
    visit_function(fcn, &mut visitor_get_set);

    struct CheckSet<'a,'l,'l2> {
        location: crate::LineRef,
        logger: &'l mut Logger<'l2>,
        gs: &'a GetSet,
        //fail: bool,
    }
    impl CheckSet<'_,'_,'_> {
        fn check_slot(&mut self, slot: &crate::mir::Slot) {
            if let Some(v) = slot.is_local() {
                if !self.gs.set_locals[v] {
                    self.logger.error(format_args!("{}: Local #{v} used without ever being set", self.location));
                    //self.fail = true;
                }
            }
        }
    }
    impl Vistior for CheckSet<'_,'_,'_> {
        fn set_location(&mut self, location: crate::LineRef) {
            self.location = location;
        }
        
        fn slot_write(&mut self, _slot: &crate::mir::Slot) {
        }
    
        fn slot_borrow(&mut self, slot: &crate::mir::Slot, _mutablity: crate::types::Mutability) {
            self.check_slot(slot);
        }
        fn slot_read(&mut self, slot: &crate::mir::Slot) {
            self.check_slot(slot);
        }
    }

    visit_function(fcn, &mut CheckSet {
        location: crate::LineRef { bb_idx: 0, stmt_idx: crate::StmtIdx::Term },
        logger,
        gs: &visitor_get_set,
        //fail: false,
    });
}