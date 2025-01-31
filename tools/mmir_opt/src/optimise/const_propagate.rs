//!
//! 
//! 
use crate::mir::Terminator;
use crate::mir::Statement;
use crate::mir::Value;
use super::helpers;

pub(super) fn const_propagate(logger: &mut crate::logger::Logger, fcn: &mut crate::mir::Function) -> bool
{
    log_debug!(logger, "const_propagate");
    use crate::mir::Const;

    let mut rv = false;
    let usage_count = {
        let mut usage_count: Vec<_> = (0..fcn.blocks.len()).map(|_| 0).collect();
        usage_count[0] = 1;
        helpers::visit_block_targets_mut(fcn, |&mut tgt| usage_count[tgt] += 1);
        usage_count
        };
    
    let mut known_values = ::std::collections::HashMap::new();
    for bb in &mut fcn.blocks {
        known_values.clear();

        fn get_for_slot<'a>(
            known_values: &::std::collections::HashMap<usize,&'a crate::mir::Const>,
            lv: &crate::mir::Slot
        ) -> Option<&'a crate::mir::Const> {
            if let Some(i) = lv.is_local() {
                if let Some(&v) = known_values.get(&i) {
                    return Some(v);
                }
            }
            None
        }
        fn get_for_param<'a>(
            known_values: &::std::collections::HashMap<usize,&'a crate::mir::Const>,
            lv: &crate::mir::Param
        ) -> Option<&'a crate::mir::Const> {
            if let crate::mir::Param::Slot(lv) = lv {
                return get_for_slot(known_values, lv);
            }
            None
        }
        for stmt in &mut bb.statements {
            match stmt {
            Statement::SpanComment(_) => {},
            Statement::Assign(dst, src) => {
                if let Some(i) = dst.is_local() {
                    known_values.remove(&i);
                }
                match src
                {
                Value::Constant(c) => {
                    if let Some(idx) = dst.is_local() {
                        known_values.insert(idx, &*c);
                    }
                },
                Value::Use(v) => {
                    if let Some(c) = get_for_slot(&known_values, v) {
                        *src = Value::Constant(c.clone());
                        rv = true;
                    }
                }
                Value::Borrow(_, slot) => {
                    if let Some(i) = slot.is_local() {
                        known_values.remove(&i);
                    }
                },
                Value::BinOp(a, op, b) => {
                    if let Some(c) = get_for_param(&known_values, a) {
                        *a = crate::mir::Param::Const(c.clone());
                        rv = true;
                    }
                    if let Some(c) = get_for_param(&known_values, b) {
                        *b = crate::mir::Param::Const(c.clone());
                        rv = true;
                    }
                    if let (crate::mir::Param::Const(a), crate::mir::Param::Const(b)) = (a,b) {
                        use crate::mir::{BinOp,Const};
                        fn cu(v: u128, bits: &crate::types::Bits) -> Value {
                            Value::Constant(Const::Unsigned(bits.mask_unsigned(v), *bits))
                        }
                        fn cs(v: i128, bits: &crate::types::Bits) -> Value {
                            Value::Constant(Const::Signed(bits.mask_signed(v), *bits))
                        }
                        fn cf(v: f64, bits: &crate::types::Bits) -> Value {
                            Value::Constant(Const::Float(v, *bits))
                        }
                        match op {
                        BinOp::Add => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a + *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a + *b, bits),
                            (Const::Float(a, bits), Const::Float(b, _)) => *src = cf(*a + *b, bits),
                            _ => {},
                            },
                        BinOp::Sub => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a - *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a - *b, bits),
                            (Const::Float(a, bits), Const::Float(b, _)) => *src = cf(*a - *b, bits),
                            _ => {},
                            },
                        BinOp::Mul => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a * *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a * *b, bits),
                            (Const::Float(a, bits), Const::Float(b, _)) => *src = cf(*a * *b, bits),
                            _ => {},
                            },
                        BinOp::Div => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a / *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a / *b, bits),
                            (Const::Float(a, bits), Const::Float(b, _)) => *src = cf(*a / *b, bits),
                            _ => {},
                            },
                        BinOp::Rem => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a % *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a % *b, bits),
                            (Const::Float(a, bits), Const::Float(b, _)) => *src = cf(*a % *b, bits),
                            _ => {},
                            },
                        BinOp::Shr => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a >> *b, bits),
                            (Const::Unsigned(a, bits),Const::Signed  (b, _)) => *src = cu(*a >> *b, bits),
                            _ => {},
                            },
                        BinOp::Shl => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a << *b, bits),
                            (Const::Unsigned(a, bits),Const::Signed  (b, _)) => *src = cu(*a << *b, bits),
                            _ => {},
                            },
                        BinOp::BitAnd => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a & *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a & *b, bits),
                            _ => {},
                            },
                        BinOp::BitOr  => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a | *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a | *b, bits),
                            _ => {},
                            },
                        BinOp::BitXor => match (a,b)
                            {
                            (Const::Unsigned(a, bits),Const::Unsigned(b, _)) => *src = cu(*a ^ *b, bits),
                            (Const::Signed  (a, bits),Const::Signed  (b, _)) => *src = cs(*a ^ *b, bits),
                            _ => {},
                            },
                        BinOp::Less         => *src = Value::Constant(Const::Boolean( a < b )),
                        BinOp::Greater      => *src = Value::Constant(Const::Boolean( a > b )),
                        BinOp::LessEqual    => *src = Value::Constant(Const::Boolean( a <= b )),
                        BinOp::GreaterEqual => *src = Value::Constant(Const::Boolean( a >= b )),
                        BinOp::Equals       => *src = Value::Constant(Const::Boolean( a == b )),
                        BinOp::NotEquals    => *src = Value::Constant(Const::Boolean( a != b )),
                        }
                    }
                },
                Value::UniOp(op, a) => {
                    if let Some(c) = get_for_slot(&known_values, a) {
                        *src = Value::Constant(match op {
                            crate::mir::UniOp::Inv => match c {
                                Const::Boolean(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::Unsigned(v, bits) => Const::Unsigned(bits.mask_unsigned(!*v), bits.clone()),
                                Const::Signed(v, bits) => Const::Signed(!*v, bits.clone()),
                                Const::Float(_, _) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::String(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::ItemAddr(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                            },
                            crate::mir::UniOp::Neg => match c {
                                Const::Boolean(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::Unsigned(_, _) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::Signed(v, bits) => Const::Signed(-*v, bits.clone()),
                                Const::Float(v, bits) => Const::Float(-*v, bits.clone()),
                                Const::String(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::ItemAddr(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                            },
                            });
                        rv = true;
                    }
                },
                Value::Cast(v, ty) => {
                    if let Some(c) = get_for_slot(&known_values, v) {
                        use crate::types::Root;
                        let v = match &ty.root {
                            Root::Unsigned(bits) if ty.wrappers.is_empty() => {
                                let new_v = match c
                                    {
                                    Const::Boolean(v) => *v as u128,
                                    Const::Unsigned(v, _bits) => *v,
                                    Const::Signed(v, _bits) => *v as u128,
                                    Const::Float(_, _) => todo!(),
                                    Const::String(_)
                                    | Const::ItemAddr(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
                                    };
                                Some(Const::Unsigned(new_v, bits.clone()))
                                },
                            Root::Signed(bits) if ty.wrappers.is_empty() => {
                                let new_v = match c
                                    {
                                    Const::Boolean(v) => *v as i128,
                                    Const::Unsigned(v, _bits) => *v as i128,
                                    Const::Signed(v, _bits) => *v,
                                    Const::Float(_, _) => todo!(),
                                    Const::String(_)
                                    | Const::ItemAddr(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
                                    };
                                Some(Const::Signed(new_v, bits.clone()))
                                },
                            Root::Float(bits) if ty.wrappers.is_empty() => {
                                let new_v = match c
                                    {
                                    Const::Boolean(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
                                    Const::Unsigned(v, _bits) => *v as f64,
                                    Const::Signed(v, _bits) => *v as f64,
                                    Const::Float(_, _) => todo!(),
                                    Const::String(_)
                                    | Const::ItemAddr(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
                                    };
                                Some(Const::Float(new_v, bits.clone()))
                                },
                            _ => None,
                            };
                        if let Some(c) = v {
                            *src = Value::Constant(c);
                            rv = true;
                        }
                    }
                },
                Value::DstPtr(_) => {},
                Value::DstMeta(_) => {},
                Value::Tuple(vals) => {
                    for v in vals {
                        if let Some(c) = get_for_param(&known_values, v) {
                            *v = crate::mir::Param::Const(c.clone());
                            rv = true;
                        }
                    }
                },
                Value::Array(vals) => {
                    for v in vals {
                        if let Some(c) = get_for_param(&known_values, v) {
                            *v = crate::mir::Param::Const(c.clone());
                            rv = true;
                        }
                    }
                },
                Value::Struct(_, vals) => {
                    for v in vals {
                        if let Some(c) = get_for_param(&known_values, v) {
                            *v = crate::mir::Param::Const(c.clone());
                            rv = true;
                        }
                    }
                },
                Value::UnionVariant(_, _, _) => {},
                Value::EnumVariant(_, _, _) => {},
                }
                },
            }
        }

        match bb.terminator {
        Terminator::Removed => {},
        Terminator::Invalid => {},
        Terminator::Return => {},
        Terminator::Diverge => {},
        Terminator::Goto(tgt) => if usage_count[tgt] == 1 {
            // Continue into this block
        },
        Terminator::Call(ref call) => {
            if usage_count[call.bb_ret] == 1 {
                // Continue into this block
            }
        },
        Terminator::If(_, bb_true, bb_false) => {
            if usage_count[bb_true] == 1 {
                // Continue into this block
            }
            if usage_count[bb_false] == 1 {
                // Continue into this block
            }
        },
        Terminator::SwitchValue(ref value, _, ref targets, bb_default) => {
            if let Some(c) = get_for_slot(&known_values, value) {
                todo!("Replace SwitchValue with a jump due to known value - {:?}", c);
            }
            for &t in targets {
                if usage_count[t] == 1 {
                    // Continue into this block
                }
            }
            if usage_count[bb_default] == 1 {
                // Continue into this block
            }

        }
        }
    }
    rv
}
