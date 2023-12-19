use crate::mir::Terminator;
use crate::mir::Statement;
use crate::mir::Value;

pub fn optimise_function(fcn: &mut crate::mir::Function, sig: &crate::types::FcnTy)
{
    loop {
        let mut changed = false;
        // - Simplify flow control: If a block is just a goto, replace uses of it with the target
        while simplify_control_flow(fcn) {
            changed = true;
        }
        // - Redundant Pointer Casts
        //  > Two casts in a row can be simplfied into just the latter (if the initial source is a pointer or usize)
        //  * This happens when `NULL` is cast to something.
        #[cfg(false_)]
        while remove_redundant_casts(fcn, sig) {
            changed = true;
        }
        // - Single write/use temporaries
        #[cfg(false_)]
        while remove_single_use(fcn) {
            changed = true;
        }
        // - Unused writes
        #[cfg(false_)]
        while remove_dead_writes(fcn) {
            changed = true;
        }
        // - Constant propagation
        //  > Replace casts with literals
        //  > Propagate through to operators
        while const_propagate(fcn) {
            changed = true;
        }
        if !changed {
            break
        }
    }

    //println!(">>");
    //crate::dump::dump_function_body(&mut ::std::io::stdout(), fcn, None).unwrap();
    // Delete unused blocks
    clean_unused_blocks(fcn);
    //println!(">>");
    //crate::dump::dump_function_body(&mut ::std::io::stdout(), fcn, None).unwrap();
    simplify_control_flow(fcn);
}

fn simplify_control_flow(fcn: &mut crate::mir::Function) -> bool
{
    let rewrites: Vec<_> = fcn.blocks.iter()
        .map(|bb| {
            match bb.terminator {
            Terminator::Goto(v) if bb.statements.is_empty() => Some(v),
            _ => None,
            }
        })
        .collect();
    let mut rv = false;
    let mut check = |tgt: &mut usize|if let Some(new) = rewrites[*tgt] {
        *tgt = new;
        rv = true;
    };
    for bb in &mut fcn.blocks {
        match &mut bb.terminator {
        Terminator::Invalid => {},
        Terminator::Return => {},
        Terminator::Diverge => {},
        Terminator::Goto(ref mut tgt) => check(tgt),
        Terminator::Call(call) => {
            check(&mut call.bb_panic);
            check(&mut call.bb_ret);
        },
        Terminator::If(_, bb_true, bb_false) => {
            check(bb_true);
            check(bb_false);
        },
        }
    }
    rv
}


fn const_propagate(fcn: &mut crate::mir::Function) -> bool
{
    use crate::mir::Const;

    let mut rv = false;
    let usage_count = {
        let mut usage_count: Vec<_> = (0..fcn.blocks.len()).map(|_| 0).collect();
        usage_count[0] = 1;
        for bb in &fcn.blocks {
            match bb.terminator {
            Terminator::Invalid => {},
            Terminator::Return => {},
            Terminator::Diverge => {},
            Terminator::Goto(tgt) => usage_count[tgt] += 1,
            Terminator::Call(ref call) => {
                usage_count[call.bb_panic] += 1;
                usage_count[call.bb_ret] += 1;
            },
            Terminator::If(_, bb_true, bb_false) => {
                usage_count[bb_true] += 1;
                usage_count[bb_false] += 1;
            },
            }
        }
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
                Value::BinOp(a, _, b) => {
                    if let Some(c) = get_for_param(&known_values, a) {
                        *a = crate::mir::Param::Const(c.clone());
                        rv = true;
                    }
                    if let Some(c) = get_for_param(&known_values, b) {
                        *b = crate::mir::Param::Const(c.clone());
                        rv = true;
                    }
                },
                Value::UniOp(op, a) => {
                    if let Some(c) = get_for_slot(&known_values, a) {
                        *src = Value::Constant(match op {
                            crate::mir::UniOp::Inv => todo!("Evaluate UniOp {:?} {:?}", op, c),
                            crate::mir::UniOp::Neg => match c {
                                Const::Boolean(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::Unsigned(_, _) => todo!("Evaluate UniOp {:?} {:?}", op, c),
                                Const::Signed(v, bits) => Const::Signed(-*v, bits.clone()),
                                Const::Float(v, bits) => Const::Float(-*v, bits.clone()),
                                Const::String(_) => todo!("Evaluate UniOp {:?} {:?}", op, c),
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
                                    Const::String(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
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
                                    Const::String(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
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
                                    Const::String(_) => panic!("Malformed cast: {:?} to {:?}", c, ty),
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
        }
    }
    rv
}

fn clean_unused_blocks(fcn: &mut crate::mir::Function)
{
    // Determine if each block is referenced
    loop {
        let mut used: Vec<_> = (0..fcn.blocks.len()).map(|_| false).collect();
        used[0] = true;
        for bb in &fcn.blocks {
            match bb.terminator {
            Terminator::Invalid => {},
            Terminator::Return => {},
            Terminator::Diverge => {},
            Terminator::Goto(tgt) => used[tgt] = true,
            Terminator::Call(ref call) => {
                used[call.bb_panic] = true;
                used[call.bb_ret] = true;
            },
            Terminator::If(_, bb_true, bb_false) => {
                used[bb_true] = true;
                used[bb_false] = true;
            },
            }
        }
        let mut changed = false;
        for (used,bb) in Iterator::zip(used.into_iter(), fcn.blocks.iter_mut()) {
            match bb.terminator {
            Terminator::Invalid if bb.statements.is_empty() => {},
            _ if !used => {
                bb.statements.clear();
                bb.terminator = Terminator::Invalid;
                changed = true;
            },
            _ => {},
            }
        }
        if !changed {
            break;
        }
    }

    let mut new_idx = 0;
    let mapping: Vec<_> = fcn.blocks.iter()
        .map(|bb| {
            if let Terminator::Invalid = bb.terminator {
                None
            }
            else {
                let v = new_idx;
                new_idx += 1;
                Some(v)
            }
        })
        .collect()
        ;
    fcn.blocks.retain(|bb| match bb.terminator {
        Terminator::Invalid => false,
        _ => true,
    });
    let check = |tgt: &mut usize| *tgt = mapping[*tgt].unwrap();
    for bb in &mut fcn.blocks {
        match &mut bb.terminator {
        Terminator::Invalid => {},
        Terminator::Return => {},
        Terminator::Diverge => {},
        Terminator::Goto(ref mut tgt) => check(tgt),
        Terminator::Call(call) => {
            check(&mut call.bb_panic);
            check(&mut call.bb_ret);
        },
        Terminator::If(_, bb_true, bb_false) => {
            check(bb_true);
            check(bb_false);
        },
        }
    }
}