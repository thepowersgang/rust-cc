use crate::mir::Terminator;
use crate::mir::Statement;
use crate::mir::Value;

pub fn optimise_function(fcn: &mut crate::mir::Function, _sig: &crate::types::FcnTy)
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

fn remove_dead_writes(fcn: &mut crate::mir::Function) -> bool
{
    // Identify writes to variables that are never read, just overwritten
    // - This catches `"uninit"` assignments, AND just dead statements

    // Method:
    // - Enumerate paths through the function, stopping on a loop-back
    struct Path {
        ents: Vec<usize>,
        looped: Option<usize>,
    }
    let paths = {
        let mut paths = Vec::new();
        let mut stack = Vec::new();
        stack.push(vec![0usize]);
        while let Some(v) = stack.pop() {
            let mut check = |mut v: Vec<_>, blk: Option<usize>| {
                //println!("{:?}, {:?}", v, blk);
                let completed = if let Some(blk) = blk {
                    v.iter().find(|b| **b == blk).is_some()
                } else {
                    true
                };
                if completed {
                    paths.push(Path { ents: v, looped: blk });
                }
                else {
                    if let Some(blk) = blk {
                        v.push(blk);
                    }
                    stack.push(v);
                }
            };
            let bb = &fcn.blocks[*v.last().unwrap()];
            match bb.terminator {
            Terminator::Invalid => { check(v, None); },
            Terminator::Return => { check(v, None); },
            Terminator::Diverge => { check(v, None); },
            Terminator::Goto(blk) => {
                check(v, Some(blk));
                },
            Terminator::Call(ref call) => {
                check(v.clone(), Some(call.bb_ret));
                check(v, Some(call.bb_panic));
            },
            Terminator::If(_, bb_true, bb_false) => {
                check(v.clone(), Some(bb_true));
                check(v, Some(bb_false));
            },
            }
        }
        paths
    };

    // - Enumerate reads/writes of each variable (locations)
    //  > For each write, see if there is a read before the next write (along the path)
    #[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
    enum Op {
        Read,
        Write,
    }
    use ::std::collections::BTreeMap;
    let mut ops = BTreeMap::<usize,Vec<(usize,usize,Op)>>::new();
    for (bb_idx,bb) in fcn.blocks.iter().enumerate()
    {
        fn visit_slot_in_param(param: &crate::mir::Param, mut fcn: impl FnMut(&crate::mir::Slot)) {
            if let crate::mir::Param::Slot(s) = param {
                fcn(s);
            }
        }
        fn visit_slot_in_value(value: &Value, mut fcn: impl FnMut(&crate::mir::Slot)) {
            match value {
            Value::Constant(_) => {},
            Value::Use(v) => { fcn(v); },
            Value::Borrow(_, v) => { fcn(v); },
            Value::BinOp(a, _, b) => {
                visit_slot_in_param(a, &mut fcn);
                visit_slot_in_param(b, &mut fcn);
            },
            Value::UniOp(_, a) => {
                fcn(a);
            },
            Value::Cast(a, _) => { fcn(a) },
            Value::DstPtr(a) => { fcn(a) },
            Value::DstMeta(a) => { fcn(a) },
            Value::Tuple(ents) => {
                for p in ents {
                    visit_slot_in_param(p, &mut fcn);
                }
            },
            Value::Array(_) => todo!(),
            Value::Struct(_, _) => todo!(),
            Value::UnionVariant(_, _, _) => todo!(),
            Value::EnumVariant(_, _, _) => todo!(),
            }
        }

        fn visit_read(ops: &mut BTreeMap<usize,Vec<(usize,usize,Op)>>, slot: &crate::mir::Slot, bb_idx: usize, stmt_idx: usize) {
            for w in &slot.wrappers {
                if let crate::mir::SlotWrapper::Index(i) = *w {
                    ops.entry(i).or_default().push((bb_idx,stmt_idx, Op::Read));
                }
            }
            if let crate::mir::SlotRoot::Local(i) = slot.root {
                ops.entry(i).or_default().push((bb_idx,stmt_idx, Op::Read));
            }
        }
        fn visit_write(ops: &mut BTreeMap<usize,Vec<(usize,usize,Op)>>, slot: &crate::mir::Slot, bb_idx: usize, stmt_idx: usize) {
            for w in &slot.wrappers {
                if let crate::mir::SlotWrapper::Index(i) = *w {
                    ops.entry(i).or_default().push((bb_idx,stmt_idx, Op::Read));
                }
            }
            if let Some(i) = slot.is_local() {
                ops.entry(i).or_default().push((bb_idx,stmt_idx, Op::Write));
            }
        }
        for (stmt_idx, stmt) in bb.statements.iter().enumerate() {
            match stmt {
            Statement::SpanComment(_) => {},
            Statement::Assign(dst, src) => {
                // Look for usage

                visit_slot_in_value(src, |slot| {
                    visit_read(&mut ops, slot, bb_idx, stmt_idx);
                });

                visit_write(&mut ops, dst, bb_idx, stmt_idx);
            }
            }
        }
        let stmt_idx = bb.statements.len();
        match bb.terminator {
        Terminator::Invalid => {},
        Terminator::Return => {},
        Terminator::Diverge => {},
        Terminator::Goto(_) => {},
        Terminator::Call(ref call) => {

            for p in &call.args {
                visit_slot_in_param(p, |slot| {
                    visit_read(&mut ops, slot, bb_idx, stmt_idx);
                });
            }
            
            visit_write(&mut ops, &call.dst, bb_idx, stmt_idx);
        }
        Terminator::If(ref slot, _, _) => {
            visit_read(&mut ops, slot, bb_idx, stmt_idx);
        }
        }
    }

    for (_,o) in ops.iter_mut() {
        o.sort();
    }
    let mut rv = false;
    for (i, ops) in &ops {
        //println!("{i} {ops:?}");
        let check_block = |wr_bb_idx: usize, wr_stmt_idx: usize, _i: usize, bb_idx: usize| {
            for v @ &(bb,stmt,ref op) in ops.iter() {
                // Same BB and variable index
                if bb == bb_idx {
                    if bb_idx == wr_bb_idx {
                        if stmt < wr_stmt_idx {
                            return Some(v);
                        }
                        else if let (Op::Read,true) = (op, stmt == wr_stmt_idx) {
                            return Some(v);
                        }
                        else {
                        }
                    }
                    else {
                        return Some(v);
                    }
                }
                if bb > bb_idx {
                    break;
                }
            }
            None
        };
        let find_usage = |wr_bb_idx: usize, wr_stmt_idx: usize, i: usize| {
            // Look for a read/write with the same BB but a higher statement index
            if let Some(v) = ops.iter()
                .find(|&&(bb,stmt,_)| bb == wr_bb_idx && stmt > wr_stmt_idx) {
                return Some(v);
            }
            // Find this BB in the paths and then look for a write.
            'outer: for p in &paths {
                // Is this starting BB in this path?
                let Some(start) = p.ents.iter().position(|bb| *bb == wr_bb_idx) else { continue };

                //println!("> {:?}", &p.ents[start+1..]);
                for bb_idx in p.ents[start+1..].iter().copied() {
                    if let Some(rv) = check_block(wr_bb_idx, wr_stmt_idx, i, bb_idx) {
                        return Some(rv);
                    }
                    if bb_idx == wr_bb_idx {
                        continue 'outer;
                    }
                }
                // If this path looped, then semi-recurse
                // - Find the loop-back point, and visit all path entries without recursing again
                if let Some(loop_idx) = p.looped {
                    for p in &paths {
                        let Some(start) = p.ents.iter().position(|bb| *bb == loop_idx) else { continue };
            
                        //println!(">> {:?}", &p.ents[start..]);
                        for bb_idx in p.ents[start..].iter().copied() {
                            if let Some(rv) = check_block(wr_bb_idx, wr_stmt_idx, i, bb_idx) {
                                return Some(rv);
                            }
                            if bb_idx == wr_bb_idx {
                                break;
                            }
                        }
                    }
                }

            }
            None
        };
        for &(bb_idx, stmt_idx, ref o) in ops {
            //println!("_{i}: Write {}-{}", bb_idx, stmt_idx);
            if let Op::Write = o {
                match find_usage(bb_idx, stmt_idx, *i) {
                None | Some((_,_,Op::Write)) => {
                    let bb = &mut fcn.blocks[bb_idx];
                    if stmt_idx == bb.statements.len() {
                        let can_remove = match bb.terminator {
                            Terminator::Call(ref call) => {
                                match call.target {
                                crate::mir::CallTarget::Path(_) => None,
                                crate::mir::CallTarget::Intrinsic(ref name, _) => match &name[..] {
                                    "uninit" => Some(call.bb_ret),
                                    _ => None,
                                    },
                                crate::mir::CallTarget::Value(_) => None,
                                }

                            },
                            _ => panic!(),
                            };
                        if let Some(target) = can_remove {
                            println!("remove write at {},{} - terminator {:?}", bb_idx, stmt_idx, bb.terminator);
                            bb.terminator = Terminator::Goto(target);
                            rv = true;
                        }
                    }
                    else {
                        println!("remove write at {},{} - statement {:?}", bb_idx, stmt_idx, bb.statements[stmt_idx]);
                        bb.statements[stmt_idx] = Statement::SpanComment("".into());
                        rv = true;
                    }
                },
                _ => {},
                }
            }
        }
    }

    for bb in &mut fcn.blocks {
        bb.statements.retain(|s| match s {
            Statement::SpanComment(ref c) if c.len() == 0 => false,
            _ => true,
        });
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