use crate::mir::Terminator;
use crate::mir::Statement;
use crate::logger::Logger;

pub fn optimise_function(logger: &mut Logger, fcn: &mut crate::mir::Function, _sig: &crate::types::FcnTy)
{
    loop {
        log_debug!(logger, "-- PASS");
        let mut changed = false;
        // - Simplify flow control: If a block is just a goto, replace uses of it with the target
        while simplify_control_flow(logger, fcn) {
            changed = true;
            crate::dump::dump_function_body(&mut logger.writer(), fcn, None).unwrap();
        }
        // - Redundant Pointer Casts
        //  > Two casts in a row can be simplfied into just the latter (if the initial source is a pointer or usize)
        //  * This happens when `NULL` is cast to something.
        #[cfg(any())]
        while remove_redundant_casts(logger, fcn, sig) {
            changed = true;
            crate::dump::dump_function_body(&mut logger.writer(), fcn, None).unwrap();
        }
        // - Single write/use temporaries
        #[cfg(any())]
        while remove_single_use(logger, fcn) {
            changed = true;
            crate::dump::dump_function_body(&mut logger.writer(), fcn, None).unwrap();
        }
        // - Unused writes
        while remove_dead_writes(logger, fcn) {
            changed = true;
            crate::dump::dump_function_body(&mut logger.writer(), fcn, None).unwrap();
        }
        // - Constant propagation
        //  > Replace casts with literals
        //  > Propagate through to operators
        while const_propagate(logger, fcn) {
            changed = true;
            crate::dump::dump_function_body(&mut logger.writer(), fcn, None).unwrap();
        }
        if !changed {
            break
        }
    }

    log_debug!(logger, "-- Cleanup");
    crate::dump::dump_function_body(&mut logger.writer(), fcn, None).unwrap();
    // Delete unused blocks
    clean_unused_blocks(logger, fcn);
    //println!(">>");
    //crate::dump::dump_function_body(&mut ::std::io::stdout(), fcn, None).unwrap();
    simplify_control_flow(logger, fcn);
}

fn simplify_control_flow(logger: &mut Logger, fcn: &mut crate::mir::Function) -> bool
{
    log_debug!(logger, "simplify_control_flow");
    let rewrites: Vec<_> = fcn.blocks.iter()
        .map(|bb| {
            match bb.terminator {
            Terminator::Goto(v) if bb.statements.is_empty() => Some(v),
            _ => None,
            }
        })
        .collect();
    let mut rv = false;
    helpers::visit_block_targets_mut(fcn, |tgt: &mut usize| if let Some(new) = rewrites[*tgt] {
        *tgt = new;
        rv = true;
    });
    rv
}

mod const_propagate;
use const_propagate::const_propagate;

fn remove_dead_writes(logger: &mut Logger, fcn: &mut crate::mir::Function) -> bool
{
    log_debug!(logger, "remove_dead_writes");
    // Identify writes to variables that are never read, just overwritten
    // - This catches `"uninit"` assignments, AND just dead statements

    /*
    #[derive(Clone)]
    struct Path(Vec<u32>);
    impl Path {
        pub fn contains(&self, idx: usize) -> Option<usize> {
            self.0.iter().position(|v| *v == idx as u32)
        }
    }
    fn path_contains(p: &[&u32], idx: usize) -> Option<usize> {
        p.iter().position(|&&v| v == idx as u32)
    }
    type PathSet = crate::helper_types::Trie<u32>;
    fn enumerate_paths_for_bb(logger: &Logger, fcn: &crate::mir::Function, bb_idx: usize) -> PathSet {
        let mut paths = PathSet::new();
        let mut stack: Vec<(usize, Path)> = Vec::new();
        stack.push((bb_idx, Path(Vec::new())));
        while let Some( (idx, path)) = stack.pop() {
            let path = helpers::visit_terminator_targets(&fcn.blocks[idx].terminator, path, |mut path,idx| {
                if ! path.contains(idx).is_some() {
                    path.0.push(idx as u32);
                    stack.push((idx, path));
                }
                else {
                    path.0.push(idx as u32);
                    //log_debug!(logger, "Paths BB{}: {:?}", bb_idx, path.0);
                    paths.push(&path.0);
                }
            });
            if let Some(path) = path {
                //log_debug!(logger, "Paths BB{}: {:?}", bb_idx, path.0);
                paths.push(&path.0);
            }
        }
        log_debug!(logger, "Paths BB{}", bb_idx);
        paths.dump();
        paths
    }
    let paths_from_each_bb: Vec<_> = (0 .. fcn.blocks.len()).map(|bb_idx| {
        log_debug!(logger, "Paths BB{}", bb_idx);
        enumerate_paths_for_bb(logger, fcn, bb_idx)
        }).collect();
    */

    // - Enumerate reads/writes of each variable (locations)
    //  > For each write, see if there is a read before the next write (along the path)
    #[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
    enum Op {
        Read,
        Write,
    }
    use crate::{LineRef,StmtIdx};
    use ::std::collections::BTreeMap;
    let mut ops = BTreeMap::<usize,Vec<(LineRef,Op)>>::new();
    for (bb_idx,bb) in fcn.blocks.iter().enumerate()
    {
        fn visit_read(ops: &mut BTreeMap<usize,Vec<(LineRef,Op)>>, slot: &crate::mir::Slot, lr: &LineRef) {
            for w in &slot.wrappers {
                if let crate::mir::SlotWrapper::Index(i) = *w {
                    ops.entry(i).or_default().push((lr.clone(), Op::Read));
                }
            }
            if let crate::mir::SlotRoot::Local(i) = slot.root {
                ops.entry(i).or_default().push((lr.clone(), Op::Read));
            }
        }
        fn visit_write(ops: &mut BTreeMap<usize,Vec<(LineRef,Op)>>, slot: &crate::mir::Slot, lr: LineRef) {
            for w in &slot.wrappers {
                if let crate::mir::SlotWrapper::Index(i) = *w {
                    ops.entry(i).or_default().push((lr.clone(), Op::Read));
                }
            }
            if let Some(i) = slot.is_local() {
                ops.entry(i).or_default().push((lr, Op::Write));
            }
            else {
                // Any other usage is assumed to be a read
                if let crate::mir::SlotRoot::Local(i) = slot.root {
                    ops.entry(i).or_default().push((lr, Op::Read));
                }
            }
        }

        for (stmt_idx, stmt) in bb.statements.iter().enumerate() {
            match stmt {
            Statement::SpanComment(_) => {},
            Statement::Assign(dst, src) => {
                // Look for usage
                let lr = LineRef { bb_idx, stmt_idx: StmtIdx::Stmt(stmt_idx.try_into().expect("Too many statements?!")) };
                helpers::visit_slot_in_value(src, |slot| {
                    visit_read(&mut ops, slot, &lr);
                });

                visit_write(&mut ops, dst, lr);
            }
            }
        }
        let lr = LineRef { bb_idx, stmt_idx: StmtIdx::Term };
        match bb.terminator {
        Terminator::Removed => {},
        Terminator::Invalid => {},
        Terminator::Return => {},
        Terminator::Diverge => {},
        Terminator::Goto(_) => {},
        Terminator::Call(ref call) => {

            match &call.target {
            crate::mir::CallTarget::Path(_) => {},
            crate::mir::CallTarget::Intrinsic(_, _) => {},
            crate::mir::CallTarget::Value(slot) => {
                visit_read(&mut ops, slot, &lr);
            },
            }

            for p in &call.args {
                helpers::visit_slot_in_param(p, |slot| {
                    visit_read(&mut ops, slot, &lr);
                });
            }
            
            visit_write(&mut ops, &call.dst, lr);
        }
        Terminator::If(ref slot, _, _) => {
            visit_read(&mut ops, slot, &lr);
        }
        Terminator::SwitchValue(ref slot, _, _, _) => {
            visit_read(&mut ops, slot, &lr);
        }
        }
    }

    for (_,o) in ops.iter_mut() {
        o.sort();
    }
    let mut rv = false;
    for (&var_idx, ops) in &ops {
        log_debug!(logger, "{var_idx} {ops:?}");
        let find_usage = |fcn: &crate::mir::Function, wr_pos: &LineRef, var_idx: usize| {
            let _ = var_idx;
            // Look for a read/write with the same BB but a higher statement index
            // - Assumes the list is sorted
            if let Some(v) = ops.iter()
                .find(|(lr,_)| lr.bb_idx == wr_pos.bb_idx && lr.stmt_idx > wr_pos.stmt_idx) {
                return Some(v);
            }

            let mut found_read = None;
            let mut found_write = None;
            helpers::visit_path_from_bb(logger, fcn, wr_pos.bb_idx, |_path_index, bb_idx| {
                for ops_ent @ (lr,op) in ops.iter() {
                    if lr.bb_idx == bb_idx {
                        log_debug!(logger, ">> {_path_index} {op:?} @ {lr}");
                        match op {
                        Op::Read => {
                            found_read = Some(ops_ent);
                            // Any read (before a write) from this is an immediate exit
                            return helpers::PathVisit::StopAll;
                            }
                        Op::Write => {
                            found_write = Some(ops_ent);
                            return helpers::PathVisit::StopArm;
                            }
                        }
                    }
                }
                return helpers::PathVisit::Continue;
            });
            if let Some(r) = found_read {
                return Some(r);
            }
            // If there was no write, then return not-found
            return None;
        };
        for (lr, o) in ops.iter() {
            if let Op::Write = o {
                let try_remove = if ops.iter().all(|(_,o)| matches!(o, Op::Write)) {
                        // Only use is a write? Remove
                        log_debug!(logger, "_{var_idx}: Write {lr} - Never read; remove");
                        true
                    }
                    else if ops.iter().filter(|(_,o)| matches!(o, Op::Write)).count() == 1 {
                        // Only one write (and non-zero reads)? Don't remove
                        log_debug!(logger, "_{var_idx}: Write {lr} - One write, has reads; keep");
                        false
                    }
                    else {
                        log_debug!(logger, "_{var_idx}: Write {lr} - Check");
                        // Otherwise, there's multiple writes - so we need to determine if this is invalidated
                        match find_usage(fcn, lr, var_idx) {
                        None | Some((_,Op::Write)) => true,
                        _ => false
                        }
                    };
                if try_remove {
                    let bb = &mut fcn.blocks[lr.bb_idx];
                    match lr.stmt_idx {
                    StmtIdx::Term => {
                        let can_remove = match bb.terminator {
                            Terminator::Call(ref call) => {
                                match call.target {
                                crate::mir::CallTarget::Path(_) => None,
                                crate::mir::CallTarget::Intrinsic(ref name, _) => match &name[..] {
                                    // These intrinsics have no side-effects (not the case with all intrinsics)
                                    "uninit"
                                    |"init"
                                    |"offset"
                                        => Some(call.bb_ret),
                                    _ => None,
                                    },
                                crate::mir::CallTarget::Value(_) => None,
                                }

                            },
                            _ => panic!(),
                            };
                        if let Some(target) = can_remove {
                            log_debug!(logger, "remove write at {lr} - terminator {:?}", bb.terminator);
                            bb.terminator = Terminator::Goto(target);
                            rv = true;
                        }
                        else {
                            log_debug!(logger, "CANNOT remove write at {lr} - terminator {:?}", bb.terminator);
                        }
                        },
                    StmtIdx::Stmt(stmt_idx) => {
                        log_debug!(logger, "remove write at {lr} - statement {:?}", bb.statements[stmt_idx as usize]);
                        bb.statements[stmt_idx as usize] = Statement::SpanComment("".into());
                        rv = true;
                        }
                    }
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

fn clean_unused_blocks(logger: &mut Logger, fcn: &mut crate::mir::Function)
{
    log_debug!(logger, "clean_unused_blocks");
    // Determine if each block is referenced
    loop {
        let mut used: Vec<_> = (0..fcn.blocks.len()).map(|_| false).collect();
        used[0] = true;
        helpers::visit_block_targets_mut(fcn, |&mut tgt| used[tgt] = true);
        let mut changed = false;
        for (used,bb) in Iterator::zip(used.into_iter(), fcn.blocks.iter_mut()) {
            match bb.terminator {
            Terminator::Removed if bb.statements.is_empty() => {},
            _ if !used => {
                bb.statements.clear();
                bb.terminator = Terminator::Removed;
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
            if let Terminator::Removed = bb.terminator {
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
        Terminator::Removed => false,
        _ => true,
    });
    helpers::visit_block_targets_mut(fcn, |tgt: &mut usize| *tgt = match mapping[*tgt]
        {
        Some(v) => v,
        None => log_panic!(logger, "Block {tgt} still referenced but was deleted"),
        });
}

mod helpers {
    use crate::mir::{Function,Terminator, Value};
    pub fn visit_block_targets_mut(fcn: &mut Function, mut check: impl FnMut(&mut usize)) {
        for bb in &mut fcn.blocks {
            match &mut bb.terminator {
            Terminator::Removed => {},
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
            Terminator::SwitchValue(_, _, targets, bb_default) => {
                for t in targets {
                    check(t);
                }
                check(bb_default);
            }
            }
        }
    }

    pub fn visit_slot_in_param(param: &crate::mir::Param, mut fcn: impl FnMut(&crate::mir::Slot)) {
        if let crate::mir::Param::Slot(s) = param {
            fcn(s);
        }
    }
    pub fn visit_slot_in_value(value: &Value, mut fcn: impl FnMut(&crate::mir::Slot)) {
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

    pub fn visit_terminator_targets<S: Clone>(term: &Terminator, state: S, mut check: impl FnMut(S, usize)) -> Option<S>
    {
        match *term {
        Terminator::Removed => Some(state),
        Terminator::Invalid => Some(state),
        Terminator::Return => Some(state),
        Terminator::Diverge => Some(state),
        Terminator::Goto(blk) => {
            check(state, blk);
            None
            },
        Terminator::Call(ref call) => {
            check(state.clone(), call.bb_ret);
            check(state, call.bb_panic);
            None
        },
        Terminator::If(_, bb_true, bb_false) => {
            check(state.clone(), bb_true);
            check(state, bb_false);
            None
        },
        Terminator::SwitchValue(_, _, ref targets, bb_default) => {
            for &t in targets {
                check(state.clone(), t);
            }
            check(state, bb_default);
            None
        }
        }
    }


    // If `cb` returns true, the path is stopped
    pub enum PathVisit {
        Continue,
        StopArm,
        StopAll,
    }
    pub fn visit_path_from_bb(logger: &crate::logger::Logger, fcn: &crate::mir::Function, bb_idx: usize, mut cb: impl FnMut(usize, usize)->PathVisit) {
        let _ = logger;
        struct Counter(::std::cell::Cell<usize>);
        impl Counter {
            fn get(&self) -> usize {
                let rv = self.0.get();
                self.0.set(rv + 1);
                rv
            }
        }
        struct State<'a> {
            state_idx: usize,
            next_state_idx: &'a Counter,
            cur_bb: u32,
            visited: crate::helper_types::Bitmap,
        }
        impl Clone for State<'_> {
            fn clone(&self) -> Self {
                Self {
                    state_idx: self.next_state_idx.get(),
                    next_state_idx: self.next_state_idx,
                    cur_bb: self.cur_bb,
                    visited: self.visited.clone()
                }
            }
        }
        let next_state_idx = Counter(Default::default());
        let mut visited = crate::helper_types::Bitmap::new(fcn.blocks.len());
        visited.set(bb_idx);
        let mut stack: Vec<State<'_>> = Vec::new();
        stack.push(State {
            state_idx: next_state_idx.get(),
            next_state_idx: &next_state_idx,
            cur_bb: bb_idx as u32,
            visited,
        });
        let mut abort = false;
        while let Some(s) = stack.pop() {
            if abort {
                break;
            }
            //log_debug!(logger, "-- {}: BB{}", s.state_idx, s.cur_bb);
            visit_terminator_targets(&fcn.blocks[s.cur_bb as usize].terminator, s, |mut s, idx| {
                match cb(s.state_idx, idx)
                {
                PathVisit::StopAll => {
                    abort = true;
                },
                PathVisit::StopArm => {
                    // Stop processing this state, just let `s` drop
                    },
                PathVisit::Continue => {
                    if s.visited.get(idx) {
                        // We've already visited this block
                    }
                    else {
                        s.visited.set(idx);
                        s.cur_bb = idx as u32;
                        stack.push(s);
                    }
                    }
                }
                });
        }
    }
}
