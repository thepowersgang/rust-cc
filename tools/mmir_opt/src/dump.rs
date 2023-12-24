use ::std::io::Write;

pub fn dump_tree(dst: &::std::path::Path, tree: &crate::modtree::Root) -> ::std::io::Result<()>
{
    let mut fp = ::std::io::BufWriter::new( ::std::fs::File::create(dst)?);
    for (name, ty) in &tree.types {
        if ty.align == usize::MAX {
            writeln!(fp, "type {};", name)?;
        }
        else {
            writeln!(fp, "type {name} {{")?;
            writeln!(fp, "\tSIZE {}, ALIGN {};", ty.size, ty.align)?;
            for f in &ty.fields {
                write!(fp, "\t{} = {};", f.offset, Ty(&f.ty,0))?;
                if let Some(c) = &f.comment {
                    write!(fp, "\t /*{c}*/")?;
                }
                writeln!(fp, "")?;
            }
            writeln!(fp, "}}")?;
        }
    }

    for (name, def) in &tree.statics {
        if let Some(v) = &def.link_name {
            writeln!(fp, "static {}: {} = @{:?};", name, Ty(&def.ty,0), v)?;
        }
        if let Some(v) = &def.value {
            write!(fp, "static {}: {} = {}", name, Ty(&def.ty,0), Bytes(&v.data))?;
            if ! v.reloc.is_empty() {
                write!(fp, "{{")?;
                for r in &v.reloc {
                    write!(fp, "@{}+{}=", r.ofs, r.size)?;
                    match &r.value {
                    crate::modtree::RelocVal::Symbol(s) => write!(fp, "{s}")?,
                    crate::modtree::RelocVal::Data(d) => write!(fp, "{}", Bytes(d))?,
                    }
                    write!(fp, ", ")?;
                }
                write!(fp, "}}")?;
            }
            writeln!(fp, ";")?;
        }
    }

    for (name, def) in &tree.functions {
        write!(fp, "fn {name}(")?;
        for (name,ty) in Iterator::zip( def.arg_names.iter(), def.sig.args.iter() ) {
            write!(fp, "{name}: {}, ", Ty(ty,0))?;
        }
        if def.sig.is_variadic {
            write!(fp, "...")?;
        }
        write!(fp, ")")?;
        if !def.sig.ret.is_unit() {
            write!(fp, " -> {}", Ty(&def.sig.ret,0))?;
        }
        if let Some(link_name) = &def.link_name {
            write!(fp, " = {link_name:?}:\"\"")?;
        }
        if let Some(b) = &def.body {
            dump_function_body(&mut fp, b, Some(def))?;
        }
        else {
            writeln!(fp, ";")?;
        }
    }

    Ok( () )
}

pub fn dump_function_body(fp: &mut dyn ::std::io::Write, fcn: &crate::mir::Function, def: Option<&crate::modtree::Function>) -> ::std::io::Result<()>
{
    let b = fcn;
    writeln!(fp, " {{")?;
    for (name,ty) in &b.locals {
        writeln!(fp, "\tlet {name}: {ty};", ty=Ty(ty,0))?;
    }
    for (name,val) in &b.drop_flags {
        writeln!(fp, "\tlet {name} = {val};", val=if *val { "1" } else { "0" })?;
    }
    for (i, blk) in b.blocks.iter().enumerate() {
        writeln!(fp, "\t{i}: {{")?;
        for stmt in &blk.statements {
            write!(fp, "\t\t")?;
            match stmt {
            crate::mir::Statement::SpanComment(c) => {
                writeln!(fp, "/*{}*/", c)?;
                continue
                },
            crate::mir::Statement::Assign(dst, rval) => {
                write!(fp, "ASSIGN {} = ", Val(dst,def))?;
                match rval {
                crate::mir::Value::Constant(c) => write!(fp, "{}", C(c))?,
                crate::mir::Value::Use(v) => write!(fp, "={}", Val(v,def))?,
                crate::mir::Value::Borrow(crate::types::Mutability::Shared, slot)
                    => write!(fp, "& {}", Val(slot,def))?,
                crate::mir::Value::Borrow(mutability, slot)
                    => write!(fp, "&{} {}", mutability.to_str(), Val(slot,def))?,
                crate::mir::Value::BinOp(l,o,r) =>
                    write!(fp, "BINOP {l} {o} {r}",
                        l=Val(l,def),
                        r=Val(r,def),
                        o=match o
                            {
                            crate::mir::BinOp::Add => "+",
                            crate::mir::BinOp::Sub => "-",
                            crate::mir::BinOp::Div => "/",
                            crate::mir::BinOp::Mul => "*",
                            crate::mir::BinOp::Rem => "%",
                            crate::mir::BinOp::Shr => ">>",
                            crate::mir::BinOp::Shl => "<<",
                            crate::mir::BinOp::BitAnd => "&",
                            crate::mir::BinOp::BitOr => "|",
                            crate::mir::BinOp::BitXor => "^",
                            crate::mir::BinOp::Less => "<",
                            crate::mir::BinOp::Greater => ">",
                            crate::mir::BinOp::LessEqual => "<=",
                            crate::mir::BinOp::GreaterEqual => ">=",
                            crate::mir::BinOp::Equals => "==",
                            crate::mir::BinOp::NotEquals => "!=",
                            }
                        )?,
                crate::mir::Value::UniOp(op, val)
                    => write!(fp, "UNIOP {op} {v}",
                        v=Val(val, def),
                        op=match op  {
                            crate::mir::UniOp::Inv => "!",
                            crate::mir::UniOp::Neg => "-",
                            },
                        )?,
                crate::mir::Value::Cast(val, ty) => write!(fp, "CAST {} as {}", Val(val,def), Ty(ty,0))?,
                crate::mir::Value::DstPtr(v) => write!(fp, "DSTPTR {}", Val(v,def))?,
                crate::mir::Value::DstMeta(_) => todo!(),
                crate::mir::Value::Tuple(ents) => {
                    write!(fp, "(")?;
                    for e in ents {
                        write!(fp, "{}, ", Val(e, def))?;
                    }
                    write!(fp, ")")?;
                },
                crate::mir::Value::Array(_) => todo!(),
                crate::mir::Value::Struct(_, _) => todo!(),
                crate::mir::Value::UnionVariant(_, _, _) => todo!(),
                crate::mir::Value::EnumVariant(_, _, _) => todo!(),
                }
            },
            }
            writeln!(fp, ";")?;
        }
        write!(fp, "\t\t")?;
        match &blk.terminator {
        crate::mir::Terminator::Removed => panic!(),
        crate::mir::Terminator::Invalid => writeln!(fp, "INCOMPLETE")?,
        crate::mir::Terminator::Return => writeln!(fp, "RETURN")?,
        crate::mir::Terminator::Diverge => writeln!(fp, "DIVERGE")?,
        crate::mir::Terminator::Goto(idx) => writeln!(fp, "GOTO {}", idx)?,
        crate::mir::Terminator::Call(call) => {
            write!(fp, "CALL {} = ", Val(&call.dst,def))?;
            match &call.target {
            crate::mir::CallTarget::Path(p) => write!(fp, "{}", p)?,
            crate::mir::CallTarget::Intrinsic(name, tys) => {
                write!(fp, "{:?}<", name)?;
                for t in tys {
                    write!(fp, "{},", Ty(t,0))?;
                }
                write!(fp, ">")?;
            },
            crate::mir::CallTarget::Value(v) => write!(fp, "({})", Val(v, def))?,
            }
            write!(fp, "(")?;
            for v in &call.args {
                write!(fp, "{},", Val(v, def))?;
            }
            writeln!(fp, ") goto {} else {}", call.bb_ret, call.bb_panic)?;
        },
        crate::mir::Terminator::If(v, bb_true, bb_false)
            => writeln!(fp, "IF {} goto {} else {}", Val(v, def), bb_true, bb_false)?,
        crate::mir::Terminator::SwitchValue(v, vals, targets, bb_default) => {
            writeln!(fp, "SWITCHVALUE {} {{", Val(v, def))?;
            match vals {
            crate::mir::SwitchValues::Signed(vals) => {
                for (v,t) in Iterator::zip(vals.iter(), targets.iter()) {
                    writeln!(fp, "\t\t{:+} = {},", v, t)?;
                }
            }
            crate::mir::SwitchValues::Unsigned(vals) => {
                for (v,t) in Iterator::zip(vals.iter(), targets.iter()) {
                    writeln!(fp, "\t\t{:+} = {},", v, t)?;
                }
            }
            crate::mir::SwitchValues::Float(vals) => {
                for (v,t) in Iterator::zip(vals.iter(), targets.iter()) {
                    writeln!(fp, "\t\t{:+} = {},", v, t)?;
                }
            }
            crate::mir::SwitchValues::String(vals) => {
                for (v,t) in Iterator::zip(vals.iter(), targets.iter()) {
                    writeln!(fp, "\t\t{} = {},", Bytes(v), t)?;
                }
            }
            }
            writeln!(fp, "\t\t_ = {}", bb_default)?;
            writeln!(fp, "\t\t}}")?;
            },
        }
        writeln!(fp, "\t}}")?;
    }
    writeln!(fp, "}}")?;
    Ok( () )
}

struct Bytes<'a>(&'a [u8]);
impl ::std::fmt::Display for Bytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\"")?;
        for &b in self.0 {
            match b {
            0 => f.write_str("\\0")?,
            b'\\' => f.write_str("\\\\")?,
            b'"' => f.write_str("\\\"")?,
            b' ' ..= 0x7F => (b as char).fmt(f)?,
            b => write!(f, "\\x{:02x}", b)?,
            }
        }
        f.write_str("\"")?;
        Ok( () )
    }
}

struct Ty<'a>(&'a crate::types::TypeRef, usize);
impl<'a> Ty<'a> {
    fn inner(&self) -> Ty<'a> {
        assert!(self.1 < self.0.wrappers.len());
        Ty(self.0, self.1+1)
    }
}
impl ::std::fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1 < self.0.wrappers.len() {
            let w = &self.0.wrappers[ self.0.wrappers.len() - 1 - self.1 ];
            match w {
            crate::types::Wrapper::Slice => write!(f, "[{}]", self.inner()),
            crate::types::Wrapper::Array(c) => write!(f, "[{}; {c}]", self.inner()),
            crate::types::Wrapper::Pointer(m) => write!(f, "*{} {}", m.to_str(), self.inner()),
            crate::types::Wrapper::Borrow(crate::types::Mutability::Shared) => write!(f, "&{}", self.inner()),
            crate::types::Wrapper::Borrow(m) => write!(f, "&{} {}", m.to_str(), self.inner()),
            }
        }
        else {
            match &self.0.root {
            crate::types::Root::Diverge => f.write_str("!"),
            crate::types::Root::Str => f.write_str("str"),
            crate::types::Root::Unsigned(bits) => write!(f, "u{bits}"),
            crate::types::Root::Signed(bits) => write!(f, "i{bits}"),
            crate::types::Root::Float(bits) => write!(f, "f{bits}"),
            crate::types::Root::Named(name) => f.write_str(name),
            crate::types::Root::Tuple(ents) => {
                f.write_str("(")?;
                for t in ents {
                    Ty(t,0).fmt(f)?;
                    f.write_str(",")?;
                }
                f.write_str(")")
            },
            crate::types::Root::Function(fcn) => {
                if fcn.abi != "" {
                    write!(f, "extern {:?}", fcn.abi)?;
                }
                f.write_str("fn(")?;
                for t in &fcn.args {
                    Ty(t,0).fmt(f)?;
                    f.write_str(",")?;
                }
                f.write_str(")")?;
                if ! fcn.ret.is_unit() {
                    f.write_str("->")?;
                    Ty(&fcn.ret,0).fmt(f)?;
                }
                Ok( () )
            },
            }
        }
    }
}

struct Val<'a, T>(&'a T, Option<&'a crate::modtree::Function>);
impl<'a,T> Val<'a, T> {
    pub fn get_arg(&self, i: usize) -> ::std::borrow::Cow<str> {
        if let Some(d) = self.1 {
            d.arg_names[i][..].into()
        }
        else {
            format!("a{}", i).into()
        }
    }
    pub fn get_local(&self, i: usize) -> ::std::borrow::Cow<str> {
        if let Some(d) = self.1 {
            d.body.as_ref().unwrap().locals[i].0[..].into()
        }
        else {
            format!("_{}", i).into()
        }
    }
}
impl<'a> ::std::fmt::Display for Val<'a, crate::mir::Slot> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for w in &self.0.wrappers {
            match w {
            crate::mir::SlotWrapper::Deref => f.write_str("(*")?,
            _ => {},
            }
        }
        match self.0.root {
        crate::mir::SlotRoot::Named(ref name) => f.write_str(name)?,
        crate::mir::SlotRoot::Argument(i) => f.write_str(&self.get_arg(i))?,
        crate::mir::SlotRoot::Local(i) => f.write_str(&self.get_local(i))?,
        crate::mir::SlotRoot::Return => f.write_str("RETURN")?,
        }
        for w in &self.0.wrappers {
            match w {
            //crate::mir::SlotWrapper::Deref => f.write_str(".*")?,
            crate::mir::SlotWrapper::Deref => f.write_str(")")?,
            crate::mir::SlotWrapper::Index(i) => write!(f, "[{}]", self.get_local(*i))?,
            crate::mir::SlotWrapper::Field(i) => write!(f, " .{}", i)?,
            crate::mir::SlotWrapper::Downcast(i) => write!(f, "#{}", i)?,
            }
        }
        Ok( () )
    }
}
impl<'a> ::std::fmt::Display for Val<'a, crate::mir::Param> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
        crate::mir::Param::Const(c) => C(c).fmt(f),
        crate::mir::Param::Slot(s) => Val(s,self.1).fmt(f),
        }
    }
}

struct C<'a>(&'a crate::mir::Const);
impl ::std::fmt::Display for C<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
        crate::mir::Const::Boolean(true) => f.write_str("true"),
        crate::mir::Const::Boolean(false) => f.write_str("false"),
        crate::mir::Const::Unsigned(val, bits) => write!(f, "{} u{}", val, bits),
        crate::mir::Const::Signed(val, bits) => write!(f, "{:+} i{}", val, bits),
        crate::mir::Const::Float(_, _) => todo!(),
        crate::mir::Const::String(v) => write!(f, "{}", Bytes(&v.as_bytes())),
        crate::mir::Const::ItemAddr(v) => write!(f, "ADDROF {}", v),
        }
    }
}