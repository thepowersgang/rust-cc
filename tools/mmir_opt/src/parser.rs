use crate::lexer::{Lexer,Token};
use ::std::collections::btree_map::Entry;

pub fn parse_file(dst: &mut crate::modtree::Root, path: &::std::path::Path) {
    let inner = ::std::fs::read_to_string(path).expect("Unable to open file");
    let mut lexer = Lexer::new(path, &inner);
    let lex = &mut lexer;

    loop {
        match match lex.get_tok()
            {
            None => break,
            Some(Token::Ident(i)) => i,
            Some(t) => panic!("{lex}: Unexpected {t:?}, expected Ident(_)"),
            }
        {
        "fn" => {
            let define_location = format!("{lex}");
            let name = lex.consume_ident().to_owned();
            lex.consume_sym("(");
            let mut arg_names = Vec::new();
            let mut arg_tys = Vec::new();
            let mut is_variadic = false;
            while !lex.consume_if_sym(")") {
                if lex.consume_if_sym("...") {
                    is_variadic = true;
                    lex.consume_sym(")");
                    break;
                }
                let name = if lex.consume_if_sym("_") { "" } else { lex.consume_ident() };
                lex.consume_sym(":");
                let ty = parse_type(lex);
                arg_names.push(name.to_string());
                arg_tys.push(ty);

                if !lex.consume_if_sym(",") {
                    lex.consume_sym(")");
                    break;
                }
            }
            let ret_ty = if lex.consume_if_sym(":") || lex.consume_if_sym("->") {
                    parse_type(lex)
                }
                else {
                    crate::types::TypeRef::unit()
                };
            let (link_name, abi) = if lex.consume_if_sym("=") {
                    let link_name = lex.consume_str().parse_string();
                    lex.consume_sym(":");
                    let abi = lex.consume_str().parse_string();
                    (Some(link_name), abi)
                }
                else {
                    (None,String::new())
                };
            let body = if lex.consume_if_sym(";") {
                    None
                }
                else {
                    lex.consume_sym("{");
                    Some(parse_function_body(lex, &arg_names))
                };

            let fcn = crate::modtree::Function {
                define_location,
                link_name,
                sig: crate::types::FcnTy {
                    abi,
                    args: arg_tys,
                    ret: ret_ty,
                    is_variadic,
                },
                body,
                arg_names,
            };

            match dst.functions.entry(name)
            {
            Entry::Occupied(mut e) => {
                let name = e.key().clone();
                let exist = e.get_mut();
                if exist.arg_names != fcn.arg_names {
                    // Warning
                }
                if exist.sig != fcn.sig {
                    // Warning
                    panic!("{lex}: Conflicting definition of function {}", e.key())
                }
                match exist.body {
                None => {
                    exist.define_location = fcn.define_location;
                    exist.arg_names = fcn.arg_names;
                    exist.body = fcn.body;
                    },
                Some(ref mut body) => {
                    if let Some(ref b) = fcn.body {
                        if b != body {
                            panic!("{lex}: Re-definition of function {} with different body\n{}: Previous definition was here",
                                name, exist.define_location);
                        }
                    }
                    else {
                    }
                    },
                }
            },
            Entry::Vacant(e) => { e.insert(fcn); },
            }
        },
        "type" => {
            let name = lex.consume_ident().to_string();
            let ty = if lex.consume_if_sym(";") {
                crate::modtree::Type {
                    size: usize::MAX,
                    align: usize::MAX,
                    fields: Vec::new(),
                }
            }
            else {
                lex.consume_sym("{");
                lex.consume_keyword("SIZE");
                let size = lex.consume_int() as usize;
                lex.consume_sym(",");
                lex.consume_keyword("ALIGN");
                let align = lex.consume_int() as usize;
                lex.consume_sym(";");
                // TODO: Drop glue
    
                let mut fields = Vec::new();
                while !lex.consume_if_sym("}") {
    
                    if lex.consume_if_keyword("DROP") {
                        todo!("");
                    }
    
                    let offset = lex.consume_int() as usize;
                    lex.consume_sym("=");
                    let ty = parse_type(lex);
                    lex.consume_sym(";");
                    let comment = lex.consume_comment().map(String::from);
                    fields.push(crate::modtree::Field { offset, ty, comment });
                }
    
                crate::modtree::Type {
                    size,
                    align,
                    fields,
                }
            };
            match dst.types.entry(name)
            {
            Entry::Occupied(e) => {
                println!("{lex}: Redefinition of type {}", e.key())
            },
            Entry::Vacant(e) => { e.insert(ty); },
            }
        },
        "static" => {
            let define_location = format!("{lex}");
            let name = lex.consume_ident().to_owned();
            lex.consume_sym(":");
            let ty = parse_type(lex);
            lex.consume_sym("=");
            let mut s = crate::modtree::Static {
                define_location,
                link_name: None,
                ty,
                value: None,
            };
            if lex.consume_if_sym("@") {
                s.link_name = Some( lex.consume_str().parse_string() );
            }
            else {
                let data = lex.consume_str().parse_bytes();
                let reloc = if lex.consume_if_sym("{") {
                        let mut relocs = Vec::new();
                        while !lex.consume_if_sym("}") {
                            lex.consume_sym("@");
                            let ofs = lex.consume_int() as usize;
                            lex.consume_sym("+");
                            let len = lex.consume_int() as usize;
                            lex.consume_sym("=");
                            let value = match lex.get_tok_noeof() {
                                Token::String(s) => crate::modtree::RelocVal::Data(s.parse_bytes()),
                                Token::Ident(s) => crate::modtree::RelocVal::Symbol(s.to_owned()),
                                t => panic!("{lex}: Unexpected token in reloc {t:?}"),
                                };
                            relocs.push(crate::modtree::Reloc { ofs, size: len, value });

                            if !lex.consume_if_sym(",") {
                                lex.consume_sym("}");
                                break;
                            }
                        }
                        relocs
                    }
                    else {
                        Vec::new()
                    };
                s.value = Some(crate::modtree::StaticValue {
                    data,
                    reloc,
                });
            }
            lex.consume_sym(";");
            match dst.statics.entry(name)
            {
            Entry::Occupied(mut e) => {
                let name = e.key().clone();
                let existing = e.get_mut();
                if existing.link_name.is_none() {
                    existing.link_name = s.link_name;
                }
                else if s.link_name.is_none() {
                    if s.link_name != existing.link_name {
                        // Uh-oh
                    }
                }
                else {
                }
                if existing.value.is_none() {
                    existing.define_location = s.define_location;
                    existing.value = s.value;
                }
                else if s.value.is_some() {
                    panic!("{lex}: Re-definition of static {}\n{} previous definition was here", name, existing.define_location);
                }
                else {
                }
            }
            Entry::Vacant(e) => { e.insert(s); },
            }
        },
        t => panic!("Unexpected Ident({t:?}), expected `fn`, `type`, or `static`"),
        }
    }
}

fn parse_type(lex: &mut Lexer) -> crate::types::TypeRef {
    use crate::types::{TypeRef,Root,Bits, Wrapper, Mutability};
    match lex.get_tok_noeof()
    {
    Token::Ident("extern") => {
        let abi = lex.consume_str().parse_string();
        lex.consume_keyword("fn");
        TypeRef::root(Root::Function(Box::new( parse_type_fn(lex, Some(abi)) )))
    },
    Token::Ident("fn") => TypeRef::root(Root::Function(Box::new( parse_type_fn(lex, None) ))),
    Token::Ident("dyn") => todo!(),
    Token::Ident(i) => TypeRef::root(match i
        {
        "str" => Root::Str,
        "usize" => Root::Unsigned(Bits::SIZE),
        "isize" => Root::Signed(Bits::SIZE),
        "u8"   => Root::Unsigned(Bits::_8),
        "u16"  => Root::Unsigned(Bits::_16),
        "u32"  => Root::Unsigned(Bits::_32),
        "u64"  => Root::Unsigned(Bits::_64),
        "u128" => Root::Unsigned(Bits::_128),
        "i8"   => Root::Signed(Bits::_8),
        "i16"  => Root::Signed(Bits::_16),
        "i32"  => Root::Signed(Bits::_32),
        "i64"  => Root::Signed(Bits::_64),
        "i128" => Root::Signed(Bits::_128),
        "f32" => Root::Float(Bits::_32),
        "f64" => Root::Float(Bits::_64),
        _ => Root::Named(i.to_string()),
        }),
    Token::Sym("!") => TypeRef::root(Root::Diverge),
    Token::Sym("[") => {
        let inner = parse_type(lex);
        inner.wrapped(if lex.consume_if_sym(";") {
            let size = lex.consume_int() as usize;
            lex.consume_sym("]");
            Wrapper::Array(size)
        }
        else {
            lex.consume_sym("]");
            Wrapper::Slice
        })
    },
    Token::Sym("(") => {
        let mut tys = Vec::new();
        while !lex.consume_if_sym(")") {
            tys.push(parse_type(lex));
            if !lex.consume_if_sym(",") {
                lex.consume_sym(")");
            }
        }
        TypeRef::root(Root::Tuple(tys))
    },
    Token::Sym("*") => {
        let mutability = match lex.consume_ident()
            {
            "const" => Mutability::Shared,
            "mut" => Mutability::Unique,
            "move" => Mutability::Move,
            i => panic!("{lex}: Unexpected `{}`, expected const/mut/move", i)
            };
        let inner = parse_type(lex);
        inner.wrapped(Wrapper::Pointer(mutability))
    },
    Token::Sym("&") => {
        let mutability = if lex.consume_if_keyword("mut") {
            Mutability::Unique
        }
        else if lex.consume_if_keyword("move") {
            Mutability::Move
        }
        else {
            Mutability::Shared
        };
        let inner = parse_type(lex);
        inner.wrapped(Wrapper::Borrow(mutability))
    },
    t => panic!("Unexpected {t:?} in type position"),
    }
}
fn parse_type_fn(lex: &mut Lexer, abi: Option<String>) -> crate::types::FcnTy {
    lex.consume_sym("(");
    let mut args = Vec::new();
    let mut is_variadic = false;
    while ! lex.consume_if_sym(")") {
        if lex.consume_if_sym("...") {
            is_variadic = true;
            lex.consume_sym(")");
            break;
        }
        args.push(parse_type(lex));
        if !lex.consume_if_sym(",") {
            lex.consume_sym(")");
            break;
        }
    }
    let ret = if lex.consume_if_sym("->") {
        parse_type(lex)
    }
    else {
        crate::types::TypeRef::unit()
    };
    crate::types::FcnTy {
        abi: abi.unwrap_or("".to_owned()),
        args,
        is_variadic,
        ret,
    }
}

fn parse_function_body(lex: &mut Lexer, arg_names: &[String]) -> crate::mir::Function {
    use crate::mir::{Statement, Value, Terminator};

    let mut rv = crate::mir::Function {
        locals: Vec::new(),
        drop_flags: Vec::new(),
        blocks: Vec::new(),
    };
    while lex.consume_if_keyword("let") {
        let var_name = lex.consume_ident().to_owned();
        lex.consume_sym(":");
        let var_ty = parse_type(lex);
        lex.consume_sym(";");
        rv.locals.push((var_name, var_ty));
    }

    let lookup = FcnLookup { args: &arg_names, vars: &rv.locals };

    while let Some(bb_num) = lex.consume_if_int() {
        assert!(bb_num == rv.blocks.len() as _);
        lex.consume_sym(":");
        lex.consume_sym("{");
        let mut stmts = Vec::new();
        let term = loop {
            if let Some(c) = lex.consume_comment() {
                stmts.push(crate::mir::Statement::SpanComment(c.to_owned()));
                continue;
            }
            match lex.consume_ident()
            {
            "ASSIGN" => {
                let dst = lookup.parse_slot(lex);
                lex.consume_sym("=");
                let saved = lex.clone();
                let val = match lex.get_tok_noeof() {
                    // Composite Literals
                    Token::Sym("(") => {
                        let mut ents: Vec<crate::mir::Param> = Vec::new();
                        while ! lex.consume_if_sym(")") {
                            ents.push( lookup.parse_param(lex) );
                            if !lex.consume_if_sym(",") {
                                lex.consume_sym(")");
                            }
                        }
                        Value::Tuple(ents)
                    },
                    Token::Sym("[") => {
                        if lex.consume_if_sym("]") {
                            Value::Array(Vec::new())
                        }
                        else {
                            let ent1 = lookup.parse_param(lex);
                            if lex.consume_if_sym(";") {
                                todo!("Sized array literal")
                            }
                            else {
                                let mut ents: Vec<crate::mir::Param> = Vec::new();
                                ents.push(ent1);
                                while lex.consume_if_sym(",") {
                                    ents.push( lookup.parse_param(lex) );
                                }
                                lex.consume_sym("]");
                                Value::Array(ents)
                            }
                        }
                    },
                    Token::Sym("{") => {
                        let mut ents: Vec<crate::mir::Param> = Vec::new();
                        while ! lex.consume_if_sym("}") {
                            ents.push( lookup.parse_param(lex) );
                            if !lex.consume_if_sym(",") {
                                lex.consume_sym("}");
                            }
                        }
                        lex.consume_sym(":");
                        let ty = lex.consume_ident().to_owned();
                        Value::Struct(ty, ents)
                    },
                    Token::Ident("ENUM") => {
                        let ty = lex.consume_ident().to_owned();
                        let idx = lex.consume_int() as usize;
                        let mut ents: Vec<crate::mir::Param> = Vec::new();
                        while ! lex.consume_if_sym("}") {
                            ents.push( lookup.parse_param(lex) );
                            if !lex.consume_if_sym(",") {
                                lex.consume_sym("}");
                            }
                        }
                        Value::EnumVariant(ty, idx, ents)
                    },
                    Token::Ident("UNION") => {
                        let ty = lex.consume_ident().to_owned();
                        let idx = lex.consume_int() as usize;
                        let val = lookup.parse_param(lex);
                        Value::UnionVariant(ty, idx, val)
                    },

                    // Operations
                    Token::Ident("UNIOP") => {
                        let op = match lex.get_tok_noeof() {
                            Token::Sym("!") => crate::mir::UniOp::Inv,
                            Token::Sym("-") => crate::mir::UniOp::Neg,
                            t => panic!("{lex}: Unexpected token for UNIOP - {t:?}"),
                            };
                        let val = lookup.parse_slot(lex);
                        Value::UniOp(op, val)
                    },
                    Token::Ident("BINOP") => {
                        use crate::mir::BinOp;
                        let lhs = lookup.parse_param(lex);
                        let op = match lex.get_tok_noeof()
                            {
                            Token::Sym("+") => BinOp::Add,
                            Token::Sym("-") => BinOp::Sub,
                            Token::Sym("*") => BinOp::Mul,
                            Token::Sym("/") => BinOp::Div,
                            Token::Sym("%") => BinOp::Rem,
                            Token::Sym("<") => BinOp::Less, Token::Sym("<=") => BinOp::LessEqual,
                            Token::Sym(">") => BinOp::Greater, Token::Sym(">=") => BinOp::GreaterEqual,
                            Token::Sym("==") => BinOp::Equals, Token::Sym("!=") => BinOp::NotEquals,

                            Token::Sym("&") => BinOp::BitAnd,
                            Token::Sym("|") => BinOp::BitOr,
                            Token::Sym("^") => BinOp::BitXor,

                            Token::Sym("<<") => BinOp::Shl,
                            Token::Sym(">>") => BinOp::Shr,
                            t => todo!("{lex}: BinOp {t:?}"),
                            };
                        let rhs = lookup.parse_param(lex);
                        Value::BinOp(lhs, op, rhs)
                    },
                    Token::Ident("CAST") => {
                        let val = lookup.parse_slot(lex);
                        lex.consume_keyword("as");
                        let ty = parse_type(lex);
                        Value::Cast(val, ty)
                    },
                    Token::Ident("DSTPTR") => {
                        let val = lookup.parse_slot(lex);
                        Value::DstPtr(val)
                        },
                    Token::Ident("DSTMETA") => {
                        let val = lookup.parse_slot(lex);
                        Value::DstMeta(val)
                        },
                    Token::Sym("=") => Value::Use(lookup.parse_slot(lex)),
                    Token::Sym("&") => {
                        use crate::types::Mutability;
                        let mutability = if lex.consume_if_keyword("mut") {
                            Mutability::Unique
                        }
                        else if lex.consume_if_keyword("move") {
                            Mutability::Move
                        }
                        else {
                            Mutability::Shared
                        };
                        let slot = lookup.parse_slot(lex);
                        Value::Borrow(mutability, slot)
                    },
                    // Constants
                    Token::Sym("+")
                    |Token::Sym("-")
                    |Token::Ident("true")
                    |Token::Ident("false")
                    |Token::Integer(_)
                    |Token::String(_)
                    |Token::Ident("ADDROF")
                        => { *lex = saved; Value::Constant(parse_const(lex)) }
                    t => todo!("{lex}: ASSIGN {:?}", t),
                    };
                stmts.push(Statement::Assign(dst, val));
            },
            "DROP" => todo!(),

            "RETURN" => break Terminator::Return,
            "DIVERGE" => break Terminator::Diverge,
            "INCOMPLETE" => break Terminator::Invalid,
            "GOTO" => {
                let tgt = lex.consume_int() as usize;
                break Terminator::Goto(tgt)
                },
            "IF" => {
                let v = lookup.parse_slot(lex);
                lex.consume_keyword("goto");
                let bb_true = lex.consume_int() as usize;
                lex.consume_keyword("else");
                let bb_false = lex.consume_int() as usize;
                break Terminator::If(v, bb_true, bb_false)
            }
            "CALL" => {
                let dst = lookup.parse_slot(lex);
                lex.consume_sym("=");
                let target = match lex.get_tok_noeof() {
                    Token::Ident(name) => crate::mir::CallTarget::Path(name.to_owned()),
                    Token::String(name) => {
                        let mut tys = Vec::new();
                        if lex.consume_if_sym("<") {
                            while !lex.consume_if_sym(">") {
                                tys.push(parse_type(lex));
                                if !lex.consume_if_sym(",") {
                                    lex.consume_sym(">");
                                    break;
                                }
                            }
                        }
                        crate::mir::CallTarget::Intrinsic(name.parse_string(), tys)
                        },
                    Token::Sym("(") => {
                        let v = lookup.parse_slot(lex);
                        lex.consume_sym(")");
                        crate::mir::CallTarget::Value(v)
                        }
                    t => panic!("{lex}: Unexpected token for call target - {t:?}"),
                    };
                let mut args = Vec::new();
                lex.consume_sym("(");
                while !lex.consume_if_sym(")") {
                    args.push(lookup.parse_param(lex));
                    if !lex.consume_if_sym(",") {
                        lex.consume_sym(")");
                        break;
                    }
                }
                lex.consume_keyword("goto");
                let bb_ret = lex.consume_int() as usize;
                lex.consume_keyword("else");
                let bb_panic = lex.consume_int() as usize;
                break Terminator::Call(crate::mir::TerminatorCall {
                    target,
                    args,
                    dst,
                    bb_ret,
                    bb_panic,
                })
            }
            "SWITCHVALUE" => {
                let val = lookup.parse_slot(lex);
                lex.consume_sym("{");
                let mut targets = Vec::new();
                let vals = match lex.get_tok_noeof()
                    {
                    Token::Sym(sgn @ "+")|Token::Sym(sgn @ "-") => {
                        fn expect_sign(lex: &mut Lexer) -> bool {
                            match lex.get_tok_noeof() {
                            Token::Sym("+") => false,
                            Token::Sym("-") => true,
                            t @ _ => panic!("{lex}: Expected a sign - got {t:?}"),
                            }
                        }
                        match lex.get_tok_noeof()
                        {
                        Token::Integer(v) => {
                            let mut vals = Vec::new();
                            fn get_signed(is_neg: bool, v: u128) -> Option<i128> {
                                if is_neg && v == (u128::MAX >> 1) + 1 {
                                    Some(i128::MIN)
                                }
                                else {
                                    let v: i128 = v.try_into().ok()?;
                                    if is_neg {
                                        v.checked_neg()
                                    }
                                    else {
                                        Some(v)
                                    }
                                }
                            }
                            let Some(v) = get_signed(sgn == "-", v) else {
                                panic!("{lex}: Too large signed value")
                            };
                            vals.push(v);
                            loop {
                                lex.consume_sym("=");
                                targets.push(lex.consume_int() as usize);
                                lex.consume_sym(",");
                                if lex.consume_if_sym("_") {
                                    break;
                                }
                                let is_neg = expect_sign(lex);
                                let v = lex.consume_int();
                                let Some(v) = get_signed(is_neg, v) else {
                                    panic!("{lex}: Too large signed value")
                                };
                                vals.push(v);
                            }
                            crate::mir::SwitchValues::Signed(vals)
                            },
                        Token::Float(v) => {
                            let mut vals = Vec::new();
                            vals.push(if sgn == "-" { -v } else { v });
                            loop {
                                lex.consume_sym("=");
                                targets.push(lex.consume_int() as usize);
                                lex.consume_sym(",");
                                if lex.consume_if_sym("_") {
                                    break;
                                }
                                let is_neg = expect_sign(lex);
                                let v = lex.consume_float();
                                let v = if is_neg { -v } else { v };
                                vals.push(v);
                            }
                            crate::mir::SwitchValues::Float(vals)
                            },
                        t => todo!("{lex}: statement/terminator - SWITCHVALUE: {:?}", t),
                        }
                    },
                    Token::Integer(i) => {
                        let mut vals = Vec::new();
                        vals.push(i);
                        loop {
                            lex.consume_sym("=");
                            targets.push(lex.consume_int() as usize);
                            lex.consume_sym(",");
                            if lex.consume_if_sym("_") {
                                break;
                            }
                            vals.push(lex.consume_int());
                        }
                        crate::mir::SwitchValues::Unsigned(vals)
                    },
                    Token::String(s) => {
                        let mut vals = Vec::new();
                        vals.push(s.parse_bytes());
                        loop {
                            lex.consume_sym("=");
                            targets.push(lex.consume_int() as usize);
                            lex.consume_sym(",");
                            if lex.consume_if_sym("_") {
                                break;
                            }
                            vals.push(lex.consume_str().parse_bytes());
                        }
                        crate::mir::SwitchValues::String(vals)
                    },
                    t => todo!("{lex}: statement/terminator - SWITCHVALUE: {:?}", t),
                    };
                lex.consume_sym("=");
                let default_bb = lex.consume_int() as usize;
                lex.consume_sym("}");
                break Terminator::SwitchValue(val, vals, targets, default_bb);
                },
            i => todo!("{lex}: statement/terminator - {}", i),
            }
            lex.consume_sym(";");
        };
        lex.consume_sym("}");
        rv.blocks.push(crate::mir::BasicBlock {
            statements: stmts,
            terminator: term,
        })
    }

    lex.consume_sym("}");
    rv
}

struct FcnLookup<'a> {
    args: &'a [String],
    vars: &'a [(String,crate::types::TypeRef)]
}
impl FcnLookup<'_> {
    fn parse_slot(&self, lex: &mut Lexer) -> crate::mir::Slot {
        use crate::mir::{SlotRoot, SlotWrapper};
        let mut n_deref = 0;
        while lex.consume_if_sym("*") {
            n_deref += 1;
        }
        let mut rv = if lex.consume_if_sym("(") {
                let v = self.parse_slot(lex);
                lex.consume_sym(")");
                v
            }
            else {
                let i = lex.consume_ident();
                crate::mir::Slot {
                    wrappers: Vec::new(),
                    root: if i == "RETURN" {
                        SlotRoot::Return
                    }
                    else {
                        if let Some(idx) = self.vars.iter().position(|(name, _)| name == i) {
                            SlotRoot::Local(idx)
                        }
                        else if let Some(idx) = self.args.iter().position(|name| name == i) {
                            SlotRoot::Argument(idx)
                        }
                        else {
                            SlotRoot::Named(i.to_string())
                        }
                    },
                }
            };
        loop {
            if lex.consume_if_sym(".") {
                if lex.consume_if_sym("*") {
                    rv.wrappers.push(SlotWrapper::Deref)
                }
                else {
                    let idx = lex.consume_int() as usize;
                    rv.wrappers.push(SlotWrapper::Field(idx))
                }
            }
            else if lex.consume_if_sym("[") {
                let i = lex.consume_ident();
                let Some(idx) = self.vars.iter().position(|(name, _)| name == i) else {
                    panic!("{lex}: Unable to find {i:?} for index")
                };
                lex.consume_sym("]");
                rv.wrappers.push(SlotWrapper::Index(idx))
            }
            else if lex.consume_if_sym("#") {
                let idx = lex.consume_int() as usize;
                rv.wrappers.push(SlotWrapper::Downcast(idx))
            }
            else {
                break
            }
        }

        for _ in 0 .. n_deref {
            rv.wrappers.push(SlotWrapper::Deref)
        }

        rv
    }
    fn parse_param(&self, lex: &mut Lexer) -> crate::mir::Param {
        let saved = lex.clone();
        if lex.consume_if_sym("+") || lex.consume_if_sym("-") {
            *lex = saved;
            crate::mir::Param::Const(parse_const(lex))
        }
        else if let Some(_) = lex.consume_if_int() {
            *lex = saved;
            crate::mir::Param::Const(parse_const(lex))
        }
        else {
            crate::mir::Param::Slot(self.parse_slot(lex))
        }
    }
}

fn parse_const(lex: &mut Lexer) -> crate::mir::Const {
    match lex.get_tok_noeof() {
    Token::Sym("+") => {
        let v = lex.consume_int();
        let t = parse_type(lex);
        let bits = match t.root {
            crate::types::Root::Signed(bits) if t.wrappers.is_empty() => bits,
            _ => panic!("{lex}: Unexpected integer type {t:?}"),
            };
        crate::mir::Const::Signed(v as i128, bits)
        },
    Token::Sym("-") => {
        let v = lex.consume_int();
        let t = parse_type(lex);
        let bits = match t.root {
            crate::types::Root::Signed(bits) if t.wrappers.is_empty() => bits,
            _ => panic!("{lex}: Unexpected integer type {t:?}"),
            };
        crate::mir::Const::Signed( -(v as i128), bits)
        },
    Token::Integer(v) => {
        let t = parse_type(lex);
        match t.root {
            crate::types::Root::Signed(bits) if t.wrappers.is_empty()
                => crate::mir::Const::Signed( -(v as i128), bits),
            crate::types::Root::Unsigned(bits) if t.wrappers.is_empty()
                => crate::mir::Const::Unsigned( v as u128, bits),
            _ => panic!("{lex}: Unexpected unsigned integer type {t:?}"),
            }
        },
    Token::Float(v) => {
        let t = parse_type(lex);
        match t.root {
            crate::types::Root::Float(bits) if t.wrappers.is_empty()
                => crate::mir::Const::Float(v, bits),
            _ => panic!("{lex}: Unexpected float type {t:?}"),
            }
        },
    Token::Ident("true") => crate::mir::Const::Boolean(true),
    Token::Ident("false") => crate::mir::Const::Boolean(false),
    Token::String(v) => crate::mir::Const::String(v.parse_string()),
    Token::Ident("ADDROF") => crate::mir::Const::ItemAddr( lex.consume_ident().to_owned() ),
    t => todo!("{lex}: parse_const - {t:?}"),
    }
}