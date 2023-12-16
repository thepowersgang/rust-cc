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
            Some(t) => panic!("Unexpected {t:?}, expected Ident(_)"),
            }
        {
        "fn" => {
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
                let name = lex.consume_ident();
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
            Entry::Occupied(_) => todo!(),
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
            let name = lex.consume_ident().to_owned();
            lex.consume_sym(":");
            let ty = parse_type(lex);
            lex.consume_sym("=");
            let mut s = crate::modtree::Static {
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
                let v = e.get_mut();
                if v.link_name.is_none() {
                    v.link_name = s.link_name;
                }
                else if s.link_name.is_none() {
                    if s.link_name != v.link_name {
                        // Uh-oh
                    }
                }
                else {
                }
                if v.value.is_none() {
                    v.value = s.value;
                }
                else if s.value.is_none() {
                    panic!("{lex}: Re-definition of {}", e.key());
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
                        let val = lookup.parse_param(lex);
                        lex.consume_keyword("as");
                        let ty = parse_type(lex);
                        Value::Cast(val, ty)
                    },
                    Token::Ident("DSTPTR") => {
                        let val = lookup.parse_param(lex);
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
                    Token::Sym("+")
                    |Token::Sym("-")
                    |Token::Ident("true")
                    |Token::Ident("false")
                    |Token::Integer(_)
                    |Token::String(_)
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
                        lex.consume_sym("<");
                        let mut tys = Vec::new();
                        while !lex.consume_if_sym(">") {
                            tys.push(parse_type(lex));
                            if !lex.consume_if_sym(",") {
                                lex.consume_sym(">");
                                break;
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
    Token::Ident("true") => crate::mir::Const::Boolean(true),
    Token::Ident("false") => crate::mir::Const::Boolean(false),
    Token::String(v) => crate::mir::Const::String(v.parse_string()),
    t => todo!("{lex}: parse_const - {t:?}"),
    }
}