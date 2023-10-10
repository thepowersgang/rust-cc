//! Expression parsing
//! `ParseState::parse_expr` - Returns an AST expression node, parsed from the token stream
use parse::Token;
use parse::ParseResult;

/// Shortcut for creating a new node
macro_rules! node {
	( $span:expr, $ty:ident $($args:tt)* ) => {
		crate::ast::Node::new( $span, crate::ast::NodeKind::$ty $($args)* )
	};
}

// Parse, left associative
macro_rules! parse_left_assoc
{
	( $(#[$a:meta])* $_self:ident, $name:ident, $next:ident, $sp:ident, $rv:ident, { $($patterns:pat => $vals:expr),*, }) => {
		$(#[$a])*
		fn $name(&mut $_self) -> ParseResult<::ast::Node> {
			let $sp = $_self.lex.point_span();
			let mut $rv = $_self.$next()?;
			loop
			{
				$rv = match $_self.lex.get_token()?
					{
					$($patterns => { let $sp = $sp.clone(); $vals }),*,
					t @ _ => {
						$_self.lex.put_back(t);
						break;
						}
					};
			}
			Ok($rv)
		}
	}
}

impl<'ast> super::ParseState<'ast>
{
	// ----------------------------------------------------------------
	// Expressions!
	// ----------------------------------------------------------------
	/// Parse a single expression
	pub fn parse_expr(&mut self) -> ParseResult<::ast::Node>
	{
		return self.parse_expr_0();
	}
	/// Parse a sequence of expressions (comma-separated)
	pub(super) fn parse_expr_list(&mut self) -> ParseResult<::ast::Node>
	{
		let sp = self.lex.point_span();
		let exp = self.parse_expr()?;
		if peek_token_nc!(self.lex, Token::Comma)
		{
			let mut exprs = vec![exp];
			while peek_token!(self.lex, Token::Comma)
			{
				exprs.push( self.parse_expr()? );
			}
			Ok(node!( sp, StmtList(exprs) ))
		}
		else
		{
			Ok(exp)
		}
	}
	
	/// Parse #0 : Assignment
	fn parse_expr_0(&mut self) -> ParseResult<::ast::Node>
	{
		let sp = self.lex.point_span();
		let rv = self.parse_expr_1()?;
		Ok( match self.lex.get_token()?
		{
		Token::Assign => node!( sp, Assign(Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignBitAnd => node!( sp, AssignOp(::ast::BinOp::BitAnd, Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignBitOr  => node!( sp, AssignOp(::ast::BinOp::BitOr,  Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignBitXor => node!( sp, AssignOp(::ast::BinOp::BitXor, Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignAdd  => node!( sp, AssignOp(::ast::BinOp::Add,  Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignSub  => node!( sp, AssignOp(::ast::BinOp::Sub,  Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignMul  => node!( sp, AssignOp(::ast::BinOp::Mul,  Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignDiv  => node!( sp, AssignOp(::ast::BinOp::Div,  Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignMod  => node!( sp, AssignOp(::ast::BinOp::Mod,  Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignShiftLeft  => node!( sp, AssignOp(::ast::BinOp::ShiftLeft , Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		Token::AssignShiftRight => node!( sp, AssignOp(::ast::BinOp::ShiftRight, Box::new(rv), Box::new(self.parse_expr_0()?)) ),
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	/// Expression #1 - Ternary
	fn parse_expr_1(&mut self) -> ParseResult<::ast::Node>
	{
		let sp = self.lex.point_span();
		let rv = self.parse_expr_2()?;
		
		Ok( match self.lex.get_token()?
		{
		Token::QuestionMark => {
			debug!("Ternary, rv (cnd) = {:?}", rv);
			let tv = Box::new(self.parse_expr_1()?);
			debug!("Ternary - tv = {:?}", tv);
			syntax_assert!(self.lex.get_token()?, Token::Colon);
			let fv = Box::new(self.parse_expr_1()?);
			debug!("Ternary - fv = {:?}", fv);
			node!( sp, Ternary(Box::new(rv), tv, fv) )
			}
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	parse_left_assoc!{
		/// Expression #2 - Boolean AND/OR
		self, parse_expr_2, parse_expr_3, sp, rv, {
		Token::DoublePipe      => node!( sp, BinOp(::ast::BinOp::LogicOr,  Box::new(rv), Box::new(self.parse_expr_3()?)) ),
		Token::DoubleAmpersand => node!( sp, BinOp(::ast::BinOp::LogicAnd, Box::new(rv), Box::new(self.parse_expr_3()?)) ),
	}}
	
	parse_left_assoc!{
		/// Expresission #3 - Bitwise
		self, parse_expr_3, parse_expr_4, sp, rv, {
		Token::Pipe      => node!( sp, BinOp(::ast::BinOp::BitOr , Box::new(rv), Box::new(self.parse_expr_4()?)) ),
		Token::Ampersand => node!( sp, BinOp(::ast::BinOp::BitAnd, Box::new(rv), Box::new(self.parse_expr_4()?)) ),
		Token::Caret     => node!( sp, BinOp(::ast::BinOp::BitXor, Box::new(rv), Box::new(self.parse_expr_4()?)) ),
	}}
	
	parse_left_assoc!{
		/// Expression #4 - Comparison Operators
		self, parse_expr_4, parse_expr_5, sp, rv, {
		Token::Equality  => node!( sp, BinOp(::ast::BinOp::CmpEqu , Box::new(rv), Box::new(self.parse_expr_5()?)) ),
		Token::NotEquals => node!( sp, BinOp(::ast::BinOp::CmpNEqu, Box::new(rv), Box::new(self.parse_expr_5()?)) ),
		Token::Lt  => node!( sp, BinOp(::ast::BinOp::CmpLt,  Box::new(rv), Box::new(self.parse_expr_5()?)) ),
		Token::LtE => node!( sp, BinOp(::ast::BinOp::CmpLtE, Box::new(rv), Box::new(self.parse_expr_5()?)) ),
		Token::Gt  => node!( sp, BinOp(::ast::BinOp::CmpGt,  Box::new(rv), Box::new(self.parse_expr_5()?)) ),
		Token::GtE => node!( sp, BinOp(::ast::BinOp::CmpGtE, Box::new(rv), Box::new(self.parse_expr_5()?)) ),
	}}
	
	parse_left_assoc!{
		/// Expression #5 - Bit Shifts
		self, parse_expr_5, parse_expr_6, sp, rv, {
		Token::ShiftLeft  => node!( sp, BinOp(::ast::BinOp::ShiftLeft,  Box::new(rv), Box::new(self.parse_expr_6()?)) ),
		Token::ShiftRight => node!( sp, BinOp(::ast::BinOp::ShiftRight, Box::new(rv), Box::new(self.parse_expr_6()?)) ),
	}}
	
	parse_left_assoc!{
		/// Expresion #6 - Arithmatic
		self, parse_expr_6, parse_expr_7, sp, rv, {
		Token::Plus  => node!( sp, BinOp(::ast::BinOp::Add, Box::new(rv), Box::new(self.parse_expr_7()?)) ),
		Token::Minus => node!( sp, BinOp(::ast::BinOp::Sub, Box::new(rv), Box::new(self.parse_expr_7()?)) ),
	}}
	
	parse_left_assoc!{
		/// Expression #7 - Multiply/Divide
		self, parse_expr_7, parse_expr_8, sp, rv, {
		Token::Star    => node!( sp, BinOp(::ast::BinOp::Mul, Box::new(rv), Box::new(self.parse_expr_8()?)) ),
		Token::Slash   => node!( sp, BinOp(::ast::BinOp::Div, Box::new(rv), Box::new(self.parse_expr_8()?)) ),
		Token::Percent => node!( sp, BinOp(::ast::BinOp::Mod, Box::new(rv), Box::new(self.parse_expr_8()?)) ),
	}}
	
	parse_left_assoc!{
		/// Expression #8 - Unary Righthand
		self, parse_expr_8, parse_expr_9, sp, rv, {
		Token::DoublePlus  => node!( sp, UniOp(::ast::UniOp::PreInc, Box::new(rv)) ),
		Token::DoubleMinus => node!( sp, UniOp(::ast::UniOp::PreDec, Box::new(rv)) ),
	}}
	
	/// Expression #9 - Unary left
	fn parse_expr_9(&mut self) -> ParseResult<::ast::Node>
	{
		let sp = self.lex.point_span();
		Ok( match self.lex.get_token()?
		{
		Token::Minus       => node!( sp, UniOp(::ast::UniOp::Neg,      Box::new(self.parse_expr_9()?)) ),
		Token::Tilde       => node!( sp, UniOp(::ast::UniOp::BitNot,   Box::new(self.parse_expr_9()?)) ),
		Token::Exclamation => node!( sp, UniOp(::ast::UniOp::LogicNot, Box::new(self.parse_expr_9()?)) ),
		Token::DoublePlus  => node!( sp, UniOp(::ast::UniOp::PostInc,  Box::new(self.parse_expr_9()?)) ),
		Token::DoubleMinus => node!( sp, UniOp(::ast::UniOp::PostDec,  Box::new(self.parse_expr_9()?)) ),
		Token::Star        => node!( sp, UniOp(::ast::UniOp::Deref,    Box::new(self.parse_expr_9()?)) ),
		Token::Ampersand   => node!( sp, UniOp(::ast::UniOp::Address,  Box::new(self.parse_expr_member()?)) ),	// different, as double addr is inval
		t @ _ => {
			self.lex.put_back(t);
			self.parse_expr_member()?
			},
		})
	}
	
	
	parse_left_assoc!{
		/// Expression - Member access
		self, parse_expr_member, parse_expr_p, sp, rv, {
		Token::DerefMember => node!( sp, DerefMember(Box::new(rv), syntax_assert!(self.lex => Token::Ident(i) @ i)) ),
		Token::Period      => node!( sp, Member(     Box::new(rv), syntax_assert!(self.lex => Token::Ident(i) @ i)) ),
		Token::SquareOpen => {
			let idx = self.parse_expr()?;
			syntax_assert!(self.lex => Token::SquareClose);
			node!( sp, Index(Box::new(rv), Box::new(idx)) )
			},
		Token::ParenOpen => {
			let mut args = Vec::new();
			if ! peek_token!(self.lex, Token::ParenClose)
			{
				loop
				{
					args.push( self.parse_expr()? );
					if peek_token!(self.lex, Token::ParenClose) {
						break;
					}
					syntax_assert!(self.lex => Token::Comma);
				}
			}
			node!( sp, FcnCall(Box::new(rv), args) )
			},
	}}
	
	/// Expression - Parens
	fn parse_expr_p(&mut self) -> ParseResult<::ast::Node>
	{
		let sp = self.lex.point_span();
		Ok(match self.lex.get_token()?
		{
		// - Either a cast, or a grouped expression
		Token::ParenOpen => match self.get_base_type_opt()?
			{
			Some((_sc, basetype)) => {
				let (fulltype, ident) = self.get_full_type(basetype)?;
				if ident != "" {
					syntax_error!("Unexpected identifier in cast");
				}
				syntax_assert!(self.lex => Token::ParenClose);
				// Handle `(struct Foo){...}` expressions
				if peek_token_nc!(self.lex, Token::BraceOpen) {
					self.lex.point_span().todo(format_args!("Handle inline init syntax `(struct Foo){{ ... }}`"));
				}
				node!( sp, Cast(fulltype, Box::new(self.parse_expr_9()?)) )
				},
			None => {
				let rv = self.parse_expr_list()?;
				syntax_assert!(self.lex => Token::ParenClose);
				rv
				},
			},
		t @ _ => {
			self.lex.put_back(t);
			self.parse_expr_z()?
			}
		})
	}
	/// Expression - Leaf nodes
	fn parse_expr_z(&mut self) -> ParseResult<::ast::Node>
	{
		let sp = self.lex.point_span();
		Ok(match self.lex.get_token()?
		{
		Token::Ident(ref id) if id == "__magiccall__" => {
			syntax_assert!(self.lex => Token::ParenOpen);
			let name = syntax_assert!(self.lex => Token::String(s) @ s);
			let mut vals = Vec::new();
			if peek_token!(self.lex, Token::Colon)
			{
				loop
				{
					if peek_token_nc!(self.lex, Token::ParenClose) || peek_token_nc!(self.lex, Token::Colon) {
						break;
					}

					let v = Box::new(self.parse_expr_0()?);
					vals.push(v);

					if !peek_token!(self.lex, Token::Comma) {
						break;
					}
				}
			}
			let mut tys = Vec::new();
			if peek_token!(self.lex, Token::Colon)
			{
				loop
				{
					if peek_token_nc!(self.lex, Token::ParenClose) {
						break;
					}

					let (_sc, base_ty) = self.get_base_type()?;
					let (ty, name) = self.get_full_type( base_ty )?;
					if ! name.is_empty() {
						syntax_error!("Unexpected name in magic call");
					}
					tys.push(ty);

					if !peek_token!(self.lex, Token::Comma) {
						break;
					}
				}
			}
			syntax_assert!(self.lex => Token::ParenClose);
			node!( sp, Intrinsic(name, tys, vals) )
			},
		Token::Ident(ref id) if id == "__func__" => {
			node!( sp, String(format!("dunno")) )
			},
		Token::Ident(id) => {
			use crate::ast::IdentRef;
			// TODO: Look up the name here? (It's where it should be done...)
			let b = if let Some( (e,i) ) = self.ast.find_enum_var(&id) {
					Some(IdentRef::Enum(e, i))
				}
				else if let Some(v) = self.ast.get_symbol(&id) {
					if let crate::types::BaseType::Function(_) = v.symtype.basetype {
						// Special type for functions, as they have strange typecheck decay rules
						Some(IdentRef::Function)
					}
					else {
						Some(IdentRef::StaticItem)
					}
				}
				else {
					None
				};
			node!( sp, Identifier(id, b) )
			},
		Token::String(s) => {
			let mut val = s;
			loop
			{
				match self.lex.get_token()? {
				Token::String(s) => { val.push_str(&s); },
				t @ _ => { self.lex.put_back(t); break; }
				}
			}
			node!( sp, String(val) )
			},
		Token::Integer(v,cls,_) => node!( sp, Integer(v, cls) ),
		Token::Character(v) => node!( sp, Integer(v, crate::types::IntClass::char()) ),
		Token::Float(v,cls,_) => node!( sp, Float(v, cls) ),
		Token::Rword_sizeof => {
			let expect_paren = peek_token!(self.lex, Token::ParenOpen);
			let rv = match self.get_base_type_opt()?
				{
				Some((_sc,t)) => {
					let (tr, name) = self.get_full_type(t)?;
					if ! name.is_empty() {
						syntax_error!("Unexpected name in sizeof");
					}
					node!( sp, SizeofType(tr) )
					},
				None => {
					let val = if expect_paren { self.parse_expr_0()? } else { self.parse_expr_p()? };
					node!( sp, SizeofExpr(Box::new(val)) )
					},
				};
			if expect_paren {
				syntax_assert!(self.lex => Token::ParenClose);
			}
			debug!("{:?}, lex={}", rv, self.lex);
			rv
			},
		t @ _ => syntax_error!("Unexpected {:?}, expected value", t),
		})
	}
}
