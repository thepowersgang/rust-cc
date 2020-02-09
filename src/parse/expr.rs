//! Expression parsing
//! `ParseState::parse_expr` - Returns an AST expression node, parsed from the token stream
use parse::Token;
use parse::ParseResult;

/// Shortcut for creating a new node
macro_rules! node {
	( $ty:ident $($args:tt)* ) => {
		crate::ast::Node::new( crate::ast::NodeKind::$ty $($args)* )
	};
}

// Parse, left associative
macro_rules! parse_left_assoc
{
	($_self:ident, $name:ident, $next:ident, $rv:ident, { $($patterns:pat => $vals:expr),*, }) => {
		fn $name(&mut $_self) -> ParseResult<::ast::Node> {
			let mut $rv = $_self.$next()?;
			loop
			{
				$rv = match $_self.lex.get_token()?
					{
					$($patterns => $vals),*,
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
		let exp = self.parse_expr()?;
		if peek_token_nc!(self.lex, Token::Comma)
		{
			let mut exprs = vec![exp];
			while peek_token!(self.lex, Token::Comma)
			{
				exprs.push( self.parse_expr()? );
			}
			Ok(node!( StmtList(exprs) ))
		}
		else
		{
			Ok(exp)
		}
	}
	
	/// Parse #0 : Assignment
	fn parse_expr_0(&mut self) -> ParseResult<::ast::Node>
	{
		let rv = self.parse_expr_1()?;
		Ok( match self.lex.get_token()?
		{
		Token::Assign => node!( Assign(box rv, box self.parse_expr_0()?) ),
		Token::AssignBitAnd => node!( AssignOp(::ast::BinOp::BitAnd, box rv, box self.parse_expr_0()?) ),
		Token::AssignBitOr  => node!( AssignOp(::ast::BinOp::BitOr,  box rv, box self.parse_expr_0()?) ),
		Token::AssignAdd  => node!( AssignOp(::ast::BinOp::Add,  box rv, box self.parse_expr_0()?) ),
		Token::AssignSub  => node!( AssignOp(::ast::BinOp::Sub,  box rv, box self.parse_expr_0()?) ),
		Token::AssignMul  => node!( AssignOp(::ast::BinOp::Mul,  box rv, box self.parse_expr_0()?) ),
		Token::AssignDiv  => node!( AssignOp(::ast::BinOp::Div,  box rv, box self.parse_expr_0()?) ),
		Token::AssignMod  => node!( AssignOp(::ast::BinOp::Mod,  box rv, box self.parse_expr_0()?) ),
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	/// Expression #1 - Ternary
	fn parse_expr_1(&mut self) -> ParseResult<::ast::Node>
	{
		let rv = self.parse_expr_2()?;
		
		Ok( match self.lex.get_token()?
		{
		Token::QuestionMark => {
			debug!("Ternary, rv (cnd) = {:?}", rv);
			let tv = box self.parse_expr_1()?;
			debug!("Ternary - tv = {:?}", tv);
			syntax_assert!(self.lex.get_token()?, Token::Colon);
			let fv = box self.parse_expr_1()?;
			debug!("Ternary - fv = {:?}", fv);
			node!( Ternary(box rv, tv, fv) )
			}
		t @ _ => {
			self.lex.put_back(t);
			rv
			}
		})
	}
	
	/// Expression #2 - Boolean AND/OR
	parse_left_assoc!{self, parse_expr_2, parse_expr_3, rv, {
		Token::DoublePipe      => node!( BinOp(::ast::BinOp::LogicOr,  box rv, box self.parse_expr_3()?) ),
		Token::DoubleAmpersand => node!( BinOp(::ast::BinOp::LogicAnd, box rv, box self.parse_expr_3()?) ),
	}}
	
	/// Expresission #3 - Bitwise
	parse_left_assoc!{self, parse_expr_3, parse_expr_4, rv, {
		Token::Pipe      => node!( BinOp(::ast::BinOp::BitOr , box rv, box self.parse_expr_4()?) ),
		Token::Ampersand => node!( BinOp(::ast::BinOp::BitAnd, box rv, box self.parse_expr_4()?) ),
		Token::Caret     => node!( BinOp(::ast::BinOp::BitXor, box rv, box self.parse_expr_4()?) ),
	}}
	
	/// Expression #4 - Comparison Operators
	parse_left_assoc!{self, parse_expr_4, parse_expr_5, rv, {
		Token::Equality  => node!( BinOp(::ast::BinOp::CmpEqu , box rv, box self.parse_expr_5()?) ),
		Token::NotEquals => node!( BinOp(::ast::BinOp::CmpNEqu, box rv, box self.parse_expr_5()?) ),
		Token::Lt  => node!( BinOp(::ast::BinOp::CmpLt,  box rv, box self.parse_expr_5()?) ),
		Token::LtE => node!( BinOp(::ast::BinOp::CmpLtE, box rv, box self.parse_expr_5()?) ),
		Token::Gt  => node!( BinOp(::ast::BinOp::CmpGt,  box rv, box self.parse_expr_5()?) ),
		Token::GtE => node!( BinOp(::ast::BinOp::CmpGtE, box rv, box self.parse_expr_5()?) ),
	}}
	
	/// Expression #5 - Bit Shifts
	parse_left_assoc!{self, parse_expr_5, parse_expr_6, rv, {
		Token::ShiftLeft  => node!( BinOp(::ast::BinOp::ShiftLeft,  box rv, box self.parse_expr_6()?) ),
		Token::ShiftRight => node!( BinOp(::ast::BinOp::ShiftRight, box rv, box self.parse_expr_6()?) ),
	}}
	
	/// Expresion #6 - Arithmatic
	parse_left_assoc!{self, parse_expr_6, parse_expr_7, rv, {
		Token::Plus  => node!( BinOp(::ast::BinOp::Add, box rv, box self.parse_expr_7()?) ),
		Token::Minus => node!( BinOp(::ast::BinOp::Sub, box rv, box self.parse_expr_7()?) ),
	}}
	
	/// Expression #7 - Multiply/Divide
	parse_left_assoc!{self, parse_expr_7, parse_expr_8, rv, {
		Token::Star    => node!( BinOp(::ast::BinOp::Mul, box rv, box self.parse_expr_8()?) ),
		Token::Slash   => node!( BinOp(::ast::BinOp::Div, box rv, box self.parse_expr_8()?) ),
		Token::Percent => node!( BinOp(::ast::BinOp::Mod, box rv, box self.parse_expr_8()?) ),
	}}
	
	/// Expression #8 - Unary Righthand
	parse_left_assoc!{self, parse_expr_8, parse_expr_9, rv, {
		Token::DoublePlus  => node!( UniOp(::ast::UniOp::PostInc, box rv) ),
		Token::DoubleMinus => node!( UniOp(::ast::UniOp::PostDec, box rv) ),
	}}
	
	/// Expression #9 - Unary left
	fn parse_expr_9(&mut self) -> ParseResult<::ast::Node>
	{
		Ok( match self.lex.get_token()?
		{
		Token::Minus       => node!( UniOp(::ast::UniOp::Neg,      box self.parse_expr_9()?) ),
		Token::Tilde       => node!( UniOp(::ast::UniOp::BitNot,   box self.parse_expr_9()?) ),
		Token::Exclamation => node!( UniOp(::ast::UniOp::LogicNot, box self.parse_expr_9()?) ),
		Token::DoublePlus  => node!( UniOp(::ast::UniOp::PreInc,   box self.parse_expr_9()?) ),
		Token::DoubleMinus => node!( UniOp(::ast::UniOp::PreDec,   box self.parse_expr_9()?) ),
		Token::Star        => node!( UniOp(::ast::UniOp::Deref,    box self.parse_expr_9()?) ),
		Token::Ampersand   => node!( UniOp(::ast::UniOp::Address, box self.parse_expr_member()?) ),	// different, as double addr is inval
		t @ _ => {
			self.lex.put_back(t);
			self.parse_expr_member()?
			},
		})
	}
	
	
	/// Expression - Member access
	parse_left_assoc!{self, parse_expr_member, parse_expr_p, rv, {
		Token::DerefMember => node!( DerefMember(box rv, syntax_assert!(self.lex => Token::Ident(i) @ i)) ),
		Token::Period      => node!( Member(     box rv, syntax_assert!(self.lex => Token::Ident(i) @ i)) ),
		Token::SquareOpen => {
			let idx = box self.parse_expr()?;
			syntax_assert!(self.lex => Token::SquareClose);
			node!( Index(box rv, idx) )
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
			node!( FcnCall(box rv, args) )
			},
	}}
	
	/// Expression - Parens
	fn parse_expr_p(&mut self) -> ParseResult<::ast::Node>
	{
		Ok(match self.lex.get_token()?
		{
		// - Either a cast, or a grouped expression
		Token::ParenOpen => match self.get_base_type_opt()?
			{
			Some(basetype) => {
				let (fulltype, ident) = self.get_full_type(basetype)?;
				if ident != "" {
					syntax_error!("Unexpected identifier in cast");
				}
				syntax_assert!(self.lex => Token::ParenClose);
				node!( Cast(fulltype, box self.parse_expr_9()?) )
				},
			None => {
				let rv = self.parse_expr()?;
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

					let v = box self.parse_expr_0()?;
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

					let base_ty = self.get_base_type()?;
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
			node!( Intrinsic(name, tys, vals) )
			},
		Token::Ident(id) => node!( Identifier(id, None) ),
		Token::String(s) => {
			let mut val = s;
			loop
			{
				match self.lex.get_token()? {
				Token::String(s) => { val.push_str(&s); },
				t @ _ => { self.lex.put_back(t); break; }
				}
			}
			node!( String(val) )
			},
		Token::Integer(v,cls,_) => node!( Integer(v, cls) ),
		Token::Character(v) => node!( Integer(v, crate::types::IntClass::char()) ),
		Token::Float(v,cls,_) => node!( Float(v, cls) ),
		Token::Rword_sizeof => {
			let expect_paren = peek_token!(self.lex, Token::ParenOpen);
			let rv = match self.get_base_type_opt()?
				{
				Some(t) => {
					let (tr, name) = self.get_full_type(t)?;
					if ! name.is_empty() {
						syntax_error!("Unexpected name in sizeof");
					}
					node!( SizeofType(tr) )
					},
				None => {
					let val = if expect_paren { box self.parse_expr_0()? } else { box self.parse_expr_p()? };
					node!( SizeofExpr(val) )
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
