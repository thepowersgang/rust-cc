#[allow(non_camel_case_types)]
#[derive(Debug,PartialEq,Clone)]
/// Token type (result of lexing/pre-processor)
pub enum Token
{
	EOF,
	
	// Ignored Tokens
	Whitespace,
	LineComment(String),
	BlockComment(String),
	Newline,
	EscapedNewline,

	// Pre-preocessor forwarding (when confiured to do so)
	Preprocessor(Preprocessor),

	// -- Expression leaves
	Integer(u64, ::types::IntClass, String),
	Float(f64, ::types::FloatClass, String),
	Chararacter(u64),
	String(String),
	Ident(String),
	
	// -- Symbols
	Hash,
	Tilde,
	Exclamation,
	Period,
	DerefMember,
	Comma,
	Semicolon,
	Star,
	Slash,
	Vargs,
	QuestionMark,
	Colon,
	
	Assign,
	AssignAdd,
	AssignSub,
	AssignMul,
	AssignDiv,
	AssignMod,
	AssignLogicOr,
	AssignLogicAnd,
	AssignBitOr,
	AssignBitAnd,
	
	ShiftRight,
	ShiftLeft,
	
	Equality,
	NotEquals,
	Lt,
	Gt,
	LtE,
	GtE,
	
	Percent,
	Plus,
	Minus,
	DoublePlus,	// ungood (bad joke, sorry)
	DoubleMinus,
	
	Ampersand,
	Pipe,
	Caret,
	DoubleAmpersand,
	DoublePipe,
	
	// -- Brackets
	BraceOpen,
	BraceClose,
	ParenOpen,
	ParenClose,
	SquareOpen,
	SquareClose,
	
	// -- Reserved Words
	// - Storage classes
	Rword_typedef,
	Rword_auto,
	Rword_extern,
	Rword_static,
	Rword_register,
	Rword_inline,
	// - Qualifiers
	Rword_const,
	Rword_volatile,
	Rword_restrict,
	// - Types
	Rword_void,
	Rword_Bool,
	Rword_signed,
	Rword_unsigned,
	Rword_char,
	Rword_short,
	Rword_int,
	Rword_long,
	Rword_float,
	Rword_double,
	// - Composites
	Rword_enum,
	Rword_union,
	Rword_struct,
	// - Blocks
	Rword_if,
	Rword_else,
	Rword_while,
	Rword_do,
	Rword_for,
	Rword_switch,
	// - Flow
	Rword_goto,
	Rword_continue,
	Rword_break,
	Rword_return,
	Rword_case,
	Rword_default,
	// - Meta
	Rword_sizeof,
}
impl From<Preprocessor> for Token {
	fn from(v: Preprocessor) -> Token {
		Token::Preprocessor(v)
	}
}

#[derive(Debug,PartialEq,Clone)]
pub enum Preprocessor
{
	/// #include
	Include {
		angle_brackets: bool,
		path: String,
	},
	/// EOF in an included file
	EndOfInclude,

	IfDef {
		is_not_defined: bool,
		ident: String,
	},
	If {
		tokens: Vec<Token>,
	},
	ElseIf {
		tokens: Vec<Token>,
	},
	Else,

	/// A macro definition
	MacroDefine {
		name: String,
		arg_names: Option< Vec<String> >,
		expansion: Vec<Token>,
	},
	/// Macro un-definition
	MacroUndefine {
		name: String,
	},
	/// A macro invocation/expansion
	/// NOTE: This only gets emitted if macros are being handled by the pre-processor
	MacroInvocaton {
		/// Input tokens (with whitespace stripped)
		input: Vec<Token>,
		/// Output tokens
		output: Vec<Token>,
	},
}

