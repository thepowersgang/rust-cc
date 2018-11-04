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

	// Pre-preocessor forwarding (when confiured to do so)
	PreprocessorInclude(String),
	/// A macro definition
	MacroDefine {
		name: String,
		arg_names: Option< Vec<String> >,
		expansion: Vec<Token>,
	},
	/// A macro invocation/expansion
	/// NOTE: This only gets emitted if macros are being handled by the pre-processor
	MacroInvocaton {
		/// Input tokens (with whitespace stripped)
		input: Vec<Token>,
		/// Output tokens
		output: Vec<Token>,
	},

	// -- Expression leaves
	Integer(u64, ::types::IntClass),
	Float(f64, ::types::FloatClass),
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
	Rword_gcc_attribute,
	Rword_gcc_va_arg,
}

