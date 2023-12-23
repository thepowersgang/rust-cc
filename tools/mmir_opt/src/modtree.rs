use crate::types::TypeRef;
use ::std::collections::BTreeMap;

pub struct Root
{
    pub types: BTreeMap<String, Type>,
    pub functions: BTreeMap<String, Function>,
    pub statics: BTreeMap<String, Static>,
}
impl Root {
    pub fn new() -> Self {
        Root {
            types: Default::default(),
            functions: Default::default(),
            statics: Default::default(),
        }
    }
}
pub struct Type
{
    pub size: usize,
    pub align: usize,
    pub fields: Vec<Field>,
}
pub struct Field {
    pub offset: usize,
    pub ty: TypeRef,
    pub comment: Option<String>,
}
pub struct Function
{
    pub define_location: String,
    pub link_name: Option<String>,
    pub sig: crate::types::FcnTy,
    pub arg_names: Vec<String>,
    pub body: Option<crate::mir::Function>,
}
pub struct Static
{
    pub define_location: String,
    pub link_name: Option<String>,
    pub ty: TypeRef,
    pub value: Option<StaticValue>,
}
pub struct StaticValue {
    pub data: Vec<u8>,
    pub reloc: Vec<Reloc>,
}
pub struct Reloc {
    pub ofs: usize,
    pub size: usize,
    pub value: RelocVal,
}
pub enum RelocVal {
    Symbol(String),
    Data(Vec<u8>),
}