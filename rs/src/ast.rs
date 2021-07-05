
#[derive(Debug)]
pub enum CtLiteral {
    Integer(i128),
    Float(f64),
    ByteString(Vec<u8>),
    UnicodeString(String),
    Regex(String),
    Char(char),
    Boolean(bool),
    Array(Vec<CtLiteral>),
}

#[derive(Debug)]
pub enum BuiltinType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    String,
    Bool,
    Null,
    Array(Box<CtType>, Option<usize>),
    Pointer(Box<CtType>),
}

#[derive(Debug)]
pub enum CtType {
    Builtin(BuiltinType),
    Section(String),
}

#[derive(Debug)]
pub struct CtCondition {

}

#[derive(Debug)]
pub enum CtTyConstraint {
    Literal(CtLiteral),
    Type(CtType),
    Union(Vec<CtType>),
    Conditional(CtCondition),
    Global(CtType),
}

#[derive(Debug)]
pub struct CtValConstraint {

}

#[derive(Debug)]
pub struct CtConstraint {
    name: Option<String>,
    ty_constraint: CtTyConstraint,
    val_constraint: Option<CtValConstraint>
}

#[derive(Debug)]
pub enum SectionKind {
    Section,
    File,
}

#[derive(Debug)]
pub struct CtSection {
    pub kind: SectionKind,
    pub name: String,
    pub constraints: Vec<CtConstraint>,
}

#[derive(Debug)]
pub enum CtTypeDecl {
    Rule(CtType, String),
    Alias(String, CtConstraint, String)
}

#[derive(Debug)]
pub struct CtEncoding {
    pub encoding: String,
}

#[derive(Debug)]
pub struct CtAst {
    pub encoding: Option<CtEncoding>,
    pub type_decls: Vec<CtTypeDecl>,
    pub sections: Vec<CtSection>,
}
