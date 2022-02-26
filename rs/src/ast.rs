
#[derive(Clone, Debug, PartialEq)]
pub enum CtLiteral {
    Integer(i128),
    Float(f64),
    ByteString(Vec<u8>),
    UnicodeString(String),
    Regex(String),
    ByteChar(u8),
    UnicodeChar(char),
    Boolean(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtType {
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
    Array(Box<CtType>, Option<CtExpr>),
    Pointer(Box<CtType>),
    Section(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtCondition {
    pub condition: CtExpr,
    pub left: Box<CtTyConstraint>,
    pub right: Box<CtTyConstraint>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtTyConstraint {
    Literal(CtLiteral),
    Type(CtType),
    Union(Vec<CtTyConstraint>),
    Conditional(CtCondition),
    Global(CtType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtConstraint {
    pub name: Option<String>,
    pub ty_constraint: CtTyConstraint,
    pub val_constraint: Option<CtExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SectionKind {
    Section,
    File,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtSection {
    pub kind: SectionKind,
    pub name: String,
    pub constraints: Vec<CtConstraint>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtTypeDecl {
    Rule(CtType, String),
    Alias(String, CtTyConstraint, String)
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtEncoding {
    pub encoding: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtTopLevel {
    Section(CtSection),
    File(CtSection),
    Encoding(CtEncoding),
    Type(CtTypeDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CtAst {
    pub inner: Vec<CtTopLevel>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CtUnaryOp {
    Neg,
    Inv,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CtBinOp {
    Is,
    And,
    Or,
    Lt,
    Gt,
    Ge,
    Le,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtExpr {
    Literal(CtLiteral),
    Array(Vec<CtExpr>),
    Parens(Box<CtExpr>),
    Unary(CtUnaryOp, Box<CtExpr>),
    Binary(Box<CtExpr>, CtBinOp, Box<CtExpr>),
    Ident(String),
}
