
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    ByteString(Vec<u8>),
    UnicodeString(String),
    Regex(String),
    ByteChar(u8),
    UnicodeChar(char),
    Boolean(bool),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
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
    Array(Box<Type>, Option<Expr>),
    Pointer(Box<Type>),
    Section(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Condition {
    pub condition: Expr,
    pub left: Box<TyConstraint>,
    pub right: Box<TyConstraint>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TyConstraint {
    Literal(Literal),
    Type(Type),
    Union(Vec<TyConstraint>),
    Conditional(Condition),
    Global(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constraint {
    pub name: Option<String>,
    pub ty_constraint: TyConstraint,
    pub val_constraint: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SectionKind {
    Section,
    File,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Section {
    pub kind: SectionKind,
    pub name: String,
    pub constraints: Vec<Constraint>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDecl {
    Rule(Type, String),
    Alias(String, TyConstraint, String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Encoding {
    pub encoding: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TopLevel {
    Section(Section),
    File(Section),
    Encoding(Encoding),
    Type(TypeDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct File {
    pub inner: Vec<TopLevel>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnOp {
    Neg,
    Inv,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinOp {
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
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Array(Vec<Expr>),
    Parens(Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
}
