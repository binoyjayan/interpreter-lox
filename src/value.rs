#[allow(dead_code)]
#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    String_(String),
    Bool(bool),
    Number(f64),
    Nil,
}
