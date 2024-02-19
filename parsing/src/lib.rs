use lalrpop_util::lalrpop_mod;

pub mod ast;
lalrpop_mod! {
    #[allow(unused)]
    pub parser
}