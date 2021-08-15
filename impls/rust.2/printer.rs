use super::types::MalValue;

pub fn pr_str(val: MalValue, print_readably: bool) -> String {
    return val.inspect(print_readably);
}
