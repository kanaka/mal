use super::types::MalType;
use super::MalError;
use std::collections::HashMap;

fn setup_env(lookup: &mut HashMap<String, super::types::MalFn>) {
    lookup.insert("+".to_owned(), numeric_fn(Box::new(|a, b| a + b), 0.0));
}

fn numeric_fn(f: Box<dyn Fn(f32, f32) -> f32>, init: f32) -> super::types::MalFn {
    Box::new(move |args| {
        let mut res = init;

        for a in args {
            match a {
                MalType::Number(a) => res = f(res, *a),
                _ => {
                    return Err(MalError::NonNumericArguments);
                }
            }
        }

        Ok(MalType::Number(res))
    })
}
