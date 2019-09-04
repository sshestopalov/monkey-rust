use super::object::{BuiltinFunction, Object};

fn len(args: &[Option<Object>]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments supplied. Expected 1, found {}.",
            args.len()
        ));
    }

    if let Some(Some(Object::Error(err))) = args
        .iter()
        .find(|v| v.as_ref().map_or(false, Object::is_error))
    {
        return Object::Error(err.clone());
    }

    if let Some(Object::Str(string)) = &args[0] {
        return Object::Integer(string.len() as i64);
    }

    if let Some(Object::Array(array)) = &args[0] {
        return Object::Integer(array.len() as i64);
    }

    return Object::Error(format!(
        "Argument to 'len' not supported, got {}",
        args[0].as_ref().unwrap().object_type()
    ));
}

fn reverse(args: &[Option<Object>]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments supplied. Expected 1, found {}.",
            args.len()
        ));
    }

    if let Some(Some(Object::Error(message))) = args
        .iter()
        .find(|v| v.as_ref().map_or(false, Object::is_error))
    {
        return Object::Error(message.clone());
    }

    if let Some(Object::Str(string)) = &args[0] {
        return Object::Str(string.chars().rev().collect::<String>());
    }

    if let Some(Object::Array(array)) = &args[0] {
        return Object::Array(array.clone().into_iter().rev().collect());
    }

    return Object::Error(format!(
        "argument to \"reverse\" not supported: {}",
        args[0].as_ref().unwrap().object_type()
    ));
}

fn first(args: &[Option<Object>]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments supplied. Expected 1, found {}.",
            args.len()
        ));
    }

    if let Some(Some(Object::Error(message))) = args
        .iter()
        .find(|v| v.as_ref().map_or(false, Object::is_error))
    {
        return Object::Error(message.clone());
    }

    if let Some(Object::Array(array)) = &args[0] {
        return array[0].clone();
    }

    return Object::Error(format!(
        "argument to \"first\" not supported: {}",
        args[0].as_ref().unwrap().object_type()
    ));
}

fn last(args: &[Option<Object>]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments supplied. Expected 1, found {}.",
            args.len()
        ));
    }

    if let Some(Some(Object::Error(message))) = args
        .iter()
        .find(|v| v.as_ref().map_or(false, Object::is_error))
    {
        return Object::Error(message.clone());
    }

    if let Some(Object::Array(array)) = &args[0] {
        return if array.is_empty() {
            Object::Null
        } else {
            array.iter().last().unwrap().clone()
        };
    }

    return Object::Error(format!(
        "argument to \"last\" not supported: {}",
        args[0].as_ref().unwrap().object_type()
    ));
}

pub const BUILTINS: &'static [(&'static str, Object)] = &[
    (
        "len",
        Object::BuiltinFunction(BuiltinFunction { func: len }),
    ),
    (
        "reverse",
        Object::BuiltinFunction(BuiltinFunction { func: reverse }),
    ),
    (
        "first",
        Object::BuiltinFunction(BuiltinFunction { func: first }),
    ),
    (
        "last",
        Object::BuiltinFunction(BuiltinFunction { func: last }),
    ),
];
