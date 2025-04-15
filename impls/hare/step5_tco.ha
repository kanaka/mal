use fmt;
use bufio;
use os;
use strings;
use io;
use mal;

fn read (input: []u8) (mal::MalType | io::EOF | mal::error) = {
	return mal::read_str(input);
};

fn eval_vec(vec: mal::vector, env: *mal::env) (mal::vector | mal::error) ={

	if(len(vec.data) == 0) return vec;
	let res: mal::vector = mal::make_vec(len(vec.data));

	for(let i: size = 0; i < len(vec.data); i += 1){
		res.data[i] = eval(vec.data[i], env)?;
	};
	return res;
};

fn eval_hash(map: * nullable *mal::hm_map, env: *mal::env)
	(mal::hashmap | mal::error) = {

	let res = mal::hm_init();

	match(*map){
	case null => void;
	case let map: *mal::hm_map =>
		for(let child .. map.child){
			eval_hash(&child, env)?;
		};
	mal::hm_add(res, map.key, eval(map.val, env)?);
	};

	return res;
};

fn eval (ast: mal::MalType, env: *mal::env) (mal::MalType | mal::error) = {

for(true){

	match(mal::env_get(env, "DEBUG-EVAL")){
	case mal::undefinedSymbol => void;
	case mal::nil => void;
	case =>
		fmt::print("EVAL: ")!;
		mal::print_form(os::stdout, ast);
		fmt::print("\n")!;
		mal::print_form(os::stdout, env.data);
		fmt::print("\n")!;
	};

	let ls: mal::list = match(ast){
	case let key: mal::symbol =>
		if(strings::hasprefix(key, ':')){
			return key;
		} else {
			return mal::env_get(env, key)?;
		};
	case let vec: mal::vector =>
		return eval_vec(vec, env)?;
	case let hash: mal::hashmap =>
		eval_hash(hash.map, env)?;
		return hash;
	case let ls: mal::list =>
		yield ls;
	case =>
		return ast;
	};

	if(len(ls.data) == 0) return ast;

	// handle special cases of 'if' 'fn*', 'do', 'let*' and 'def!' forms
	match(ls.data[0]){

	case let sym: mal::symbol =>
	     
	switch(sym){
	case "def!" =>
		if(len(ls.data) != 3)
			return ("def! expects 2 arguments",
				ls):mal::syntaxError;

		let val = eval(ls.data[2], env)?;
		mal::env_set(env, ls.data[1] as mal::symbol, val);
		return val;

	case "let*" =>
		if(len(ls.data) != 3)
			return ("let*: too few arguments",
				ls):mal::syntaxError;

		let bindings: []mal::MalType = match(ls.data[1]){
		case let b: mal::list =>
			yield b.data;
		case let b: mal::vector =>
			yield b.data;
		case =>
			return ("let*", ls):mal::syntaxError;
		};

		let let_env = mal::env_init(env);

		for(let i: size = 0; i < len(bindings); i += 2){
			mal::env_set(let_env, bindings[i] as mal::symbol,
			eval(bindings[i+1], let_env)?);
		};

		env = let_env;
		ast = ls.data[2];
		continue;
	case "do" =>
		let result: mal::MalType = mal::nil;
		for(let form .. ls.data[1..len(ls.data)-1]){
			result = eval(form, env)?;
		};
		ast = ls.data[len(ls.data)-1];
		continue;
	case "if" =>
		if(len(ls.data) > 4 || len(ls.data) < 3)
			return ("if expects 2 or 3 arguments",
				ls):mal::syntaxError;
		match(eval(ls.data[1], env)?){
		case mal::nil =>
			if(len(ls.data) == 4){
			ast = ls.data[3];
				continue;
			} else {
				return mal::nil;
			};
		case let b: bool =>
			if(b){
				ast = ls.data[2];
				continue;
			} else if(len(ls.data) == 4){
				ast = ls.data[3];
				continue;
			} else {
				return mal::nil;
			};
		case =>
			ast = ls.data[2];
			continue;
		};
	case "fn*" =>
		let args = match(ls.data[1]){
		case let a: mal::list =>
			yield a.data;
		case let a: mal::vector =>
			yield a.data;
		};
		let body = match(ls.data[2]){
		case let b: mal::MalType =>
			yield b;
		case => return mal::nil;
		};
		return mal::make_func(&eval, env, args, body);
	case => void;
	};
	case => void;
	};


	match(eval(ls.data[0], env)?){
	case let func: mal::intrinsic =>
		let args: []mal::MalType = [];
		defer free(args);
		for(let arg .. ls.data[1..]){
			append(args, eval(arg, env)?);
		};
		return func.eval(args);
	case let func: mal::function =>
		let args: []mal::MalType = [];
		for(let arg .. ls.data[1..]){
			append(args, eval(arg, env)?);
		};
		env = mal::env_init(func.env);
		mal::env_bind(env, func.args, args);
		ast = func.body;
		free(args);
		continue;
	case => return ("not a function:", ls.data[0]):mal::syntaxError;
	};
};};

fn print (input: mal::MalType) void = {
	mal::print_form(os::stdout, input);
	fmt::print("\n")!;
};

fn rep (input: []u8, env: *mal::env) void = {
	let ast = match(read(input)){
	case let e: mal::error =>
		return mal::handle_error(os::stderr, e);
	case let form: mal::MalType =>
		yield form;
	case io::EOF =>
		return void;
	};

	let result = match(eval(ast, env)){
	case let e: mal::error =>
		return mal::handle_error(os::stderr, e);
	case let form: mal::MalType =>
		yield form;
	};

	print(result);
};

export fn main() void = {

	const env = mal::env_init();
	mal::load_namespace(mal::core, env)!;

	for(true){

		fmt::printf("user> ")!;
		bufio::flush(os::stdout)!;

		const input =  match(bufio::read_line(os::stdin)){
		case let input: []u8 =>
			yield input;
		case io::EOF =>
			break;
		case io::error =>
			break;
		};

		defer free(input);
		rep(input, env);
	};
};
