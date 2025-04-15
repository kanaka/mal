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

fn starts_with(ast: mal::MalType, sym: str) bool = {
	match(ast){
	case let ls: mal::list=>
		if(len(ls.data) < 1) return false;
		match(ls.data[0]){
		case let s: mal::symbol =>
		     return s == sym;
		case =>
		     return false;
		};
	case =>
		return false;
	};
};

fn qq_iter(ast: []mal::MalType) (mal::MalType | mal::error) = {

	let acc = mal::make_list(0);

	for(let i: size = len(ast); 0 < i ; i -= 1){

		let elt: mal::MalType  = ast[i - 1];

		if(starts_with(elt, "splice-unquote")){
			let elt: mal::list = match(elt){
			case let l: mal::list =>
				yield l;
			case =>
				return ("expected list",
					ast):mal::typeerror;
			};

			acc = mal::make_list(3, ["concat":mal::symbol,
						 elt.data[1], acc]);
		} else {
			acc = mal::make_list(3, ["cons":mal::symbol,
						 quasiquote(elt)?, acc]);
		};
	};

	return acc;
};

fn quasiquote(ast: mal::MalType) (mal::MalType | mal::error) = {
	match(ast) {
	case let ls: mal::list =>
		if(starts_with(ls, "unquote")) {
			return ls.data[1];
		} else {
			return qq_iter(ls.data);
		};
	case let ls: mal::vector =>
		let res: mal::list = mal::make_list(2, ["vec":mal::symbol,
							qq_iter(ls.data)?]);
		return res;
	case let hm: (mal::symbol | mal::hashmap) =>
		let res: mal::list = mal::make_list(2, ["quote":mal::symbol,
							ast]);
		return res;
	case =>
		return ast;
	};
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
	case "quasiquote" =>
		ast = quasiquote(ls.data[1])?;
		continue;
	case "quote" =>
		return ls.data[1];
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

		for(let i:size = 0; i < len(bindings); i += 2){
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
		case let a: mal::vector =>
			yield a.data;
		case let a: mal::list =>
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
		free(args);
		ast = func.body;
		continue;
	case => return ("not a function:", ls.data[0]):mal::syntaxError;
	};
};};

fn print (input: mal::MalType) void = {
	mal::print_form(os::stdout, input);
	fmt::print("\n")!;
	};

fn rep (input: []u8, env: *mal::env, printp:bool = true) void = {
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

	if(printp) print(result);
};


let repl_env: nullable *mal::env = null;

fn do_eval(args: []mal::MalType) (mal::MalType | mal::error) = {
	const env = match(repl_env){
	case let env: *mal::env =>
		yield env;
	case =>
		return (void, "repl_env":mal::symbol):mal::undefinedSymbol;
	};
	return eval(args[0], env);
};

export fn main() void = {

	repl_env = mal::env_init();
	const env = match(repl_env){
	case let env: *mal::env =>
		yield env;
	case =>
		fmt::fatal("No repl environment initialized!");
	};

	mal::env_set(env, "eval", mal::make_intrinsic(&do_eval));
	mal::load_namespace(mal::core, env)!;
	let load_file = "(def! load-file (fn* (f)
 (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))";

	rep(strings::toutf8(load_file), env, false);

	// handle command line arguments
	const args = os::args;

	let argvlen: size = if (len(args) > 2) {
		yield len(args)-2;
	} else {
		yield 0;
	};

	let argv = mal::make_list(argvlen);

	if (len(args) > 2){
		for(let i: size = 2; i < len(args); i += 1){
			argv.data[i-2] = &args[i]:mal::string;
		};
	};

	mal::env_set(env, mal::make_symbol("*ARGV*"), argv);

	if(len(args) > 1){
		let exec_str = strings::join("", "(load-file \"", args[1],
					     "\")");
		rep(strings::toutf8(exec_str), env, false);
		free(exec_str);
		os::exit(0);
	};

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

		rep(input, env);
		free(input);
	};
};
