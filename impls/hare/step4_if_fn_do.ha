use fmt;
use bufio;
use os;
use strings;
use io;
use mal;

fn read (input: []u8) (mal::MalType | io::EOF | mal::error) = {
	return mal::read_str(input);
};

fn eval_let(env: *mal::env, bindings: mal::list, body: mal::MalType...) (mal::MalType | mal::error) = {

	let let_env = mal::env_init(env);

	for(let i:size = 0; i < len(bindings); i += 2){
		mal::env_set(let_env, bindings[i] as mal::symbol,
			     eval(bindings[i+1], let_env)?);
	};

	let result: mal::MalType = mal::nil;
	for(let form .. body){
		result = eval(form, let_env)?;
	};
	return result;
};

fn eval_list(ls: mal::list, env: *mal::env) (mal::MalType | mal::error) = {

	if(len(ls) == 0) return ls;

	// handle special cases of 'if' 'fn*', 'do', 'let*' and 'def!' forms
	match(ls[0]){
	case let sym: mal::symbol =>
	     switch(sym){
		case "def!" =>
			if(len(ls) != 3)
				return ("def! expects 2 arguments", ls):mal::syntaxError;

			let val = eval(ls[2], env)?;
			mal::env_set(env, ls[1] as mal::symbol, val);
			return val;

		case "let*" =>
			if(len(ls) < 3)
				return ("let*: too few arguments", ls):mal::syntaxError;

			let bindings: []mal::MalType = match(ls[1]){
			case let b: mal::list =>
				yield b;
			case let b: mal::vector =>
				yield b;
			case =>
			     return ("let*", ls):mal::syntaxError;
			};
			return eval_let(env, bindings, ls[2..]...);
		case "do" =>
			let result: mal::MalType = mal::nil;
			for(let form .. ls[1..]){
				result = eval(form, env)?;
			};
			return result;
		case "if" =>
			if(len(ls) > 4 || len(ls) < 3)
				return ("if expects 2 or 3 arguments", ls):mal::syntaxError;
			match(eval(ls[1], env)?){
			case mal::nil =>
				if(len(ls) == 4){
					return eval(ls[3], env);
				} else {
					return mal::nil;
				};
			case let b: bool =>
				if(b){
					return eval(ls[2], env);
				} else if(len(ls) == 4){
					return eval(ls[3], env);
				} else {
					return mal::nil;
				};
			case =>
				return eval(ls[2], env);
			};
		case "fn*" =>
			let args = match(ls[1]){
			case let a: (mal::list | mal::vector) =>
				yield a;
			};
			let body = match(ls[2]){
			case let b: mal::MalValue =>
				yield b;
			case => return mal::nil;
			};
			return mal::function {
				evaluator = &eval,
				env = env,
				args = args,
				body = body,
			};
		case => void;
		};
	case => void;
	};


	match(eval(ls[0], env)?){
	case let func: mal::intrinsic =>
		let args: mal::list = [];
		defer free(args);
		for(let arg .. ls[1..]){
			append(args, eval(arg, env)?);
		};
		return func(args);
	case let func: mal::function =>
		let args: mal::list = [];
		defer free(args);
		for(let arg .. ls[1..]){
			append(args, eval(arg, env)?);
		};
		let local = mal::env_init(func.env);
		mal::env_bind(local, func.args, args);
		return eval(func.body, local);
	case => return ls;
	};
};


fn eval_vec(vec: mal::vector, env: *mal::env) (mal::vector | mal::error) ={

	if(len(vec) == 0) return vec;
	let res:mal::vector = mal::make_vec(len(vec));

	for(let i:size = 0; i < len(vec); i += 1){
		res[i] = eval(vec[i],env)?;
	};
	return res;
};

fn eval_hash(map: * nullable *mal::hm_map, env: *mal::env) (mal::hashmap | mal::error) = {

	let res = mal::hm_init();

	match(*map){
	case null => void;
	case let map:*mal::hm_map =>
		for(let child .. map.child){
			eval_hash(&child, env)?;
		};
	mal::hm_add(res, map.key, eval(map.val, env)?);
	};

	return res;
};

fn eval (ast: mal::MalType, env: *mal::env) (mal::MalType | mal::error) = {

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
	let res: mal::MalType = match(ast){
	case let key: mal::symbol =>
		yield if(strings::hasprefix(key, ':')){
			yield key;
		} else {
			yield mal::env_get(env, key)?;
		};
	case let ls: mal::list =>
		yield eval_list(ls, env)?;
	case let vec: mal::vector =>
		yield eval_vec(vec, env)?;
	case let hash: mal::hashmap =>
		yield eval_hash(hash.map, env)?;
	case let func: mal::intrinsic =>
		yield func;
	case let func: mal::function =>
		yield func;
	case => yield ast;
	};

	return res;
};

fn print (input: mal::MalType) void = {
	mal::print_form(os::stdout, input);
	fmt::print("\n")!;
};

fn rep (input: []u8, env: *mal::env) void = {
	let ast = match(read(input)){
	case let e:mal::error =>
		return mal::handle_error(e);
	case let form: mal::MalType =>
		yield form;
	case io::EOF =>
	     return void;
	};

	let result = match(eval(ast, env)){
	case let e:mal::error =>
		return mal::handle_error(e);
	case let form: mal::MalType =>
		yield form;
	};

	print(result);
};

export fn main() void = {

	const env = mal::env_init();
	mal::load_namespace(mal::core, env);

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
