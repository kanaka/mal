use fmt;
use bufio;
use os;
use strings;
use io;
use mal;

fn read (input: []u8) (mal::MalType | io::EOF | mal::error) = {
	return mal::read_str(input)?;
};

fn eval_let(
	env: *mal::env,
	bindings: mal::list,
	body: mal::MalType...
) (mal::MalType | mal::error) = {

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

	// handle special cases of 'let*' and 'def!' forms
	match(ls[0]){
	case let sym: mal::symbol =>
		if(sym == "def!"){
			if(len(ls) != 3)
				return ("def! expects 2 arguments",
					ls): mal::syntaxError;

			let val = eval(ls[2], env)?;
			mal::env_set(env, ls[1] as mal::symbol, val);
			return val;

		} else if(sym == "let*"){
			if(len(ls) < 3)
				return ("let*: too few arguments",
					ls): mal::syntaxError;

			let bindings: []mal::MalType = match(ls[1]){
			case let b: mal::list =>
				yield b;
			case let b: mal::vector =>
				yield b;
			case =>
			     return ("let*", ls):mal::syntaxError;
			};
			return eval_let(env, bindings, ls[2..]...);
		};
	case => void;
	};

	const func = match(eval(ls[0], env)?){
		case let func: mal::intrinsic =>
			yield func;
		case => return ls;
	};

	for(let i: size = 1; i < len(ls); i += 1){
		ls[i] = eval(ls[i], env)?;
	};

	return func.eval(ls[1..]);
};


fn eval_vec(vec: mal::vector, env: *mal::env) (mal::vector | mal::error) ={

	let res:mal::vector = mal::make_vec(len(vec));

	if(len(vec) == 0) return vec;
	for(let i:size = 0; i < len(vec); i += 1){
		res[i] = eval(vec[i],env)?;
	};
	return res;
};

fn eval_hash(
	map: * nullable *mal::hm_map,
	env: *mal::env
) (mal::hashmap | mal::error) = {

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
	case mal::undefinedSymbol =>
		void;
	case mal::nil =>
		void;
	case => 
		fmt::print("EVAL: ")!;
		mal::print_form(os::stdout, ast);
		fmt::print("\n")!;
		mal::print_form(os::stdout, env.data);
		fmt::print("\n")!;
	};

	let res: mal::MalType = match(ast){
	case let key: mal::symbol =>
		yield eval(mal::env_get(env, key)?, env)?;
	case let ls: mal::list =>
		yield eval_list(ls, env)?;
	case let vec: mal::vector =>
		yield eval_vec(vec, env)?;
	case let hash: mal::hashmap =>
		yield eval_hash(hash.map, env)?;
	case let func: mal::intrinsic =>
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
	let ast = match (read(input)){
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

	mal::env_set(env, "nil":mal::symbol, mal::nil);
	mal::load_namespace(mal::core, env)!;

	for(true){

		fmt::printf("user> ")!;
		bufio::flush(os::stdout)!;

		const input = match(bufio::read_line(os::stdin)){
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
