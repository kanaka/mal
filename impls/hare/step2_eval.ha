use bufio;
use fmt;
use io;
use mal;
use os;
use strings;

fn read (input: []u8) (mal::MalType | io::EOF | mal::error) = {
	return mal::read_str(input)?;
};

fn eval_list(ls: mal::list, env: mal::hashmap) mal::MalType = {

	if(len(ls.data) == 0) return ls;

	const func = match(eval(ls.data[0], env)){
	case let func: mal::intrinsic =>
		yield func;
	case => return ls;
	};

	for(let i: size = 1; i < len(ls.data); i += 1){
		ls.data[i] = eval(ls.data[i], env);
	};

	return func.eval(ls.data[1..])!;
};


fn eval_vec(vec: mal::vector, env: mal::hashmap) mal::vector ={

	if(len(vec.data) == 0) return vec;

	for(let i: size = 0; i < len(vec.data); i += 1){
		vec.data[i] = eval(vec.data[i], env);
	};
	return vec;
};

fn eval_hash(
	map: mal::hashmap,
	env: mal::hashmap,
) mal::hashmap = {

	let res = mal::hm_init();

	for(let e .. map.data) {
		mal::hm_add(res, e.key, eval(e.val, env));
	};

	return res;
};

fn eval (ast: mal::MalType, env: mal::hashmap) mal::MalType = {

	let res: mal::MalType = match(ast){
	case let key: mal::symbol =>
		let v: mal::MalType = match(mal::hm_get(env, key)){
		case let v: mal::MalType =>
			yield v;
		case =>
			yield mal::nil;
		};
		yield eval(v, env);
	case let ls: mal::list =>
		yield eval_list(ls, env);
	case let vec: mal::vector =>
		yield eval_vec(vec, env);
	case let hash: mal::hashmap =>
		yield eval_hash(hash, env);
	case let func: mal::intrinsic =>
		yield func;
	case =>
		yield ast;
	};

	return res;
};

fn print (input: mal::MalType) void = {
	mal::print_form(os::stdout, input);
	fmt::print("\n")!;
};

fn rep (input: []u8, env: mal::hashmap) void = {
	match (read(input)){
	case let e: mal::error =>
		mal::format_error(os::stderr, e);
	case let form: mal::MalType =>
		print(eval(form, env));
	case io::EOF =>
		return void;
	};
};

export fn main() void = {

	const env = mal::hm_init();

	mal::hm_add(env, "+": mal::symbol, mal::make_intrinsic(&mal::plus));
	mal::hm_add(env, "-": mal::symbol, mal::make_intrinsic(&mal::minus));
	mal::hm_add(env, "*": mal::symbol, mal::make_intrinsic(&mal::mult));
	mal::hm_add(env, "/": mal::symbol, mal::make_intrinsic(&mal::div));

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
