use fmt;
use bufio;
use os;
use strings;
use io;
use mal;

fn read (input: []u8) (mal::MalType | io::EOF | mal::error) = {
	return mal::read_str(input)?;
};

fn eval_list(ls: mal::list, env: mal::hashmap) mal::MalType = {

	if(len(ls) == 0) return ls;

	const func = match(eval(ls[0], env)){
		case let func: mal::intrinsic =>
			yield func;
		case => return ls;
	};

	for(let i:size = 1; i < len(ls); i += 1){
		ls[i] = eval(ls[i], env);
	};

	return func(ls[1..]...);
};


fn eval_vec(vec: mal::vector, env: mal::hashmap) mal::vector ={
	if(len(vec) == 0) return vec;
	for(let i:size = 0; i < len(vec); i += 1){
		vec[i] = eval(vec[i],env);
	};
	return vec;
};

fn eval_hash(map: * nullable *mal::hm_map, env: mal::hashmap) void = {

	match(*map){
	case null => return void;
	case let map:*mal::hm_map =>
		for(let child .. map.child){
			eval_hash(&child, env);
		};
	map.val = eval(map.val, env);
	};
};

fn eval (ast: mal::MalType, env: mal::hashmap) mal::MalType = {

	let res: mal::MalType = match(ast){
	case let key: mal::symbol =>
		yield eval(mal::hm_get(env, key), env);
	case let ls: mal::list =>
		yield eval_list(ls, env);
	case let vec: mal::vector =>
		yield eval_vec(vec, env);
	case let hash: mal::hashmap =>
		eval_hash(hash.map, env);
		yield hash;
	case let func: mal::intrinsic =>
		yield func;
	case => yield ast;
	};

	return res;
};

fn print (input: mal::MalType) void = {
	mal::print_form(input);
	fmt::print("\n")!;
};

fn rep (input: []u8, env: mal::hashmap) void = {
	match (read(input)){
	case let e: mal::error =>
	     mal::handle_error(e);
	case let form: mal::MalType =>
		print(eval(form, env));
	case io::EOF =>
	     return void;
	};
};

export fn main() void = {

	const env = mal::hm_init();

	mal::hm_add(env, "+":mal::symbol, &mal::plus:mal::intrinsic);
	mal::hm_add(env, "-":mal::symbol, &mal::minus:mal::intrinsic);
	mal::hm_add(env, "*":mal::symbol, &mal::mult:mal::intrinsic);
	mal::hm_add(env, "/":mal::symbol, &mal::div:mal::intrinsic);

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
