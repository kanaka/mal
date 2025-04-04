use fmt;
use bufio;
use os;
use strings;
use io;
use mal;

fn read (input: []u8) (mal::MalType | io::EOF | mal::error) = {
	return mal::read_str(input)?;
};

fn eval (input: mal::MalType) mal::MalType = {
	return input;
};

fn print (input: mal::MalType) void = {
	mal::print_form(os::stdout, input);
	fmt::print("\n")!;
};

fn rep (input: []u8) void = {
	match (read(input)){
	case let e: mal::error =>
	     mal::handle_error(e);
	case let form: mal::MalType =>
		print(eval(form));
	case io::EOF =>
	     return void;
	};
};

export fn main() void = {

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
		rep(input);
	};

};
