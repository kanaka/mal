use bufio;
use fmt;
use io;
use os;
use strings;

fn read (input: []u8) []u8 = {
	return input;
};

fn eval (input: []u8) []u8 = {
	return input;
};

fn print (input: []u8) str = {
	return strings::fromutf8(input)!;
};

fn rep (input: []u8) str = {
	return print(eval(read(input)));
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

		fmt::println(rep(input))!;
	};

};
