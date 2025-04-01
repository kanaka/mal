export fn plus (args: MalType...) MalType = {

	let result: number = 0;
	for(let n .. args) {
		result += n as number;
	};
	return result;
};

export fn minus (args: MalType...) MalType = {

	let result: number = args[0] as number;
	for(let n .. args[1..]) {
		result -= n as number;
	};
	return result;
};

export fn mult (args: MalType...) MalType = {

	let result: number = 1;
	for(let n .. args) {
		result *= n as number;
	};
	return result;
};


export fn div (args: MalType...) MalType = {
	let x = args[0] as number;
	let y = switch(len(args)){
	case 2 =>
	     yield args[1] as number;
	case 1 =>
		yield 1:number; 
	case 0 =>
	     yield 1:number;
	case =>
	     yield div(args[1..]...) as number;
	};

	return x / y;
};
