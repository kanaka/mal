use fmt;
use os;
use memio;

export fn plus (args: list) (MalType | error) = {

	let result: number = 0;
	for(let n .. args) {
		result += n as number;
	};
	return result;
};

export fn minus (args: list) (MalType | error) = {

	let result: number = args[0] as number;
	for(let n .. args[1..]) {
		result -= n as number;
	};
	return result;
};

export fn mult (args: list) (MalType | error) = {

	let result: number = 1;
	for(let n .. args) {
		result *= n as number;
	};
	return result;
};


export fn div (args: list) (MalType | error) = {
	let x = args[0] as number;
	let y = switch(len(args)){
	case 2 =>
	     yield args[1] as number;
	case 1 =>
		yield 1:number;
	case 0 =>
	     yield 1:number;
	case =>
	     yield div(args[1..]) as number;
	};

	return x / y;
};

export fn mallist (args: list) (MalType | error) = {
	return args:list;
};

export fn listp (args: list) (MalType | error) = {
	return args[0] is list;
};


export fn emptyp (args: list) (MalType | error) = {

	let a:[]MalType = match(args[0]){
	case let a: vector =>
		yield a;
	case let a: list =>
		yield a;
	case => return nil;
	};

	return len(a) == 0;
};

export fn count (args: list) (MalType | error) = {
	let arg:[]MalType = match(args[0]) {
	case let a: list =>
		yield a;
	case let a: vector =>
		yield a;
	case nil => return 0;
	};
	return len(arg):number;
};

export fn greater_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return ("> expected exactly 2 args, got:", args):syntaxError;

	return args[0] as number > args[1] as number;
};

export fn smaller_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return ("< expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number < args[1] as number;
};


export fn greq_than (args: list) (MalType | error) = {
	if(len(args) != 2)
		return (">= expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number >= args[1] as number;
};

export fn seq_than (args: list) (MalType | error) = {
	if(len(args) != 2)
	  return ("<= expected exactly 2 args, got:", args):syntaxError;
	return args[0] as number <= args[1] as number;
};

fn list_cmp (ls: collection, ls2: collection) bool = {

	let x:[]MalType = match(ls){
		case let x:list =>
			yield x;
		case let x:vector =>
			yield x;
	};

	let y:[]MalType = match(ls2){
		case let x:list =>
			yield x;
		case let x:vector =>
			yield x;
	};

	if(!(len(x) == len(y)))
		return false;

	for(let i:size = 0; i < len(x); i += 1){
		if(!(mal_eq([x[i], y[i]]) as bool)){
			return false;
		};
	};
	return true;
};

export fn mal_eq (args: list) (MalType | error) = {

	match(args[0]){
	case let x: number =>
		if(args[1] is number) {
			return x == args[1] as number;
		};
	case let x: bool =>
		if(args[1] is bool) {
			return x == args[1] as bool;
		};
	case let x: collection =>
		if(args[1] is collection) {
			return list_cmp(x, args[1] as collection);
		};
	case let x: nil =>
		if(args[1] is nil) {
			return true;
		};
	case let x: string =>
		if(args[1] is string) {
			return x == args[1] as string;
		};
	case let s:symbol =>
		if(args[1] is symbol){
			return s == args[1] as symbol;
		};
	case => void;
	};
	return false;
};

export fn not (args: list) (MalType | error) = {
	match(args[0]){
	case let b: bool =>
		return !b;
	case nil =>
		return true;
	case =>
		return false;
	};
};

export fn prn (args: list) (MalType | error) = {

	for(let i:size = 0; i < len(args); i += 1) {
		print_form(os::stdout, args[i]);
		if (i < len(args) - 1)
			fmt::fprint(os::stdout, " ")!;
	};
	fmt::fprint(os::stdout, "\n")!;
	return nil;
};


export fn prn_line (args: list) (MalType | error) = {

	for(let i:size = 0; i < len(args); i += 1) {
		print_form(os::stdout, args[i], false);
		if (i < len(args) - 1)
			fmt::fprint(os::stdout, " ")!;
	};
	fmt::fprint(os::stdout, "\n")!;
	return nil;
};

export fn pr_str(args: list) (MalType | error) = {

	let strbuf = memio::dynamic();
	for(let i:size = 0; i < len(args); i += 1) {
		print_form(&strbuf, args[i]);
		if (i < len(args) - 1)
			fmt::fprint(&strbuf, " ")!;
	};
	return memio::string(&strbuf):string;
};


export fn pr_str_ugly(args: list) (MalType | error) = {

	let strbuf = memio::dynamic();
	for(let i:size = 0; i < len(args); i += 1) {
		print_form(&strbuf, args[i], false);
	};
	return memio::string(&strbuf):string;
};
