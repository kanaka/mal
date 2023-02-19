import readline { read_line }

fn rep_read(input string) string {
	return input
}

fn rep_eval(ast string) string {
	return ast
}

fn rep_print(ast string) string {
	return ast
}

fn rep(line string) string {
	return rep_print(rep_eval(rep_read(line)))
}

fn main() {
	for {
		line := read_line('user> ') or { break }
		println(rep(line))
	}
}
