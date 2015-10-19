@include "types.awk"
@include "reader.awk"
@include "printer.awk"

function READ(str)
{
	return reader_read_str(str)
}

function EVAL(ast)
{
	return ast
}

function PRINT(expr)
{
	return printer_pr_str(expr, 1)
}

function rep(str,    ast, expr)
{
	ast = READ(str)
	if (ast ~ /^!/) {
		return ast
	}
	expr = EVAL(ast)
	if (expr ~ /^!/) {
		return expr
	}
	return PRINT(expr)
}

function main(str, ret)
{
	while (1) {
		printf("user> ")
		if (getline str <= 0) {
			break
		}
		ret = rep(str)
		if (ret ~ /^!/) {
			print "ERROR: " printer_pr_str(substr(ret, 2))
		} else {
			print ret
		}
	}
}

BEGIN {
	main()
	exit(0)
}
