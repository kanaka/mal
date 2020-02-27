function READ(str)
{
	return str
}

function EVAL(ast)
{
	return ast
}

function PRINT(expr)
{
	return expr
}

function rep(str)
{
	return PRINT(EVAL(READ(str)))
}

function main(str)
{
	while (1) {
		printf("user> ")
		if (getline str <= 0) {
			break
		}
		print rep(str)
	}
}

BEGIN {
	main()
	exit(0)
}
