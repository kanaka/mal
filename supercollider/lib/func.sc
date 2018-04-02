Func {
	var <ast, <params, <env, <fn;

	*new {
		|ast, params, env, fn|
		^super.newCopyArgs(ast, params, env, fn)
	}
}
