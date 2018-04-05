Func {
	var <ast, <params, <env, <fn, <>isMacro;

	*new {
		|ast, params, env, fn, isMacro = false|
		^super.newCopyArgs(ast, params, env, fn, isMacro)
	}
}
