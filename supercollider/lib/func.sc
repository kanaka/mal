Func {
	var <ast, <params, <env, <fn, <>isMacro, <>meta;

	*new {
		|ast, params, env, fn, isMacro = false, meta|
		^super.newCopyArgs(ast, params, env, fn, isMacro, meta)
	}
}
