Core {
	classvar <ns;

	*coerce { |x| if (x) { ^MALObject.t } { ^MALObject.f } }

	*swap {
		|atom, fn, args|
		var f = fn;
		if (fn.class == Func) { f = fn.fn };
		atom.value = f.(*([atom.value] ++ args));
		^atom.value
	}

	*seq {
		 |x|
		^switch(x.class,
			MALList, { if (x.value.isEmpty)
				{ MALObject.n }
				{ x } },
			MALVector, { if (x.value.isEmpty)
				{ MALObject.n }
				{ MALList(x.value) } },
			MALString, { if (x.value.isEmpty)
				{ MALObject.n }
				{ MALList(List.newFrom(x.value.explode.collect { |s| MALString(s) })) } },
			{ MALObject.n })
	}

	*initClass {
		ns = Dictionary.newFrom([
			'+', { |a, b| MALInt(a.value + b.value) },
			'-', { |a, b| MALInt(a.value - b.value) },
			'*', { |a, b| MALInt(a.value * b.value) },
			'/', { |a, b| MALInt(a.value / b.value) },

			'list', { |...args| MALList(List.newFrom(args)) },
			'list?', { |x| Core.coerce(x.class == MALList) },
			'empty?', { |x| Core.coerce(x.value.isEmpty) },
			'count', { |x| MALInt(x.value.size) },

			'=', { |a, b| Core.coerce(a.value == b.value) },
			'<', { |a, b| Core.coerce(a.value < b.value) },
			'<=', { |a, b| Core.coerce(a.value <= b.value) },
			'>', { |a, b| Core.coerce(a.value > b.value) },
			'>=', { |a, b| Core.coerce(a.value >= b.value) },

			'pr-str', { |...xs| MALString(xs.collect { |x| Printer.prStr(x, true) }.join(" ")) },
			'str', { |...xs| MALString(xs.collect { |x| Printer.prStr(x) }.join("")) },
			'prn', { |...xs| xs.collect { |x| Printer.prStr(x, true) }.join(" ").postln; MALObject.n },
			'println', { |...xs| xs.collect { |x| Printer.prStr(x) }.join(" ").postln; MALObject.n},

			'read-string', { |str| Reader.readStr(str.value) },
			'slurp', { |path| MALString(File.use(path.value, "r", { |f| f.readAllString })) },

			'atom', { |x| MALAtom(x) },
			'atom?', { |x| Core.coerce(x.class == MALAtom) },
			'deref', { |atom| atom.value },
			'reset!', { |atom, x| atom.value = x; x },
			'swap!', { |atom, fn ...args| Core.swap(atom, fn, args) },

			'cons', { |x, xs| MALList(List.newFrom([x] ++ xs.value)) },
			'concat', { |...xs| MALList(List.newFrom(xs.collect(_.value).reduce('++'))) },
			'nth', { |xs, i| if (i.value < xs.value.size) { xs.value[i.value] } { MALError("out of bounds").throw } },
			'first', { |xs| if (xs.class == MALNil or: { xs.value.isEmpty }) { MALObject.n } { xs.value[0] } },
			'rest', { |xs| if (xs.class == MALNil or: { xs.value.isEmpty }) { MALList(List.new) } { MALList(List.newFrom(xs.value[1..])) } },

			'throw', { |x| MALError(x).throw },
			'apply', { |fn ...args| var f = fn; if (fn.class == Func) { f = fn.fn }; f.value(*(args.copyRange(0, args.size - 2) ++ args[args.size - 1].value)) },
			'map', { |fn, xs| var f = fn; if (fn.class == Func) { f = fn.fn }; MALList(List.newFrom(xs.value.collect(f.value(_)))) },

			'nil?', { |x| Core.coerce(x.class == MALNil) },
			'true?', { |x| Core.coerce(x.class == MALTrue) },
			'false?', { |x| Core.coerce(x.class == MALFalse) },
			'number?', { |x| Core.coerce(x.class == MALInt) },
			'symbol?', { |x| Core.coerce(x.class == MALSymbol) },
			'symbol', { |string| MALSymbol(string.value.asSymbol) },
			'keyword?', { |x| Core.coerce(x.class == MALKeyword) },
			'keyword', { |string| MALKeyword(string.value.asSymbol) },
			'string?', { |x| Core.coerce(x.class == MALString) },
			'fn?', { |x| Core.coerce(switch(x.class, Function, { true }, Func, { x.isMacro.not }, { false } )) },
			'macro?', { |x| Core.coerce(x.class == Func and: { x.isMacro }) },
			'vector?', { |x| Core.coerce(x.class == MALVector) },
			'vector', { |...xs| MALVector(List.newFrom(xs)) },
			'map?', { |x| Core.coerce(x.class == MALMap) },
			'hash-map', { |...xs| MALMap(Dictionary.newFrom(xs)) },
			'sequential?', { |x| Core.coerce([MALList, MALVector].includes(x.class)) },

			'assoc', { |m ...kvs| MALMap(Dictionary.newFrom(m.value.asPairs ++ kvs)) },
			'dissoc', { |m ...ks| var dict = Dictionary.newFrom(m.value.asPairs); ks.do { |key| dict.removeAt(key) }; MALMap(dict) },
			'get', { |m, k| if (m.class == MALNil) { m } { m.value.atFail(k) { MALObject.n } } },
			'contains?', { |m, k| Core.coerce(m.value.at(k).notNil) },
			'keys', { |m| MALList(List.newFrom(m.value.keys)) },
			'vals', { |m| MALList(List.newFrom(m.value.values)) },

			'readline', { |prompt| var input = prompt.value.readline; if (input.notNil) { MALString(input) } { MALObject.n } },
			'meta', { |x| if (x.class == Function or: { x.meta.isNil }) { MALObject.n } { x.meta } },
			'with-meta', { |x, meta| var y = x.copy; y.meta = meta; y },
			'time-ms', { MALInt(Process.elapsedTime * 1000) },
			'conj', { |xs ...args| if (xs.class == MALList) { MALList(List.newFrom( args.reverse ++ xs.value)) } { MALVector(List.newFrom(xs.value ++ args)) } },
			'seq', { |x| Core.seq(x) },
		])
	}
}
