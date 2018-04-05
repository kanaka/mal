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
		])
	}
}
