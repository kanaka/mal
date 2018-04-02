Core {
	classvar <ns;

	*coerce { |x| if (x) { ^MALObject.t } { ^MALObject.f } }

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
		])
	}
}
