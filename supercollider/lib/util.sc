+ String {
	sub {
		|re, rep|
		var match = this.findRegexp(re)[0], from, to;
		if (match.notNil) {
			to = match[0] - 1;
			from = match[0] + match[1].size;
			^this[..to] ++ rep ++ this[from..]
		} {
			^this
		}
	}

	readline {
		this.post;
		^File.use("/dev/stdin", "r") { |f| f.getLine }
	}
}
