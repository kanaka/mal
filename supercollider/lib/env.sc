MALEnv {
	var outer, data;

	*new {
		arg outer, binds = [], exprs = [];
		var data = Dictionary.new, i = 0;
		while { i < binds.size } {
			var bind = binds[i], expr = exprs[i];
			if (bind == '&') {
				bind = binds[i + 1];
				expr = MALList(exprs[i..]);
				data[bind] = expr;
				i = binds.size
			} {
				data[bind] = expr;
				i = i + 1
			}
		};
		^super.newCopyArgs(outer, data)
	}

	set {
		|key, value|
		data[key] = value;
		^value
	}

	find {
		|key|
		^data.atFail(key) {
			if (outer.notNil) {
				outer.find(key)
			} { nil }
		}
	}

	get {
		|key|
		var value = this.find(key);
		if (value.isNil) {
			MALError("'%' not found".format(key)).throw
		} { ^value }
	}
}
