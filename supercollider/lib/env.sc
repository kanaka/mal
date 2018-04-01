MALEnv {
	var outer, data;

	*new {
		|outer|
		var data = Dictionary.new;
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
