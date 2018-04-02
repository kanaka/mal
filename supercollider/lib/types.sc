MALError : Error {
	*new {
		|what|
		^super.new(what)
	}
}

MALEmptyInputError : MALError {}

MALObject {
	classvar <t, <f, <n;
	var <value, <meta;

	*new {
		|value, meta|
		^super.newCopyArgs(value, meta)
	}

	dumpOn {
		|stream|
		var name = this.class.name.asString;
		stream << name << " " << value << " " << meta
	}

	== {
		|other|
		^this.class == other.class && this.value == other.value
	}
}

MALTrue : MALObject {}
MALFalse : MALObject {}
MALNil : MALObject {}
MALInt : MALObject {}
MALSymbol : MALObject {}
MALString : MALObject {}
MALKeyword : MALObject {}

MALList : MALObject {
	== {
		|other|
		var compatible = [MALList, MALVector].includes(other.class);
		^compatible && this.value == other.value
	}
}

MALVector : MALObject {
	== {
		|other|
		var compatible = [MALVector, MALList].includes(other.class);
		^compatible && this.value == other.value
	}
}

MALMap : MALObject {}

+ MALObject {
	*initClass {
		t = MALTrue(true);
		f = MALFalse(false);
		n = MALNil(nil)
	}
}