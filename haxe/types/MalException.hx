package types;

import types.Types.MalType;

class MalException {
    public var obj:MalType = null;
    public function new(obj:MalType) {
        this.obj = obj;
    }
}
