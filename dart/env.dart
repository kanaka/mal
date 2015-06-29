library mal.env;

import "types.dart";

class Env {
  Map<MalSymbol, MalType> _env = {};
  Env _outer;
  MalList _binds;
  MalList _exprs;

  Env({outer: null, binds: null, exprs: null}) {
    // TODO(adam): we should use dart factory constructor here.
    if (binds == null) {
      _binds = new MalList();
    } else {
      _binds = binds;
    }

    if (exprs == null) {
      _exprs = new MalList();
    } else {
      _exprs = exprs;
    }

    _outer = outer;

    for (int i = 0; i < _binds.malTypes.length; i++) {
      if (_binds.malTypes[i] == BIND_SYMBOL) {
        var key = _binds.malTypes[i+1];
        _env[key] = new MalList.fromList(_exprs.malTypes.getRange(i, _exprs.malTypes.length).toList());
        break;
      } else {
        var key = _binds.malTypes[i];
        _env[key] = _exprs.malTypes[i];
      }
    }
  }

  void set(MalSymbol key, MalType value) {
    _env[key] = value;
  }

  MalType find(MalSymbol key) {
    if (_env.containsKey(key)) {
      return _env[key];
    }

    return _outer != null ? _outer.find(key) : null;
  }

  MalType get(MalSymbol key) {
    var value = find(key);

    if (value != null) {
      return value;
    }

    throw new StateError("'$key' not found");
  }
}
