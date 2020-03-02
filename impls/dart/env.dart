import 'types.dart';

class Env {
  final Env outer;

  final data = <MalSymbol, MalType>{};

  Env([this.outer, List<MalSymbol> binds, List<MalType> exprs]) {
    if (binds == null) {
      assert(exprs == null);
    } else {
      assert(exprs != null &&
          (binds.length == exprs.length || binds.contains(new MalSymbol('&'))));
      for (var i = 0; i < binds.length; i++) {
        if (binds[i] == new MalSymbol('&')) {
          set(binds[i + 1], new MalList(exprs.sublist(i)));
          break;
        }
        set(binds[i], exprs[i]);
      }
    }
  }

  void set(MalSymbol key, MalType value) {
    data[key] = value;
  }

  Env find(MalSymbol key) {
    if (data[key] != null) {
      return this;
    }
    if (outer != null) {
      return outer.find(key);
    }
    return null;
  }

  MalType get(MalSymbol key) {
    var env = find(key);
    if (env != null) {
      return env.data[key];
    }
    throw new NotFoundException(key.value);
  }
}

class NotFoundException implements Exception {
  /// The name of the symbol that was not found.
  final String value;

  NotFoundException(this.value);

  String toString() => "'$value' not found";
}
