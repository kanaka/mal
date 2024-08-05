import 'types.dart';

class Env {
  final Env outer;

  final data = <String, MalType>{};

  Env([this.outer, List<MalSymbol> binds, List<MalType> exprs]) {
    if (binds == null) {
      assert(exprs == null);
    } else {
      assert(exprs != null &&
          (binds.length == exprs.length || binds.contains(new MalSymbol('&'))));
      for (var i = 0; i < binds.length; i++) {
        if (binds[i].value == '&') {
          set(binds[i + 1].value, new MalList(exprs.sublist(i)));
          break;
        }
        set(binds[i].value, exprs[i]);
      }
    }
  }

  void set(String key, MalType value) {
    data[key] = value;
  }

  MalType get(String key) {
    var value = data[key];
    if (value != null) {
      return value;
    }
    if (outer != null) {
      return outer.get(key);
    }
    return null;
  }
}

class NotFoundException implements Exception {
  /// The name of the symbol that was not found.
  final String value;

  NotFoundException(this.value);

  String toString() => "'$value' not found";
}
