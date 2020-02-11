import 'dart:collection';
import 'env.dart';

abstract class MalType {
  bool get isMacro => false;
  MalType meta;

  MalType clone();
}

abstract class MalIterable extends MalType
    with ListMixin<MalType>
    implements List<MalType> {
  final List<MalType> elements;

  MalIterable(this.elements);

  MalType operator [](int index) => elements[index];
  void operator []=(int index, MalType value) {
    elements[index] = value;
  }

  int get length => elements.length;
  void set length(int newLength) {
    elements.length = newLength;
  }

  bool operator ==(other) {
    if (other is! MalIterable) return false;

    // apparently (= (list) nil) should be false...
    if (other is MalNil) return false;

    if (elements.length != other.elements.length) return false;
    for (var i = 0; i < elements.length; i++) {
      if (elements[i] != other.elements[i]) return false;
    }
    return true;
  }

  @override
  MalIterable clone();
}

class MalList extends MalIterable {
  MalList(List<MalType> elements) : super(elements);

  @override
  MalList clone() {
    return new MalList(elements.toList());
  }
}

class MalVector extends MalIterable {
  MalVector(List<MalType> elements) : super(elements);

  @override
  MalVector clone() {
    return new MalVector(elements.toList());
  }
}

class MalHashMap extends MalType {
  final Map<MalType, MalType> value;

  MalHashMap(this.value);

  MalHashMap.fromSequence(List<MalType> elements)
      : value = _mapFromSequence(elements);

  static Map<MalType, MalType> _mapFromSequence(List<MalType> elements) {
    var result = <MalType, MalType>{};

    var readingKey = true;
    MalType pendingKey;
    for (var malType in elements) {
      if (readingKey) {
        if (malType is MalString || malType is MalKeyword) {
          pendingKey = malType;
        } else {
          throw new ArgumentError('hash-map keys must be strings or keywords');
        }
      } else {
        result[pendingKey] = malType;
      }
      readingKey = !readingKey;
    }

    return result;
  }

  bool operator ==(other) {
    if (other is! MalHashMap) return false;
    var otherMap = (other as MalHashMap).value;
    if (otherMap.length != value.length) return false;
    for (var key in value.keys) {
      if (!otherMap.containsKey(key)) return false;
      if (value[key] != otherMap[key]) return false;
    }
    return true;
  }

  @override
  MalHashMap clone() {
    return new MalHashMap(new Map.from(value));
  }
}

class MalInt extends MalType {
  final int value;

  MalInt(this.value);

  bool operator ==(other) {
    if (other is! MalInt) return false;
    return other.value == value;
  }

  @override
  MalInt clone() {
    return new MalInt(value);
  }
}

class MalSymbol extends MalType {
  final String value;

  MalSymbol(this.value);

  int get hashCode => value.hashCode;

  bool operator ==(other) {
    if (other is! MalSymbol) return false;
    return value == other.value;
  }

  @override
  MalSymbol clone() {
    return new MalSymbol(value);
  }
}

class MalKeyword extends MalType {
  final String value;

  MalKeyword(this.value);

  int get hashCode => value.hashCode;

  bool operator ==(other) {
    if (other is! MalKeyword) return false;
    return value == other.value;
  }

  @override
  MalKeyword clone() {
    return new MalKeyword(value);
  }
}

class MalString extends MalType {
  final String value;

  MalString(this.value);

  int get hashCode => value.hashCode;

  bool operator ==(other) {
    if (other is! MalString) return false;
    return other.value == value;
  }

  @override
  MalString clone() {
    return new MalString(value);
  }
}

class MalBool extends MalType {
  final bool value;

  MalBool(this.value);

  bool operator ==(other) {
    if (other is! MalBool) return false;
    return other.value == value;
  }

  @override
  MalBool clone() {
    return new MalBool(value);
  }
}

class MalNil extends MalIterable {
  MalNil() : super(const <MalType>[]);

  bool operator ==(other) => other is MalNil;

  @override
  MalNil clone() {
    return new MalNil();
  }
}

class MalAtom extends MalType {
  MalType value;

  MalAtom(this.value);

  @override
  MalAtom clone() {
    return new MalAtom(value);
  }
}

abstract class MalCallable extends MalType {
  MalType call(List<MalType> args);

  bool get isMacro => false;
}

typedef MalType BuiltinFunc(List<MalType> args);

class MalBuiltin extends MalCallable {
  final BuiltinFunc func;

  MalBuiltin(this.func);

  MalType call(List<MalType> args) {
    return func(args);
  }

  @override
  MalBuiltin clone() {
    return new MalBuiltin(func);
  }
}

typedef MalType EvalFun(MalType ast, Env env);

class MalClosure extends MalCallable {
  final List<MalSymbol> params;
  final MalType ast;
  final Env env;
  final Function func;

  @override
  bool isMacro = false;

  MalClosure(this.params, this.ast, this.env, this.func);

  MalType call(List<MalType> args) {
    return func(args);
  }

  @override
  MalClosure clone() {
    var closure =
        new MalClosure(this.params.toList(), this.ast, this.env, this.func);
    closure.isMacro = this.isMacro;
    return closure;
  }
}

class MalException implements Exception {
  final MalType value;

  MalException(this.value);
}
