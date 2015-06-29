library mal.types;

import 'dart:mirrors';

abstract class MalType extends Function {
  MalType meta;
  copy() { throw new Error(); }
  call() { throw new Error(); }
  @override
  String toString([bool print_readable = false]) { return "MalType"; }
}

class MalList extends MalType {
  @override
  MalType meta = new MalNil();

  // TODO(adam): should create a varargs constructor for MalList and MalVector.
  final List<MalType> malTypes;
  MalList(): malTypes = new List<MalType>();
  MalList.fromList(this.malTypes);

  MalList copy() {
    MalList malList = new MalList.fromList(malTypes);
    malList.meta = meta;
    return malList;
  }

  /**
   * nth: this function takes a list (or vector) and a number (index) as arguments, returns the element of the
   * list at the given index. If the index is out of range, this function raises an exception.
   */
  MalType nth(int index) {
    if (index >= malTypes.length || index < 0) {
      throw new StateError("nth cannot index into $index element of $malTypes");
    }

    return malTypes[index];
  }

  MalList conjBANG(List args) {
    args.forEach((arg) {
      malTypes.add(arg);
    });

    return this;
  }

  /**
   * rest: this function takes a list (or vector) as its argument and returns a new list containing all the elements
   * except the first.
   */
  MalList rest() {
    if (malTypes.isEmpty) {
      return new MalList();
    }

    return new MalList.fromList(malTypes.toList()..removeAt(0));
  }

  MalType first() {
    return malTypes.first;
  }

  @override
  String toString([bool print_readable = false]) => "(${malTypes.join(' ')})";
}

// TODO(ADAM): do not inherit from mallist, treat them as two seprate types and then fix the type checks.
class MalVector extends MalList {
  MalList copy() {
    MalVector malVector = new MalVector();
    malVector.malTypes.addAll(malTypes);
    malVector.meta = meta;
    return malVector;
  }

  @override
  String toString([bool print_readable = false]) => "[${malTypes.join(' ')}]";
}

class MalHashMap extends MalType {
  @override
  MalType meta = new MalNil();

  // TODO(ADAM): Object should be MalType
  final Map<Object, MalType> malHashMap;
  MalHashMap(): malHashMap = new Map<Object, MalType>();
  MalHashMap.fromMalList(MalList malList): malHashMap = new Map<Object, MalType>() {
    for (int i = 0; i < malList.malTypes.length; i+=2) {
      MalType key = malList.malTypes[i];
      MalType val = malList.malTypes[i+1];
      // malHashMap[key.toString()] = val;
      malHashMap[key] = val;
    }
  }

  MalHashMap copy() {
    return clone();
  }

  MalHashMap clone() {
    MalList malList = new MalList();
    malHashMap.forEach((k,v) => malList.malTypes.addAll([k,v]));
    MalHashMap clonedMap = new MalHashMap.fromMalList(malList);
    clonedMap.meta = meta;
    return clonedMap;
  }


  MalHashMap assocBang(MalList malList) {
    for (int i = 0; i < malList.malTypes.length; i+=2) {
      var key = malList.malTypes[i];
      var value = malList.malTypes[i+1];
      malHashMap[key] = value;
    }

    return this;
  }

  MalHashMap dissocBang(MalList malList) {
    for (int i = 0; i < malList.malTypes.length; i++) {
      var key = malList.malTypes[i];
      malHashMap.remove(key);
    }

    return this;
  }

  @override
  String toString([bool print_readable = false]) {
    StringBuffer sb = new StringBuffer();
    malHashMap.forEach((Object k,MalType v) {
      sb.write('${(k as MalType).toString(true)} ${v.toString()}');
    });

    return "{${sb.toString()}}";
  }
}

class MalNumber extends MalType {
  @override
  MalType meta = new MalNil();

  final num number;
  MalNumber(this.number);

  MalNumber copy() {
    return this;
  }

  @override
  String toString([bool print_readable = false]) => number.toString();
}

class MalSymbol extends MalType {
  @override
  MalType meta = new MalNil();

  final String symbol;
  MalSymbol(this.symbol);

  MalSymbol copy() {
    return this;
  }

  @override
  String toString([bool print_readable = false]) => symbol.toString();

  @override
  bool operator ==(other) {
    if (other.runtimeType == this.runtimeType && other.symbol == this.symbol) {
      return true;
    } else {
      return false;
    }
  }

  @override
  int get hashCode {
    return symbol.hashCode;
  }
}

class MalKeyword extends MalType {
  @override
  MalType meta = new MalNil();

  static final K = ":";
  final String keyword;
  MalKeyword(this.keyword);

  MalKeyword copy() {
    return this;
  }

  @override
  String toString([bool print_readable = false]) => keyword.toString();

  @override
  bool operator ==(other) {
    if (other.runtimeType == this.runtimeType && other.keyword == this.keyword) {
      return true;
    } else {
      return false;
    }
  }

  @override
  int get hashCode {
    return keyword.hashCode;
  }
}

final BIND_SYMBOL = new MalSymbol("&");

class MalString extends MalType {
  @override
  MalType meta = new MalNil();

  final String string;
  MalString(this.string);

  MalString copy() {
    return this;
  }

  @override
  String toString([bool print_readable = false]) {

    if (print_readable) {
      return '"${string.toString()}"';
    } else {
      return string.toString();
    }
  }

  bool operator == (o) => o is MalString && o.string == string;
  int get hashCode => string.hashCode;
}

class MalBoolean extends MalType {

  final bool value;
  MalBoolean(this.value);

  MalBoolean copy() {
    return this;
  }

  String toString([bool print_readable = false]) => "${this.value}";
}

class MalTrue extends MalBoolean {
  @override
  MalType meta = new MalNil();

  MalTrue(): super(true);
}

class MalFalse extends MalBoolean {
  MalFalse(): super(false);
}

class MalNil extends MalFalse {
  MalNil(): super() {
    meta = this;
  }

  @override
  String toString([bool print_readable = false]) => "nil";
}

// Need a better way to make this final.
final MAL_NIL = new MalNil();
final MAL_TRUE = new MalTrue();
final MAL_FALSE = new MalFalse();

typedef dynamic OnCall(List);

class VarargsFunction extends MalType {
  @override
  MalType meta = new MalNil();

  OnCall _onCall;
  var ast;
  var env;
  var fParams;
  var macro = false;

  VarargsFunction(this._onCall, {this.ast, this.env, this.fParams});

  call() => _onCall([]);

  noSuchMethod(Invocation invocation) {
    final arguments = invocation.positionalArguments;
    return _onCall(arguments);
  }

  void setMacro() {
    macro = true;
  }

  VarargsFunction copy() {
    return new VarargsFunction(this._onCall, ast: this.ast, env: this.env, fParams: this.fParams);
  }

  @override
  String toString([bool print_readable = false]) => "MalFunction";
}

class MalFunction extends VarargsFunction {
  MalFunction(OnCall onCall, {ast: null, env: null, fParams: null}): super(onCall, ast: ast, env: env, fParams: fParams);
}

class SumBinaryOperator extends VarargsFunction  {
  SumBinaryOperator(OnCall onCall): super(onCall);
}

class MinusBinaryOperator extends VarargsFunction {
  MinusBinaryOperator(OnCall onCall): super(onCall);
}

class MultiplyBinaryOperator extends VarargsFunction {
  MultiplyBinaryOperator(OnCall onCall): super(onCall);
}

class DivideBinaryOperator extends VarargsFunction {
  DivideBinaryOperator(OnCall onCall): super(onCall);
}

class LessThanBinaryOperator extends VarargsFunction {
  LessThanBinaryOperator(OnCall onCall): super(onCall);
}

class LessThanEqualBinaryOperator extends VarargsFunction {
  LessThanEqualBinaryOperator(OnCall onCall): super(onCall);
}

class GreaterThanBinaryOperator extends VarargsFunction {
  GreaterThanBinaryOperator(OnCall onCall): super(onCall);
}

class GreaterThanEqualBinaryOperator extends VarargsFunction {
  GreaterThanEqualBinaryOperator(OnCall onCall): super(onCall);
}

class ToList extends VarargsFunction {
  ToList(OnCall onCall): super(onCall);
}

class IsList extends VarargsFunction {
  IsList(OnCall onCall): super(onCall);
}

class IsEmpty extends VarargsFunction {
  IsEmpty(OnCall onCall): super(onCall);
}

class IsEqual extends VarargsFunction {
  IsEqual(OnCall onCall): super(onCall);
}

class Count extends VarargsFunction {
  Count(OnCall onCall): super(onCall);
}

class PrStr extends VarargsFunction {
  PrStr(OnCall onCall): super(onCall);
}

class Str extends VarargsFunction {
  Str(OnCall onCall): super(onCall);
}

class Prn extends VarargsFunction {
  Prn(OnCall onCall): super(onCall);
}

class PrintLn extends VarargsFunction {
  PrintLn(OnCall onCall): super(onCall);
}

class ReadString extends VarargsFunction {
  ReadString(OnCall onCall): super(onCall);
}

class Slurp extends VarargsFunction {
  Slurp(OnCall onCall): super(onCall);
}

class Eval extends VarargsFunction  {
  Eval(OnCall onCall): super(onCall);
}

class Cons extends VarargsFunction {
  Cons(OnCall onCall): super(onCall);
}

class Concat extends VarargsFunction {
  Concat(OnCall onCall): super(onCall);
}

class Nth extends VarargsFunction {
  Nth(OnCall onCall): super(onCall);
}

class First extends VarargsFunction {
  First(OnCall onCall): super(onCall);
}

class Rest extends VarargsFunction {
  Rest(OnCall onCall): super(onCall);
}

class MalThrow extends VarargsFunction {
  MalThrow(OnCall onCall): super(onCall);
}

class IsNil extends VarargsFunction {
  IsNil(OnCall onCall): super(onCall);
}

class IsTrue extends VarargsFunction {
  IsTrue(OnCall onCall): super(onCall);
}

class IsFalse extends VarargsFunction {
  IsFalse(OnCall onCall): super(onCall);
}
class MalSymbolCore extends VarargsFunction {
  MalSymbolCore(OnCall onCall): super(onCall);
}

class IsMalSymbolCore extends VarargsFunction {
  IsMalSymbolCore(OnCall onCall): super(onCall);
}

class Keyword extends VarargsFunction {
  Keyword(OnCall onCall): super(onCall);
}
class IsKeyword extends VarargsFunction {
  IsKeyword(OnCall onCall): super(onCall);
}

//class MalListCore extends VarargsFunction {
//  MalListCore(OnCall onCall): super(onCall);
//}
//
//class IsMalListCore extends VarargsFunction {
//  IsMalListCore(OnCall onCall): super(onCall);
//}

class MalVectorCore extends VarargsFunction {
  MalVectorCore(OnCall onCall): super(onCall);
}

class IsMalVectorCore extends VarargsFunction {
  IsMalVectorCore(OnCall onCall): super(onCall);
}

class HashMapCore extends VarargsFunction {
  HashMapCore(OnCall onCall): super(onCall);
}

class IsMapCore extends VarargsFunction {
  IsMapCore(OnCall onCall): super(onCall);
}

class Assoc extends VarargsFunction {
  Assoc(OnCall onCall): super(onCall);
}

class Dissoc extends VarargsFunction {
  Dissoc(OnCall onCall): super(onCall);
}

class GetCore extends VarargsFunction {
  GetCore(OnCall onCall): super(onCall);
}

class IsContains extends VarargsFunction {
  IsContains(OnCall onCall): super(onCall);
}

class KeysCore extends VarargsFunction {
  KeysCore(OnCall onCall): super(onCall);
}

class ValsCore extends VarargsFunction {
  ValsCore(OnCall onCall): super(onCall);
}

class IsSequential extends VarargsFunction {
  IsSequential(OnCall onCall): super(onCall);
}

class ApplyCore extends VarargsFunction {
  ApplyCore(OnCall onCall): super(onCall);
}

class MapCore extends VarargsFunction {
  MapCore(OnCall onCall): super(onCall);
}

class ReadLine extends VarargsFunction {
  ReadLine(OnCall onCall): super(onCall);
}

class TimeMs extends VarargsFunction {
  TimeMs(OnCall onCall): super(onCall);
}

class Conj extends VarargsFunction {
  Conj(OnCall onCall): super(onCall);
}

class Meta extends VarargsFunction {
  Meta(OnCall onCall): super(onCall);
}

class WithMeta extends VarargsFunction {
  WithMeta(OnCall onCall): super(onCall);
}

class Atom extends VarargsFunction {
  Atom(OnCall onCall): super(onCall);
}

class IsAtom extends VarargsFunction {
  IsAtom(OnCall onCall): super(onCall);
}

class Deref extends VarargsFunction {
  Deref(OnCall onCall): super(onCall);
}

class ResetBang extends VarargsFunction {
  ResetBang(OnCall onCall): super(onCall);
}

class SwapBang extends VarargsFunction {
  SwapBang(OnCall onCall): super(onCall);
}

class MalThrowException extends StateError {
  MalType malType;
  MalThrowException(this.malType, message) : super(message);
}

class MalAtom extends MalType {
  @override
  MalType meta = new MalNil();

  MalType malType;
  MalAtom(this.malType);

  copy() {
    return new MalAtom(this.malType);
  }

  @override
  String toString([bool print_readable = false]) => "(atom ${malType.toString(print_readable)})";
}