import 'dart:io';

import 'printer.dart';
import 'reader.dart' as reader;
import 'types.dart';

Map<MalSymbol, MalBuiltin> ns = <MalSymbol, MalBuiltin>{
  MalSymbol('+'): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalInt(a.value + b.value);
  }),
  MalSymbol('-'): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalInt(a.value - b.value);
  }),
  MalSymbol('*'): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalInt(a.value * b.value);
  }),
  MalSymbol('/'): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalInt(a.value ~/ b.value);
  }),
  MalSymbol('list'): MalBuiltin((List<MalType> args) => MalList(args.toList())),
  MalSymbol('list?'):
      MalBuiltin((List<MalType> args) => MalBool(args.single is MalList)),
  MalSymbol('empty?'): MalBuiltin((List<MalType> args) {
    var a = args.single as MalIterable;
    return MalBool(a.elements.isEmpty);
  }),
  MalSymbol('count'): MalBuiltin((List<MalType> args) {
    var a = args.first as MalIterable;
    return MalInt(a.elements.length);
  }),
  MalSymbol('='): MalBuiltin((List<MalType> args) {
    var a = args[0];
    var b = args[1];
    return MalBool(a == b);
  }),
  MalSymbol('<'): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalBool(a.value < b.value);
  }),
  MalSymbol('<='): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalBool(a.value <= b.value);
  }),
  MalSymbol('>'): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalBool(a.value > b.value);
  }),
  MalSymbol('>='): MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return MalBool(a.value >= b.value);
  }),
  MalSymbol('pr-str'): MalBuiltin((List<MalType> args) {
    return MalString(
        args.map((a) => pr_str(a, print_readably: true)).join(' '));
  }),
  MalSymbol('str'): MalBuiltin((List<MalType> args) {
    return MalString(args.map((a) => pr_str(a, print_readably: false)).join());
  }),
  MalSymbol('prn'): MalBuiltin((List<MalType> args) {
    print(args.map((a) => pr_str(a, print_readably: true)).join(' '));
    return MalNil();
  }),
  MalSymbol('println'): MalBuiltin((List<MalType> args) {
    print(args.map((a) => pr_str(a, print_readably: false)).join(' '));
    return MalNil();
  }),
  MalSymbol('read-string'): MalBuiltin((List<MalType> args) {
    var code = args.single as MalString;
    return reader.read_str(code.value);
  }),
  MalSymbol('slurp'): MalBuiltin((List<MalType> args) {
    var fileName = args.single as MalString;
    var file = File(fileName.value);
    return MalString(file.readAsStringSync());
  }),
  MalSymbol('atom'): MalBuiltin((List<MalType> args) {
    var value = args.single;
    return MalAtom(value);
  }),
  MalSymbol('atom?'): MalBuiltin((List<MalType> args) {
    var value = args.single;
    return MalBool(value is MalAtom);
  }),
  MalSymbol('deref'): MalBuiltin((List<MalType> args) {
    var atom = args.single as MalAtom;
    return atom.value;
  }),
  MalSymbol('reset!'): MalBuiltin((List<MalType> args) {
    var atom = args[0] as MalAtom;
    var newValue = args[1];
    atom.value = newValue;
    return newValue;
  }),
  MalSymbol('swap!'): MalBuiltin((List<MalType> args) {
    var atom = args[0] as MalAtom;
    var func = args[1] as MalCallable;
    var fnArgs = [atom.value]..addAll(args.sublist(2));
    var result = func.call(fnArgs);
    atom.value = result;
    return result;
  }),
  MalSymbol('cons'): MalBuiltin((List<MalType> args) {
    var x = args[0];
    var xs = args[1] as MalIterable;
    return MalList([x]..addAll(xs));
  }),
  MalSymbol('concat'): MalBuiltin((List<MalType> args) {
    var results = <MalType>[];
    for (MalIterable element in args) {
      results.addAll(element);
    }
    return MalList(results);
  }),
  MalSymbol('nth'): MalBuiltin((List<MalType> args) {
    var indexable = args[0] as MalIterable;
    var index = args[1] as MalInt;
    try {
      return indexable[index.value];
    } on RangeError catch (e) {
      throw MalException(MalString(e.toString()));
    }
  }),
  MalSymbol('first'): MalBuiltin((List<MalType> args) {
    var list = args.first as MalIterable;
    if (list.isEmpty) return MalNil();
    return list.first;
  }),
  MalSymbol('rest'): MalBuiltin((List<MalType> args) {
    var list = args.first as MalIterable;
    if (list.isEmpty) return MalList(<MalType>[]);
    return MalList(list.sublist(1));
  }),
  MalSymbol('throw'): MalBuiltin((List<MalType> args) {
    throw MalException(args.first);
  }),
  MalSymbol('nil?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalNil);
  }),
  MalSymbol('true?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalBool && (args.first as MalBool).value);
  }),
  MalSymbol('false?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalBool && !(args.first as MalBool).value);
  }),
  MalSymbol('symbol'): MalBuiltin((List<MalType> args) {
    return MalSymbol((args.first as MalString).value);
  }),
  MalSymbol('symbol?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalSymbol);
  }),
  MalSymbol('keyword'): MalBuiltin((List<MalType> args) {
    if (args.first is MalKeyword) return args.first;
    return MalKeyword((args.first as MalString).value);
  }),
  MalSymbol('keyword?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalKeyword);
  }),
  MalSymbol('number?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalInt);
  }),
  MalSymbol('fn?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalCallable && !(args.first.isMacro));
  }),
  MalSymbol('macro?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalCallable && args.first.isMacro);
  }),
  MalSymbol('vector'): MalBuiltin((List<MalType> args) {
    return MalVector(args);
  }),
  MalSymbol('vector?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalVector);
  }),
  MalSymbol('hash-map'): MalBuiltin((List<MalType> args) {
    return MalHashMap.fromSequence(args);
  }),
  MalSymbol('map?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalHashMap);
  }),
  MalSymbol('assoc'): MalBuiltin((List<MalType> args) {
    var map = args.first as MalHashMap;
    var assoc = MalHashMap.fromSequence(args.skip(1).toList());
    var newMap = Map<MalType, MalType>.from(map.value);
    newMap.addAll(assoc.value);
    return MalHashMap(newMap);
  }),
  MalSymbol('dissoc'): MalBuiltin((List<MalType> args) {
    var map = args.first as MalHashMap;
    var newMap = Map<MalType, MalType>.from(map.value);
    for (var key in args.skip(1)) {
      newMap.remove(key);
    }
    return MalHashMap(newMap);
  }),
  MalSymbol('get'): MalBuiltin((List<MalType> args) {
    if (args[0] is MalNil) return MalNil();
    var map = args[0] as MalHashMap;
    var key = args[1];
    return map.value[key] ?? MalNil();
  }),
  MalSymbol('contains?'): MalBuiltin((List<MalType> args) {
    var map = args[0] as MalHashMap;
    var key = args[1];
    return MalBool(map.value.containsKey(key));
  }),
  MalSymbol('keys'): MalBuiltin((List<MalType> args) {
    return MalList((args.first as MalHashMap).value.keys.toList());
  }),
  MalSymbol('vals'): MalBuiltin((List<MalType> args) {
    return MalList((args.first as MalHashMap).value.values.toList());
  }),
  MalSymbol('sequential?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalList || args.first is MalVector);
  }),
  MalSymbol('readline'): MalBuiltin((List<MalType> args) {
    var message = args.first as MalString;
    stdout.write(message.value);
    var input = stdin.readLineSync();
    if (input == null) return MalNil();
    return MalString(input);
  }),
  MalSymbol('time-ms'): MalBuiltin((List<MalType> args) {
    assert(args.isEmpty);
    return MalInt(DateTime.now().millisecondsSinceEpoch);
  }),
  MalSymbol('conj'): MalBuiltin((List<MalType> args) {
    var collection = args.first;
    var elements = args.sublist(1);
    if (collection is MalList) {
      return MalList(elements.reversed.toList()..addAll(collection.elements));
    }
    if (collection is MalVector) {
      return MalVector(collection.elements.toList()..addAll(elements));
    }
    throw MalException(MalString('"conj" takes a list or vector'));
  }),
  MalSymbol('string?'): MalBuiltin((List<MalType> args) {
    return MalBool(args.first is MalString);
  }),
  MalSymbol('seq'): MalBuiltin((List<MalType> args) {
    var arg = args.first;
    if (arg is MalIterable && arg.isEmpty) return MalNil();
    if (arg is MalString && arg.value.isEmpty) return MalNil();

    if (arg is MalNil || arg is MalList) return arg;
    if (arg is MalVector) return MalList(arg.elements.toList());
    if (arg is MalString) {
      var chars = <MalString>[];
      for (var i = 0; i < arg.value.length; i++) {
        chars.add(MalString(arg.value[i]));
      }
      return MalList(chars);
    }
    throw MalException(MalString('bad argument to "seq"'));
  }),
  MalSymbol('map'): MalBuiltin((List<MalType> args) {
    var fn = args[0] as MalCallable;
    var list = args[1] as MalIterable;
    var newList = <MalType>[];
    for (var element in list) {
      newList.add(fn.call([element]));
    }
    return MalList(newList);
  }),
  MalSymbol('apply'): MalBuiltin((List<MalType> args) {
    var func = args.first as MalCallable;
    var argList = args.last as MalIterable;
    var newArgs = args.sublist(1, args.length - 1);
    newArgs.addAll(argList);
    return func.call(newArgs);
  }),
  MalSymbol('meta'): MalBuiltin((List<MalType> args) {
    var arg = args.first;
    return arg.meta ?? MalNil();
  }),
  MalSymbol('with-meta'): MalBuiltin((List<MalType> args) {
    var evaled = args.first;
    var evaledWithMeta = evaled.clone();
    evaledWithMeta.meta = args[1];
    return evaledWithMeta;
  }),
};
