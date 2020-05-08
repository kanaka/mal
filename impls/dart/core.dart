import 'dart:io';

import 'printer.dart';
import 'reader.dart' as reader;
import 'types.dart';

Map<MalSymbol, MalBuiltin> ns = <MalSymbol, MalBuiltin>{
  new MalSymbol('+'): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value + b.value);
  }),
  new MalSymbol('-'): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value - b.value);
  }),
  new MalSymbol('*'): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value * b.value);
  }),
  new MalSymbol('/'): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value ~/ b.value);
  }),
  new MalSymbol('list'):
      new MalBuiltin((List<MalType> args) => new MalList(args.toList())),
  new MalSymbol('list?'): new MalBuiltin(
      (List<MalType> args) => new MalBool(args.single is MalList)),
  new MalSymbol('empty?'): new MalBuiltin((List<MalType> args) {
    var a = args.single as MalIterable;
    return new MalBool(a.elements.isEmpty);
  }),
  new MalSymbol('count'): new MalBuiltin((List<MalType> args) {
    var a = args.first as MalIterable;
    return new MalInt(a.elements.length);
  }),
  new MalSymbol('='): new MalBuiltin((List<MalType> args) {
    var a = args[0];
    var b = args[1];
    return new MalBool(a == b);
  }),
  new MalSymbol('<'): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalBool(a.value < b.value);
  }),
  new MalSymbol('<='): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalBool(a.value <= b.value);
  }),
  new MalSymbol('>'): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalBool(a.value > b.value);
  }),
  new MalSymbol('>='): new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalBool(a.value >= b.value);
  }),
  new MalSymbol('pr-str'): new MalBuiltin((List<MalType> args) {
    return new MalString(
        args.map((a) => pr_str(a, print_readably: true)).join(' '));
  }),
  new MalSymbol('str'): new MalBuiltin((List<MalType> args) {
    return new MalString(
        args.map((a) => pr_str(a, print_readably: false)).join());
  }),
  new MalSymbol('prn'): new MalBuiltin((List<MalType> args) {
    print(args.map((a) => pr_str(a, print_readably: true)).join(' '));
    return new MalNil();
  }),
  new MalSymbol('println'): new MalBuiltin((List<MalType> args) {
    print(args.map((a) => pr_str(a, print_readably: false)).join(' '));
    return new MalNil();
  }),
  new MalSymbol('read-string'): new MalBuiltin((List<MalType> args) {
    var code = args.single as MalString;
    return reader.read_str(code.value);
  }),
  new MalSymbol('slurp'): new MalBuiltin((List<MalType> args) {
    var fileName = args.single as MalString;
    var file = new File(fileName.value);
    return new MalString(file.readAsStringSync());
  }),
  new MalSymbol('atom'): new MalBuiltin((List<MalType> args) {
    var value = args.single;
    return new MalAtom(value);
  }),
  new MalSymbol('atom?'): new MalBuiltin((List<MalType> args) {
    var value = args.single;
    return new MalBool(value is MalAtom);
  }),
  new MalSymbol('deref'): new MalBuiltin((List<MalType> args) {
    var atom = args.single as MalAtom;
    return atom.value;
  }),
  new MalSymbol('reset!'): new MalBuiltin((List<MalType> args) {
    var atom = args[0] as MalAtom;
    var newValue = args[1];
    atom.value = newValue;
    return newValue;
  }),
  new MalSymbol('swap!'): new MalBuiltin((List<MalType> args) {
    var atom = args[0] as MalAtom;
    var func = args[1] as MalCallable;
    var fnArgs = [atom.value]..addAll(args.sublist(2));
    var result = func.call(fnArgs);
    atom.value = result;
    return result;
  }),
  new MalSymbol('cons'): new MalBuiltin((List<MalType> args) {
    var x = args[0];
    var xs = args[1] as MalIterable;
    return new MalList([x]..addAll(xs));
  }),
  new MalSymbol('concat'): new MalBuiltin((List<MalType> args) {
    var results = <MalType>[];
    for (MalIterable element in args) {
      results.addAll(element);
    }
    return new MalList(results);
  }),
  new MalSymbol('nth'): new MalBuiltin((List<MalType> args) {
    var indexable = args[0] as MalIterable;
    var index = args[1] as MalInt;
    try {
      return indexable[index.value];
    } on RangeError catch (e) {
      throw new MalException(new MalString(e.toString()));
    }
  }),
  new MalSymbol('first'): new MalBuiltin((List<MalType> args) {
    var list = args.first as MalIterable;
    if (list.isEmpty) return new MalNil();
    return list.first;
  }),
  new MalSymbol('rest'): new MalBuiltin((List<MalType> args) {
    var list = args.first as MalIterable;
    if (list.isEmpty) return new MalList(<MalType>[]);
    return new MalList(list.sublist(1));
  }),
  new MalSymbol('throw'): new MalBuiltin((List<MalType> args) {
    throw new MalException(args.first);
  }),
  new MalSymbol('nil?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalNil);
  }),
  new MalSymbol('true?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalBool && (args.first as MalBool).value);
  }),
  new MalSymbol('false?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalBool && !(args.first as MalBool).value);
  }),
  new MalSymbol('symbol'): new MalBuiltin((List<MalType> args) {
    return new MalSymbol((args.first as MalString).value);
  }),
  new MalSymbol('symbol?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalSymbol);
  }),
  new MalSymbol('keyword'): new MalBuiltin((List<MalType> args) {
    if (args.first is MalKeyword) return args.first;
    return new MalKeyword((args.first as MalString).value);
  }),
  new MalSymbol('keyword?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalKeyword);
  }),
  new MalSymbol('number?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalInt);
  }),
  new MalSymbol('fn?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalCallable && !(args.first.isMacro));
  }),
  new MalSymbol('macro?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalCallable && args.first.isMacro);
  }),
  new MalSymbol('vector'): new MalBuiltin((List<MalType> args) {
    return new MalVector(args);
  }),
  new MalSymbol('vector?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalVector);
  }),
  new MalSymbol('hash-map'): new MalBuiltin((List<MalType> args) {
    return new MalHashMap.fromSequence(args);
  }),
  new MalSymbol('map?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalHashMap);
  }),
  new MalSymbol('assoc'): new MalBuiltin((List<MalType> args) {
    var map = args.first as MalHashMap;
    var assoc = new MalHashMap.fromSequence(args.skip(1).toList());
    var newMap = new Map<MalType, MalType>.from(map.value);
    newMap.addAll(assoc.value);
    return new MalHashMap(newMap);
  }),
  new MalSymbol('dissoc'): new MalBuiltin((List<MalType> args) {
    var map = args.first as MalHashMap;
    var newMap = new Map<MalType, MalType>.from(map.value);
    for (var key in args.skip(1)) {
      newMap.remove(key);
    }
    return new MalHashMap(newMap);
  }),
  new MalSymbol('get'): new MalBuiltin((List<MalType> args) {
    if (args[0] is MalNil) return new MalNil();
    var map = args[0] as MalHashMap;
    var key = args[1];
    return map.value[key] ?? new MalNil();
  }),
  new MalSymbol('contains?'): new MalBuiltin((List<MalType> args) {
    var map = args[0] as MalHashMap;
    var key = args[1];
    return new MalBool(map.value.containsKey(key));
  }),
  new MalSymbol('keys'): new MalBuiltin((List<MalType> args) {
    return new MalList((args.first as MalHashMap).value.keys.toList());
  }),
  new MalSymbol('vals'): new MalBuiltin((List<MalType> args) {
    return new MalList((args.first as MalHashMap).value.values.toList());
  }),
  new MalSymbol('sequential?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalList || args.first is MalVector);
  }),
  new MalSymbol('readline'): new MalBuiltin((List<MalType> args) {
    var message = args.first as MalString;
    stdout.write(message.value);
    var input = stdin.readLineSync();
    if (input == null) return new MalNil();
    return new MalString(input);
  }),
  new MalSymbol('time-ms'): new MalBuiltin((List<MalType> args) {
    assert(args.isEmpty);
    return new MalInt(new DateTime.now().millisecondsSinceEpoch);
  }),
  new MalSymbol('conj'): new MalBuiltin((List<MalType> args) {
    var collection = args.first;
    var elements = args.sublist(1);
    if (collection is MalList) {
      return new MalList(
          elements.reversed.toList()..addAll(collection.elements));
    }
    if (collection is MalVector) {
      return new MalVector(collection.elements.toList()..addAll(elements));
    }
    throw new MalException(new MalString('"conj" takes a list or vector'));
  }),
  new MalSymbol('string?'): new MalBuiltin((List<MalType> args) {
    return new MalBool(args.first is MalString);
  }),
  new MalSymbol('seq'): new MalBuiltin((List<MalType> args) {
    var arg = args.first;
    if (arg is MalIterable && arg.isEmpty) return new MalNil();
    if (arg is MalString && arg.value.isEmpty) return new MalNil();

    if (arg is MalNil || arg is MalList) return arg;
    if (arg is MalVector) return new MalList(arg.elements.toList());
    if (arg is MalString) {
      var chars = <MalString>[];
      for (var i = 0; i < arg.value.length; i++) {
        chars.add(new MalString(arg.value[i]));
      }
      return new MalList(chars);
    }
    throw new MalException(new MalString('bad argument to "seq"'));
  }),
  new MalSymbol('map'): new MalBuiltin((List<MalType> args) {
    var fn = args[0] as MalCallable;
    var list = args[1] as MalIterable;
    var newList = <MalType>[];
    for (var element in list) {
      newList.add(fn.call([element]));
    }
    return new MalList(newList);
  }),
  new MalSymbol('apply'): new MalBuiltin((List<MalType> args) {
    var func = args.first as MalCallable;
    var argList = args.last as MalIterable;
    var newArgs = args.sublist(1, args.length - 1);
    newArgs.addAll(argList);
    return func.call(newArgs);
  }),
  new MalSymbol('meta'): new MalBuiltin((List<MalType> args) {
    var arg = args.first;
    return arg.meta ?? new MalNil();
  }),
  new MalSymbol('with-meta'): new MalBuiltin((List<MalType> args) {
    var evaled = args.first;
    var evaledWithMeta = evaled.clone();
    evaledWithMeta.meta = args[1];
    return evaledWithMeta;
  }),
};
