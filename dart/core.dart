library mal.core;

import "types.dart";
import "printer.dart" as printer;
import "reader.dart" as reader;
import "dart:io";

SumBinaryOperator sumBinaryOperator = new SumBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number + arguments[1].number));
MinusBinaryOperator minusBinaryOperator = new MinusBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number - arguments[1].number));
MultiplyBinaryOperator multiplyBinaryOperator = new MultiplyBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number * arguments[1].number));
DivideBinaryOperator divideBinaryOperator = new DivideBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number ~/ arguments[1].number));

// `<`, `<=`, `>`, and `>=`: treat the first two parameters as
// numbers and do the corresponding numeric comparison, returning
// either true or false.
LessThanBinaryOperator lessThanBinaryOperator = new LessThanBinaryOperator(
        (arguments) => arguments[0].number < arguments[1].number ? MAL_TRUE : MAL_FALSE);
LessThanEqualBinaryOperator lessThanEqualBinaryOperator = new LessThanEqualBinaryOperator(
        (arguments) => arguments[0].number <= arguments[1].number ? MAL_TRUE : MAL_FALSE);
GreaterThanBinaryOperator greaterThanBinaryOperator = new GreaterThanBinaryOperator(
        (arguments) => arguments[0].number > arguments[1].number ? MAL_TRUE : MAL_FALSE);
GreaterThanEqualBinaryOperator greaterThanEqualBinaryOperator = new GreaterThanEqualBinaryOperator(
        (arguments) => arguments[0].number >= arguments[1].number ? MAL_TRUE : MAL_FALSE);

// `list`: take the parameters and return them as a list.
ToList toList = new ToList(
        (arguments) => new MalList.fromList(arguments.toList()));

// `list?`: return true if the first parameter is a list, false
// otherwise.
IsList isList = new IsList(
        (arguments) =>
        // TODO(adam): fix, we should not allow vector to assert true here.
        (arguments[0] is MalList && !(arguments[0] is MalVector)) ? MAL_TRUE : MAL_FALSE);

// `empty?`: treat the first parameter as a list and return true if
// the list is empty and false if it contains any elements.
IsEmpty isEmpty = new IsEmpty(
        (arguments) => (arguments[0] as MalList).malTypes.isEmpty? MAL_TRUE : MAL_FALSE);

// `=`: compare the first two parameters and return true if they are
// the same type and contain the same value. In the case of equal
// length lists, each element of the list should be compared for
// equality and if they are the same return true, otherwise false.
// TODO(adam): break up into smaller functions.
IsEqual isEqual = new IsEqual(
        (arguments) {
          var arg0 = arguments[0];
          var arg1 = arguments[1];

          if (/*(arg0 is MalVector && arg1 is MalVector) ||*/ (arg0 is MalList && arg1 is MalList)) {
            // vector or list
            if (arg0.malTypes.length != arg1.malTypes.length) {
              return MAL_FALSE;
            }

            for (int i = 0; i < arg0.malTypes.length; i++) {
              var result = Function.apply(isEqual, [arg0.malTypes[i], arg1.malTypes[i]]);
              if (result == MAL_FALSE) {
                return MAL_FALSE;
              }
            }

            return MAL_TRUE;
          } else if (arg0 is MalNumber && arg1 is MalNumber) {
            // number
            if (arg0.number == arg1.number) {
              return MAL_TRUE;
            } else {
              return MAL_FALSE;
            }
          } else if (arg0 is MalString && arg1 is MalString) {
            if (arg0.string == arg1.string) {
              return MAL_TRUE;
            } else {
              return MAL_FALSE;
            }
          } else if (arg0 is MalHashMap && arg1 is MalHashMap) {
            // hash map
            if (arg0.malHashMap.length != arg1.malHashMap.length) {
              return MAL_FALSE;
            }

            var keys = arg0.malHashMap.keys;
            for (int i = 0; i < arg0.malHashMap.length; i++) {
              var key = keys.elementAt(i);
              var value0 = arg0.malHashMap[key];
              var value1 = arg1.malHashMap[key];

              var result = Function.apply(isEqual, [value0, value1]);
              if (result == MAL_FALSE) {
                return MAL_FALSE;
              }
            }

            return MAL_TRUE;
          } else if (arg0 is MalSymbol && arg1 is MalSymbol) {
            // both are symbol
            if (arg0.symbol == arg1.symbol) {
              return MAL_TRUE;
            } else {
              return MAL_FALSE;
            }
          } else if (arg0 is MalKeyword && arg1 is MalKeyword)  {
            if (arg0.keyword == arg1.keyword) {
              return MAL_TRUE;
            } else {
              return MAL_FALSE;
            }
          } else if (arg0 is MalNil && arg1 is MalNil) {
            // both are nil
            return MAL_TRUE;
          } else if (arg0 is MalTrue && arg1 is MalTrue) {
            // both are true
            return MAL_TRUE;
          } else if (arg0 is MalFalse && arg1 is MalFalse) {
            // both are false
            return MAL_TRUE;
          } else {
            // unknown so its false.
            return MAL_FALSE;
          }
        });

// `count`: treat the first parameter as a list and return the number
// of elements that it contains.
Count count = new Count(
        (arguments) {

          // If argument is nil then return 0.
          if ((arguments[0] is MalNil)) {
            return new MalNumber(0);
          }

          return new MalNumber((arguments[0] as MalList).malTypes.length);
        });

// TODO(adam): revisit all the printing related stuff..

// `pr-str`: calls `pr_str` on each argument with `print_readably`
// set to true, joins the results with " " and returns the new
// string.
PrStr prStr = new PrStr(
        (arguments) {
          return new MalString(arguments.map((m) => m.toString(true)).join(" "));
        });

// `str`: calls `pr_str` on each argument with `print_readably` set
// to false, concatenates the results together ("" separator), and
// returns the new string.
Str str = new Str(
        (arguments) => new MalString(arguments.map((m) => m.toString(false)).join("")));

// `prn`:  calls `pr_str` on each argument with `print_readably` set
// to true, joins the results with " ", prints the string to the
// screen and then returns `nil`.
Prn prn = new Prn((arguments) {
  print(printer.pr_str(new MalString(arguments.map((m) => m.toString(true)).join(" "))));
  return MAL_NIL;
});

// `println`:  calls `pr_str` on each argument with `print_readably` set
// to false, joins the results with " ", prints the string to the
// screen and then returns `nil`.
PrintLn printLn = new PrintLn((arguments) {
  arguments.map((m) => printer.pr_str(m.toString()));
  return MAL_NIL;
});

ReadString readString = new ReadString((arguments) {
  String str = arguments[0].string;
  return reader.read_str(str);
});

Slurp slurp = new Slurp((arguments) {
  String str = arguments[0].string;
  File file = new File(str);
  String contents = file.readAsStringSync();
  return new MalString(contents);
});

Cons cons = new Cons((arguments) {
  MalList malList = new MalList();
  malList.malTypes.add(arguments[0]);
  malList.malTypes.addAll((arguments[1] as MalList).malTypes);
  return malList;
});

Concat concat = new Concat((arguments) {

  MalList malList = new MalList();

  if (arguments.length == 0) {
    return malList;
  }

  malList.malTypes.addAll( (arguments[0] as MalList).malTypes );

  for (int i = 1; i < arguments.length; i++) {
    malList.malTypes.addAll( (arguments[i] as MalList).malTypes );
  }

  return malList;
});

Nth nth = new Nth((arguments) {
  num index = (arguments[1] as MalNumber).number;
  MalList list = (arguments[0] as MalList);

  if (index < list.malTypes.length) {
    return list.nth(index);
  } else {
    throw new StateError("nth: index out of range");
  }
});

First first = new First((arguments) {
  MalList list = (arguments[0] as MalList);
  return list.malTypes.isNotEmpty ? list.nth(0) : MAL_NIL;
});

Rest rest = new Rest((arguments) {
  MalList list = (arguments[0] as MalList);
  return list.rest();
});

MalThrow malThrow = new MalThrow((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  throw new MalThrowException(argument0, argument0.toString());
});

IsNil isNil = new IsNil((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 == MAL_NIL ? MAL_TRUE : MAL_FALSE;
});

IsTrue isTrue = new IsTrue((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 == MAL_TRUE ? MAL_TRUE : MAL_FALSE;
});

IsFalse isFalse = new IsFalse((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 == MAL_FALSE ? MAL_TRUE : MAL_FALSE;
});

MalSymbolCore malSymbolCore = new MalSymbolCore((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return new MalSymbol(argument0.toString());
});

IsMalSymbolCore isMalSymbolCore = new IsMalSymbolCore((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 is MalSymbol ? MAL_TRUE : MAL_FALSE;
});

Keyword keyword = new Keyword((arguments) {
  MalType argument0 = (arguments[0] as MalType);

  if ((argument0 is MalString &&  (argument0.string[0] == MalKeyword.K)) || argument0 is MalKeyword) {
    return new MalKeyword(argument0.string);
  } else {
    // TODO(adam): we could just do a toString and be done with it.
    return new MalKeyword(MalKeyword.K + (argument0 as MalString).string);
  }
});

IsKeyword isKeyword = new IsKeyword((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  if ((argument0 is MalString &&  (argument0.string[0] == MalKeyword.K)) || argument0 is MalKeyword) {
    return MAL_TRUE;
  } else {
    return MAL_FALSE;
  }
});

//MalListCore malListCore = new MalListCore((arguments) {
//  throw new UnimplementedError();
//});
//
//IsMalListCore isMalListCore = new IsMalListCore((arguments) {
//  throw new UnimplementedError();
//});

MalVectorCore malVectorCore = new MalVectorCore((arguments) {
  MalVector malVector = new MalVector();
  malVector.malTypes.addAll(arguments);
  return malVector;
});

IsMalVectorCore isMalVectorCore = new IsMalVectorCore((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 is MalVector ? MAL_TRUE : MAL_FALSE;
});

HashMapCore hashMapCore = new HashMapCore((arguments) {
  // MalList argument0 = (arguments[0] as MalList);
  return new MalHashMap.fromMalList(new MalList.fromList(arguments));
});

IsMapCore isMapCore = new IsMapCore((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 is MalHashMap ? MAL_TRUE : MAL_FALSE;
});

Assoc assoc = new Assoc((arguments) {
  MalHashMap argument0 = (arguments[0] as MalHashMap);
  Map<Object, MalType> malHashMap = argument0.malHashMap;
  MalHashMap newMalHashMap = argument0.clone();
  var subList = (arguments as List).sublist(1);
  var mappedSubList = new MalList.fromList(subList);
  return newMalHashMap.assocBang(mappedSubList);
});

Dissoc dissoc = new Dissoc((arguments) {
  MalHashMap argument0 = (arguments[0] as MalHashMap);
  Map<Object, MalType> malHashMap = argument0.malHashMap;
  MalHashMap newMalHashMap = argument0.clone();
  var subList = (arguments as List).sublist(1);
  var mappedSubList = new MalList.fromList(subList);
  return newMalHashMap.dissocBang(mappedSubList);
});

GetCore getCore = new GetCore((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  if (argument0 == MAL_NIL) {
    return MAL_NIL;
  } else {
    MalHashMap malHashMap = argument0 as MalHashMap;
    MalType key = arguments[1];

    if (malHashMap.malHashMap.containsKey(key)) {
      return malHashMap.malHashMap[key];
    } else {
      return MAL_NIL;
    }
  }
});

IsContains isContains = new IsContains((arguments) {
  Map<Object, MalType> malHashMap = (arguments[0] as MalHashMap).malHashMap;
  MalType key = arguments[1];
  return malHashMap.containsKey(key) ? MAL_TRUE : MAL_FALSE;
});

KeysCore keysCore = new KeysCore((arguments) {
  MalHashMap argument0 = (arguments[0] as MalHashMap);
  Map<Object, MalType> malHashMap = argument0.malHashMap;
  var keys = malHashMap.keys.toList();
  MalList malKeysList = new MalList();
  malKeysList.conjBANG(keys);
  return malKeysList;
});

ValsCore valsCore = new ValsCore((arguments) {
  MalHashMap argument0 = (arguments[0] as MalHashMap);
  Map<Object, MalType> malHashMap = argument0.malHashMap;
  List<MalType> values = malHashMap.values.toList();
  MalList malValuesList = new MalList.fromList(values);
  return malValuesList;
});

IsSequential isSequential = new IsSequential((arguments) {
  MalType argument0 = (arguments[0] as MalType);
  return argument0 is MalList ? MAL_TRUE : MAL_FALSE;
});

ApplyCore applyCore = new ApplyCore((arguments) {
  VarargsFunction function = (arguments[0] as VarargsFunction);
  var argumentsAsList = (arguments as List);
  var argumentsLength = argumentsAsList.length;
  List args = argumentsAsList.getRange(1, argumentsLength - 1).toList();
  var lastArgumentAsMalList = argumentsAsList[argumentsLength - 1] as MalList;
  args.addAll(lastArgumentAsMalList.malTypes);
  return Function.apply(function, args);
});

MapCore mapCore = new MapCore((arguments) {
  VarargsFunction function = (arguments[0] as VarargsFunction);
  List srcList = (arguments[1] as MalList).malTypes;
  List newList = new List<MalType>();
  for (int i = 0; i < srcList.length; i++) {
    var val = Function.apply(function, [srcList[i]]);
    newList.add(val);
  }

  return new MalList.fromList(newList);
});

ReadLine readLine = new ReadLine((arguments) {
  var line = (arguments[0] as MalString).string;
  if (line == null) {
    return MAL_NIL;
  } else {
    return reader.read_str(line);
  }
});

TimeMs timeMs = new TimeMs((arguments) {
  DateTime now = new DateTime.now();
  return now.millisecondsSinceEpoch;
});

Conj conj = new Conj((arguments) {
  MalList srcList = (arguments[0] as MalList);
  List<MalType> destList = new List<MalType>();
  if (arguments[0] is MalVector) {
    destList.addAll(srcList.malTypes);

    for (int i = 1; i < (arguments as List).length; i++) {
      destList.add(arguments[i]);
    }

    var malVector = new MalVector();
    malVector.malTypes.addAll(destList);
    return malVector;
  } else {
    destList.addAll(srcList.malTypes);

    for (int i = 1; i < (arguments as List).length; i++) {
      destList.insert(0, arguments[i]);
    }

    var malList = new MalList.fromList(destList);
    return malList;
  }
});

Meta meta = new Meta((arguments) {
  MalType malType = arguments[0] as MalType;
  return malType.meta;
});

WithMeta withMeta = new WithMeta((arguments) {
  MalType malType = arguments[0] as MalType;
  MalType malTypeCopy = malType.copy();
  MalType meta = arguments[1] as MalType;
  malTypeCopy.meta = meta;
  return malTypeCopy;
});

Atom atom = new Atom((arguments) {
  MalType malType = arguments[0] as MalType;
  return new MalAtom(malType);
});

IsAtom isAtom = new IsAtom((arguments) {
  return arguments[0] is MalAtom ? MAL_TRUE : MAL_FALSE;
});

Deref deref = new Deref((arguments) {
  MalAtom malAtom = arguments[0] as MalAtom;
  return malAtom.malType;
});

ResetBang resetBang = new ResetBang((arguments) {
  MalAtom malAtom = arguments[0] as MalAtom;
  MalType malType = arguments[1] as MalType;
  malAtom.malType = malType;
  return malAtom.malType;
});

SwapBang swapBang = new SwapBang((arguments) {
  MalAtom atom = arguments[0] as MalAtom;
  VarargsFunction f = arguments[1] as VarargsFunction;
  MalList malList = new MalList();
  malList.malTypes.add(atom.malType);
  malList.malTypes.addAll((arguments as List).sublist(2));
  atom.malType = Function.apply(f, malList.malTypes);
  return atom.malType;
});

// core modules namespace
Map<String, Object> ns = {
    '=':            isEqual,
    'throw':        malThrow,

    'nil?':         isNil,
    'true?':        isTrue,
    'false?':       isFalse,
    'symbol':       malSymbolCore,
    'symbol?':      isMalSymbolCore,
    'keyword':      keyword,
    'keyword?':     isKeyword,

    'pr-str':       prStr,
    'str':          str,
    'prn':          prn,
    'println':      printLn,

    'read-string':  readString,
    'readline':     readLine,
    'slurp':        slurp,

    '<':            lessThanBinaryOperator,
    '<=':           lessThanEqualBinaryOperator,
    '>':            greaterThanBinaryOperator,
    '>=':           greaterThanEqualBinaryOperator,
    '+':            sumBinaryOperator,
    '-':            minusBinaryOperator,
    '*':            multiplyBinaryOperator,
    '/':            divideBinaryOperator,
    'time-ms':      timeMs,

    'list':         toList,
    'list?':        isList,
    'vector':       malVectorCore,
    'vector?':      isMalVectorCore,
    'hash-map':     hashMapCore,
    'map?':         isMapCore,
    'assoc':        assoc,
    'dissoc':       dissoc,
    'get':          getCore,
    'contains?':    isContains,
    'keys':         keysCore,
    'vals':         valsCore,

    'sequential?':  isSequential,
    'cons':         cons,
    'concat':       concat,

    'nth':          nth,
    'first':        first,
    'rest':         rest,

    'empty?':       isEmpty,
    'count':        count,
    'apply':        applyCore,
    'map':          mapCore,
    'conj':         conj,

    'meta':         meta,
    'with-meta':    withMeta,
    'atom':         atom,
    'atom?':        isAtom,
    'deref':        deref,
    'reset!':       resetBang,
    'swap!':        swapBang
};
