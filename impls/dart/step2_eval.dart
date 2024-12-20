import 'dart:io';

import 'printer.dart' as printer;
import 'reader.dart' as reader;
import 'types.dart';

final Map<String, MalType> replEnv = <String, MalType>{
  '+': new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value + b.value);
  }),
  '-': new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value - b.value);
  }),
  '*': new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value * b.value);
  }),
  '/': new MalBuiltin((List<MalType> args) {
    var a = args[0] as MalInt;
    var b = args[1] as MalInt;
    return new MalInt(a.value ~/ b.value);
  })
};

MalType READ(String x) => reader.read_str(x);

class NotFoundException implements Exception {
  /// The name of the symbol that was not found.
  final String value;

  NotFoundException(this.value);
}

MalType EVAL(MalType ast, Map<String, MalType> env) {
  // stdout.writeln("EVAL: ${printer.pr_str(ast)}");

  if (ast is MalSymbol) {
    var result = env[ast.value];
    if (result == null) {
      throw new NotFoundException(ast.value);
    }
    return result;
  } else if (ast is MalList) {
    // Exit this switch.
  } else if (ast is MalVector) {
    return new MalVector(ast.elements.map((x) => EVAL(x, env)).toList());
  } else if (ast is MalHashMap) {
    var newMap = new Map<MalType, MalType>.from(ast.value);
    for (var key in newMap.keys) {
      newMap[key] = EVAL(newMap[key], env);
    }
    return new MalHashMap(newMap);
  } else {
    return ast;
  }
    // ast is a list. todo: indent left.
    var forms = (ast as MalList).elements;
    if (forms.isEmpty) {
      return ast;
    } else {
      MalBuiltin f = EVAL(forms.first, env);
      List<MalType> args = forms.sublist(1).map((x) => EVAL(x, env)).toList();
      return f.call(args);
    }
}

String PRINT(MalType x) => printer.pr_str(x);

String rep(String x) {
  return PRINT(EVAL(READ(x), replEnv));
}

const prompt = 'user> ';
main() {
  while (true) {
    stdout.write(prompt);
    var input = stdin.readLineSync();
    if (input == null) return;
    var output;
    try {
      output = rep(input);
    } on reader.ParseException catch (e) {
      stdout.writeln("Error: '${e.message}'");
      continue;
    } on NotFoundException catch (e) {
      stdout.writeln("Error: '${e.value}' not found");
      continue;
    } on reader.NoInputException {
      continue;
    }
    stdout.writeln(output);
  }
}
