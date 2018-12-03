import 'dart:io';

import 'printer.dart' as printer;
import 'reader.dart' as reader;
import 'types.dart';

final Map<MalSymbol, Function> replEnv = <MalSymbol, Function>{
  new MalSymbol('+'): (MalInt a, MalInt b) => new MalInt(a.value + b.value),
  new MalSymbol('-'): (MalInt a, MalInt b) => new MalInt(a.value - b.value),
  new MalSymbol('*'): (MalInt a, MalInt b) => new MalInt(a.value * b.value),
  new MalSymbol('/'): (MalInt a, MalInt b) => new MalInt(a.value ~/ b.value),
};

MalType READ(String x) => reader.read_str(x);

class NotFoundException implements Exception {
  /// The name of the symbol that was not found.
  final String value;

  NotFoundException(this.value);
}

eval_ast(MalType ast, Map<MalSymbol, Function> env) {
  if (ast is MalSymbol) {
    var result = env[ast];
    if (result == null) {
      throw new NotFoundException(ast.value);
    }
    return result;
  } else if (ast is MalList) {
    return new MalList(ast.elements.map((x) => EVAL(x, env)).toList());
  } else if (ast is MalVector) {
    return new MalVector(ast.elements.map((x) => EVAL(x, env)).toList());
  } else if (ast is MalHashMap) {
    var newMap = new Map.from(ast.value);
    for (var key in newMap.keys) {
      newMap[key] = EVAL(newMap[key], env);
    }
    return new MalHashMap(newMap);
  } else {
    return ast;
  }
}

EVAL(MalType ast, Map<MalSymbol, Function> env) {
  if (ast is! MalList) {
    return eval_ast(ast, env);
  } else {
    if ((ast as MalList).elements.isEmpty) {
      return ast;
    } else {
      var newAst = eval_ast(ast, env) as MalList;
      Function f = newAst.elements.first;
      var args = newAst.elements.sublist(1);
      return Function.apply(f, args);
    }
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
