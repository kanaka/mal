#!/usr/bin/env dart

library mal.step2_eval;

import "dart:io";
import "types.dart";
import "reader.dart" as reader;
import "printer.dart" as printer;

MalType READ(String str) {
  return reader.read_str(str);
}

SumBinaryOperator sumBinaryOperator = new SumBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number + arguments[1].number));
MinusBinaryOperator minusBinaryOperator = new MinusBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number - arguments[1].number));
MultiplyBinaryOperator multiplyBinaryOperator = new MultiplyBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number * arguments[1].number));
DivideBinaryOperator divideBinaryOperator = new DivideBinaryOperator(
        (arguments) => new MalNumber(arguments[0].number ~/ arguments[1].number));

Map<MalSymbol, MalType> repl_env = {
  new MalSymbol('+'): sumBinaryOperator,
  new MalSymbol('-'): minusBinaryOperator,
  new MalSymbol('*'): multiplyBinaryOperator,
  new MalSymbol('/'): divideBinaryOperator
};

MalType eval_ast(MalType ast, Map<MalSymbol, MalType> env) {

  if (ast is MalSymbol) {

    if (!env.containsKey(ast)) {
      throw new StateError("'${ast}' not found.");
    }

    var binaryOperator = env[ast];
    return binaryOperator;
  } else if (ast is MalList) {

    MalList newMalList = ast is MalVector ? new MalVector() : new MalList();

    ast.malTypes.forEach((MalType malType) {
      MalType evaluatedMalType = EVAL(malType, env);
      newMalList.malTypes.add(evaluatedMalType);
    });

    return newMalList;
  } else if (ast is MalHashMap) {

    MalHashMap newMalHashMap = new MalHashMap();

    ast.malHashMap.forEach((key, value) {
      MalType evaluatedMalType = EVAL(value, env);
      newMalHashMap.malHashMap[key] = evaluatedMalType;
    });

    return newMalHashMap;
  } else {
    return ast;
  }
}

MalType EVAL(MalType ast, Map<MalSymbol, MalType> env) {

  if (!(ast is MalList) || ast is MalVector) {
    return eval_ast(ast, env);
  }

  if ((ast as MalList).malTypes.length == 0) {
    return ast;
  }

  if (!((ast as MalList).malTypes[0] is MalSymbol)) {
    throw new StateError("attempt to apply on non-symbol '${(ast as MalList).malTypes[0]}'");
  }

  MalList astList = eval_ast(ast, env);

  var function = astList.malTypes[0];
  var args = astList.malTypes.getRange(1, astList.malTypes.length).toList();
  return Function.apply(function, args);
}

void PRINT(MalType exp) {
  stdout.writeln(printer.pr_str(exp));
}

void rep(String str) {
  PRINT(EVAL(READ(str), repl_env));
}

void main(List<String> args) {
  String line;

  while (true) {
    stdout.write("user> ");
    try {
      line = stdin.readLineSync();
      if (line == null) {
        // Control signal or EOF then break from REPL.
        break;
      }

      rep(line);
    } on Exception catch (ex) {
      stdout.writeln("Error: ${ex.message}");
      break;
    } on StateError catch (ex) {
      stdout.writeln("Error: ${ex.message}");
      break;
    }
  }
}
