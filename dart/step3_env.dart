#!/usr/bin/env dart

library mal.step3_env;

import "dart:io";
import "types.dart";
import "reader.dart" as reader;
import "printer.dart" as printer;
import "env.dart";

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

Env repl_env = new Env()
  ..set(new MalSymbol('+'), sumBinaryOperator)
  ..set(new MalSymbol('-'), minusBinaryOperator)
  ..set(new MalSymbol('*'), multiplyBinaryOperator)
  ..set(new MalSymbol('/'), divideBinaryOperator);

MalType eval_ast(MalType ast, Env env) {

  if (ast is MalSymbol) {
    return env.get(ast);
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

MalType EVAL(MalType ast, Env env) {

  if (!(ast is MalList) || ast is MalVector) {
    return eval_ast(ast, env);
  }

  if ((ast as MalList).malTypes.length == 0) {
    return ast;
  }

  if (!((ast as MalList).malTypes[0] is MalSymbol)) {
    throw new StateError("attempt to apply on non-symbol '${(ast as MalList).malTypes[0]}'");
  }

  var malList = ast as MalList;
  var argument0 = malList.malTypes[0] as MalSymbol;

  switch (argument0.symbol) {
    case 'def!':
      var argument1 = malList.malTypes[1] as MalSymbol;
      var argument2 = malList.malTypes[2];

      var result = EVAL(argument2, env);
      env.set(argument1, result);
      return result;
    case 'let*':
      var argument1 = malList.malTypes[1] as MalList;
      var argument2 = malList.malTypes[2];
      Env let_env = new Env(outer: env);

      for (int i = 0; i < argument1.malTypes.length; i += 2) {
        var key = argument1.malTypes[i] as MalSymbol;
        var value = argument1.malTypes[i+1];
        let_env.set(key, EVAL(value, let_env));
      }

      return EVAL(argument2, let_env);
    default:
      MalList astList = eval_ast(ast, env);
      var function = astList.malTypes[0] as VarargsFunction;
      var args = astList.malTypes.getRange(1, astList.malTypes.length).toList();
      return Function.apply(function, args);
  }
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
