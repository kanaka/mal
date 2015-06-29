#!/usr/bin/env dart

library mal.step5_tco;

import "dart:io";
import "types.dart";
import "reader.dart" as reader;
import "printer.dart" as printer;
import "env.dart";
import "core.dart" as core;

MalType READ(String str) {
  return reader.read_str(str);
}

Env repl_env = new Env();

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

MalType EVAL(MalType sourceAst, Env env) {
  var argument0, argument1, argument2, argument3;

  while(true) {
    if (!(sourceAst is MalList) || sourceAst is MalVector) {
      return eval_ast(sourceAst, env);
    }

    if ((sourceAst as MalList).malTypes.length == 0) {
      return sourceAst;
    }

    var ast = sourceAst as MalList;
    argument0 = (ast.malTypes[0] is MalSymbol) ? (ast.malTypes[0] as MalSymbol).symbol : "__<*fn*>__";

    switch (argument0) {
      case 'def!':
        argument1 = ast.malTypes[1] as MalSymbol;
        argument2 = ast.malTypes[2];

        var result = EVAL(argument2, env);
        env.set(argument1, result);
        return result;
      case 'let*':
        argument1 = ast.malTypes[1] as MalList;
        argument2 = ast.malTypes[2];
        Env let_env = new Env(outer: env);

        for (int i = 0; i < argument1.malTypes.length; i += 2) {
          var key = argument1.malTypes[i] as MalSymbol;
          var value = argument1.malTypes[i + 1];
          let_env.set(key, EVAL(value, let_env));
        }

        sourceAst = argument2;
        env = let_env;
        break;
      case 'do':
      // 'do: return eval_ast(rest(ast), env)[-1]
      // * `do`: Evaluate the all the elements of the list using `eval_ast`
      //   and return the final evaluated element.
        rest(MalList a) {
          return new MalList.fromList(a.malTypes.getRange(1, a.malTypes.length - 1).toList());
        }

        eval_ast(rest(sourceAst), env) as MalList;
        sourceAst = ast.malTypes.last;
        break;
      case 'if':
      // 'if: return EVAL(EVAL(ast[1], env) ? ast[2] : ast[3], env)
      // * `if`: Evaluate the first parameter (second element). If the result
      //   (condition) is anything other than `nil` or `false`, then evaluate
      //   the second parammeter (third element of the list) and return the
      //   result.  Otherwise, evaluate the third parameter (fourth element)
      //   and return the result. If condition is false and there is no third
      //   parameter, then just return `nil`.
        argument1 = ast.malTypes[1];
        argument2 = ast.malTypes[2];

        var result = EVAL(argument1, env);

        if ((result is MalBoolean && (result as MalBoolean).value)) {

          sourceAst = argument2;
          break;
        }

        if (!(result is MalNil) && (result is MalType) && !(result is MalBoolean)) {
          sourceAst = argument2;
          break;
        }

        if (ast.malTypes.length < 4) {
          return MAL_NIL;
        }

        argument3 = ast.malTypes[3];
        sourceAst = argument3;
        break;
      case 'fn*':
      // 'fn*: return (...a) -> EVAL(ast[2], new Env(env, ast[1], a))
        argument1 = ast.malTypes[1] as MalList;
        argument2 = ast.malTypes[2];
        Env cur_env = env;
        return new MalFunction((List<MalType> arguments) =>
          EVAL(argument2, new Env(outer: cur_env, binds: argument1, exprs: new MalList.fromList(arguments))), ast: argument2, env: env, fParams: argument1);
      default:
        MalList astList = eval_ast(sourceAst, env);
        var function = astList.malTypes[0] as VarargsFunction;
        var args = astList.malTypes.getRange(1, astList.malTypes.length).toList();

        var functionAst = function.ast;
        if (functionAst != null) {
          sourceAst = functionAst;
          env = new Env(outer: function.env, binds: function.fParams, exprs: new MalList.fromList(args));
        } else {
          var result = Function.apply(function, args);
          return result;
        }
        break;
    }
  }
}

void PRINT(MalType exp) {
  stdout.writeln(printer.pr_str(exp));
}

void rep(String str) {
  PRINT(EVAL(READ(str), repl_env));
}

void init() {
  // Setup environment
  core.ns.forEach((key, value) => repl_env.set(new MalSymbol(key), value));

  // Define global function in language
  rep("(def! not (fn* (a)(if a false true)))");
}

void main(List<String> args) {
  String line;

  init();

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
      stdout.writeln("Error: ${ex.toString()}");
      break;
    } on StateError catch (ex) {
      stdout.writeln("Error: ${ex.message}");
      break;
    } on StackOverflowError catch (ex) {
      stdout.writeln("Error: ${ex.toString()}");
      PRINT(MAL_NIL);
    }
  }
}
