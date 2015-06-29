#!/usr/bin/env dart

library mal.step4_if_fn_do;

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

MalType EVAL(MalType ast, Env env) {

  if (!(ast is MalList) || ast is MalVector) {
    return eval_ast(ast, env);
  }

  if ((ast as MalList).malTypes.length == 0) {
    return ast;
  }

  var malList = ast as MalList;
  var argument0 = (malList.malTypes[0] is MalSymbol) ? (malList.malTypes[0] as MalSymbol).symbol : "__<*fn*>__";

  switch (argument0) {
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

    case 'do':
    // 'do: return eval_ast(rest(ast), env)[-1]
    // * `do`: Evaluate the all the elements of the list using `eval_ast`
    //   and return the final evaluated element.
    rest(MalList a) {
      return new MalList.fromList(a.malTypes.getRange(1, a.malTypes.length).toList());
    }

    var doResult = eval_ast(rest(ast), env) as MalList;
    return doResult.malTypes.last;

    case 'if':
      // 'if: return EVAL(EVAL(ast[1], env) ? ast[2] : ast[3], env)
      // * `if`: Evaluate the first parameter (second element). If the result
      //   (condition) is anything other than `nil` or `false`, then evaluate
      //   the second parammeter (third element of the list) and return the
      //   result.  Otherwise, evaluate the third parameter (fourth element)
      //   and return the result. If condition is false and there is no third
      //   parameter, then just return `nil`.
      var argument1 = malList.malTypes[1];
      var argument2 = malList.malTypes[2];

      var result = EVAL(argument1, env);

      if ((result is MalBoolean && (result as MalBoolean).value)) {
        return EVAL(argument2, env);
      }

      if ( !(result is MalNil) && (result is MalType) && !(result is MalBoolean) ) {
        return EVAL(argument2, env);
      }

      if (malList.malTypes.length < 4) {
        return MAL_NIL;
      }

      var argument3 = malList.malTypes[3];
      return EVAL(argument3, env);

    case 'fn*':
      // 'fn*: return (...a) -> EVAL(ast[2], new Env(env, ast[1], a))
      var argument1 = malList.malTypes[1] as MalList;
      var argument2 = malList.malTypes[2];
      Env cur_env = env;
      return new MalFunction((List<MalType> arguments) => EVAL(argument2, new Env(outer: cur_env, binds: argument1, exprs: new MalList.fromList(arguments))));

    default:
      MalList astList = eval_ast(ast, env);
      var function = astList.malTypes[0] as VarargsFunction;
      var args = astList.malTypes.getRange(1, astList.malTypes.length).toList();
      var result = Function.apply(function, args);
      return result;
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
      stdout.writeln("Error: ${ex.message}");
      break;
    } on StateError catch (ex) {
      stdout.writeln("Error: ${ex.message}");
      break;
    }
  }
}
