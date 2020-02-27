import 'dart:io';

import 'core.dart';
import 'env.dart';
import 'printer.dart' as printer;
import 'reader.dart' as reader;
import 'types.dart';

final Env replEnv = new Env();

void setupEnv() {
  ns.forEach((sym, fun) => replEnv.set(sym, fun));

  rep('(def! not (fn* (a) (if a false true)))');
}

MalType READ(String x) => reader.read_str(x);

MalType eval_ast(MalType ast, Env env) {
  if (ast is MalSymbol) {
    var result = env.get(ast);
    if (result == null) {
      throw new NotFoundException(ast.value);
    }
    return result;
  } else if (ast is MalList) {
    return new MalList(ast.elements.map((x) => EVAL(x, env)).toList());
  } else if (ast is MalVector) {
    return new MalVector(ast.elements.map((x) => EVAL(x, env)).toList());
  } else if (ast is MalHashMap) {
    var newMap = new Map<MalSymbol, MalType>.from(ast.value);
    for (var key in newMap.keys) {
      newMap[key] = EVAL(newMap[key], env);
    }
    return new MalHashMap(newMap);
  } else {
    return ast;
  }
}

MalType EVAL(MalType ast, Env env) {
  if (ast is! MalList) {
    return eval_ast(ast, env);
  } else {
    if ((ast as MalList).elements.isEmpty) {
      return ast;
    } else {
      var list = ast as MalList;
      if (list.elements.first is MalSymbol) {
        var symbol = list.elements.first as MalSymbol;
        var args = list.elements.sublist(1);
        if (symbol.value == "def!") {
          MalSymbol key = args.first;
          MalType value = EVAL(args[1], env);
          env.set(key, value);
          return value;
        } else if (symbol.value == "let*") {
          // TODO(het): If elements.length is not even, give helpful error
          Iterable<List<MalType>> pairs(List<MalType> elements) sync* {
            for (var i = 0; i < elements.length; i += 2) {
              yield [elements[i], elements[i + 1]];
            }
          }

          var newEnv = new Env(env);
          MalIterable bindings = args.first;
          for (var pair in pairs(bindings.elements)) {
            MalSymbol key = pair[0];
            MalType value = EVAL(pair[1], newEnv);
            newEnv.set(key, value);
          }
          return EVAL(args[1], newEnv);
        } else if (symbol.value == "do") {
          return args.map((e) => EVAL(e, env)).toList().last;
        } else if (symbol.value == "if") {
          var condition = EVAL(args[0], env);
          if (condition is MalNil ||
              condition is MalBool && condition.value == false) {
            // False side of branch
            if (args.length < 3) {
              return new MalNil();
            }
            return EVAL(args[2], env);
          } else {
            // True side of branch
            return EVAL(args[1], env);
          }
        } else if (symbol.value == "fn*") {
          var params = (args[0] as MalIterable)
              .elements
              .map((e) => e as MalSymbol)
              .toList();
          return new MalClosure(
              params,
              args[1],
              env,
              (List<MalType> funcArgs) =>
                  EVAL(args[1], new Env(env, params, funcArgs)));
        }
      }
      var newAst = eval_ast(ast, env) as MalList;
      var f = newAst.elements.first;
      if (f is MalCallable) {
        return f.call(newAst.elements.sublist(1));
      } else {
        throw 'bad!';
      }
    }
  }
}

String PRINT(MalType x) => printer.pr_str(x);

String rep(String x) {
  return PRINT(EVAL(READ(x), replEnv));
}

const prompt = 'user> ';
main() {
  setupEnv();
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
    } on MalException catch (e) {
      stdout.writeln("Error: ${printer.pr_str(e.value)}");
      continue;
    } on reader.NoInputException {
      continue;
    }
    stdout.writeln(output);
  }
}
