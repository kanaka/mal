package mal;

import jline.console.ConsoleReader;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class step5_tco {
  static final String FN_FLAG = "__FN__";

  static mal READ(String val) {
    return reader.read_str(val);
  }

  static mal EVAL(mal val, env.Env env) {
    while (true) {
      if (!(val instanceof list)) {
        return eval_ast(val, env);
      }

      list ast = (list) val;
      if (ast.data.isEmpty()) {
        return ast;
      }

      String token = FN_FLAG;
      if (ast.data.get(0) instanceof symbol) {
        token = ((symbol) ast.data.get(0)).val;
      }

      switch (token) {
        case "def!":
          String key = ((symbol) ast.get(1)).val;
          env.set(key, EVAL(ast.get(2), env));
          return env.get(key);
        case "let*":
          env.Env innerEnv = new env.Env(env);
          mal binding = ast.get(1);
          if (!(binding instanceof list)) {
            throw new RuntimeException();
          }

          list bindingList = (list) binding;
          for (int i = 0; i <= bindingList.size() / 2; i += 2) {
            innerEnv.set(((symbol) bindingList.get(i)).val, EVAL(((list) binding).get(i + 1), innerEnv));
          }

          env = innerEnv;
          val = ast.get(2);
          break;
        case "do":
          list rest = ast.rest();
          eval_ast(rest, env);
          val = rest.get(rest.size() - 1);
          break;
        case "if":
          mal bool = EVAL(ast.get(1), env);
          if (bool instanceof nil || bool instanceof False) {
            val = ast.get(3);
          } else {
            val = ast.get(2);
          }
          break;
        case "fn*":
          final list f1 = ((list) ast.get(1));
          final mal f2 = ast.get(2);
          final env.Env currentEnv = env;

          return new fn(f2, env, f1, null) {
            @Override
            public mal apply(list args) {
              return EVAL(f2, new env.Env(currentEnv, f1, args));
            }
          };
        default:
          mal el = eval_ast(ast, env);
          if (!(el instanceof list)) {
            throw new RuntimeException();
          }

          list list = (list) el;
          mal ff = list.get(0);
          if (!(ff instanceof fn)) {
            return list;
          }

          fn fn = ((fn) ff);
          if (fn.ast == null || fn.ast instanceof nil) {
            return fn.apply(((list) el).rest());
          }
          val = fn.ast;
          env = new env.Env(fn.env, fn.args, ((list) el).rest());
      }
    }
  }

  static mal eval_ast(mal ast, env.Env env) {
    if (ast instanceof symbol) {
      if (env.find(((symbol) ast).val) instanceof nil) {
        throw new env.NotFoundException("symbol not found");
      } else {
        return env.get(((symbol) ast).val);
      }
    }

    if (ast instanceof hash_map) {
      hash_map hash_map = (hash_map) ast;
      List<mal> rets = hash_map.data.stream()
          .map(item -> EVAL(item, env))
          .collect(Collectors.toList());
      return new hash_map(rets);
    }

    if (ast instanceof vector) {
      vector vector = (vector) ast;
      List<mal> rets = vector.data.stream()
          .map(item -> EVAL(item, env))
          .collect(Collectors.toList());
      return new vector(rets);
    }

    if (ast instanceof list) {
      list list = (list) ast;
      List<mal> rets = list.data.stream()
          .map(item -> EVAL(item, env))
          .collect(Collectors.toList());
      return new list(rets);
    }

    return ast;
  }

  static String PRINT(mal val) {
    return printer.pr_str(val, true);
  }

  static String rep(String val, env.Env env) {
    return PRINT(EVAL(READ(val), env));
  }

  public static void main(String[] args) throws Exception {

    env.Env repl_env = new env.Env(null);
    core.ns.forEach(repl_env::set);

    List<String> codes = Arrays.asList(
        "(def! not (fn* (a) (if a false true)))"
    );

    codes.forEach(code -> rep(code, repl_env));

    while (true) {
      ConsoleReader reader = new ConsoleReader();
      String line = reader.readLine("user> ");
      if (line == null) {
        break;
      }
      try {
        System.out.println(rep(line, repl_env));
      } catch (reader.EOFException e) {
        System.out.println(e.msg);
      } catch (env.NotFoundException e) {
        System.out.println("NOT FOUND");
      } catch (Exception e) {
        e.printStackTrace();
        // do nothing
      }
    }
  }
}
