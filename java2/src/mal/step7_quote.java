package mal;

import jline.console.ConsoleReader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class step7_quote {
  static final String FN_FLAG = "__FN__";

  static mal READ(String val) {
    return reader.read_str(val);
  }

  static mal EVAL(mal val, env.Env env) {
    while (true) {
      if (!(val instanceof list)) {
        return eval_ast(val, env);
      }

      mal expanded = macroexpand(val, env);
      if (!(expanded instanceof list)) {
        return eval_ast(expanded, env);
      }

      list ast = (list) expanded;
      if (ast.data.isEmpty()) {
        return ast;
      }

      String token = FN_FLAG;
      if (ast.data.get(0) instanceof symbol) {
        token = ((symbol) ast.data.get(0)).val;
      }

      switch (token) {
        case "defmacro!":
          mal res = EVAL(ast.get(2), env);
          ((fn) res).is_macro = true;
          env.set(((symbol) ast.get(1)).val, res);
          return res;
        case "macroexpand":
          return macroexpand(ast.get(1), env);
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
          eval_ast(new list(rest.data.subList(0, rest.size() - 1)), env);
          val = ast.get(ast.size() - 1);
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
        case "quote":
          return ast.get(1);
        case "quasiquote":
          val = quasiquote(ast.get(1));
          break;
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

  private static mal macroexpand(mal ast, env.Env env) {
    while (is_macro_call(ast, env)) {
      symbol a0 = (symbol) ((list) ast).get(0);
      mal ret = env.get(a0.val);

      ast = ((fn) ret).apply(((list) ast).rest());
    }
    return ast;
  }

  private static boolean is_macro_call(mal ast, env.Env env) {
    if (!(ast instanceof list)) {
      return false;
    }

    mal a0 = ((list) ast).get(0);
    if (!(a0 instanceof symbol)) {
      return false;
    }

    if (env.find(((symbol) a0).val) instanceof nil) {
      return false;
    }

    mal mac = env.get(((symbol) a0).val);
    if (!(mac instanceof fn)) {
      return false;
    }
    return ((fn) mac).is_macro;
  }

  private static mal quasiquote(mal ast) {
    if (!is_pair(ast)) {
      List<mal> data = new ArrayList<>();
      data.add(new symbol("quote"));
      data.add(ast);
      return new list(data);
    }

    mal a0 = ((list) ast).get(0);
    if ((a0 instanceof symbol) &&
        (((symbol) a0).val.equals("unquote"))) {
      return ((list) ast).get(1);
    } else if (is_pair(a0)) {
      mal a00 = ((list) a0).get(0);
      if ((a00 instanceof symbol) &&
          (((symbol) a00).val.equals("splice-unquote"))) {
        List<mal> result = new ArrayList<>();
        result.add(new symbol("concat"));
        result.add(((list) a0).get(1));
        result.add(quasiquote(((list) ast).rest()));
        return new list(result);
      }
    }
    List<mal> result = new ArrayList<>();
    result.add(new symbol("cons"));
    result.add(quasiquote(((list) ast).get(0)));
    result.add(quasiquote(((list) ast).rest()));
    return new list(result);
  }

  private static boolean is_pair(mal ast) {
    return ast instanceof list && ((list) ast).size() > 0;
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

    repl_env.set(
        "*ARGV*",
        new list(Arrays.stream(args).map(it -> new str(it)).skip(1).collect(Collectors.toList()))
    );

    final env.Env evalEnv = repl_env;
    repl_env.set(
        "eval",
        new fn() {
          @Override
          public mal apply(list args) {
            return EVAL(args.get(0), evalEnv);
          }
        }
    );

    List<String> codes = Arrays.asList(
        "(def! not (fn* (a) (if a false true)))",
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\" )))))",
        "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
        "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"
    );

    codes.forEach(code -> rep(code, repl_env));
    if (args.length > 0) {
      rep("(load-file \"" + args[0] + "\")", repl_env);
      return;
    }

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
