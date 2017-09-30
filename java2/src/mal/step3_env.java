package mal;

import jline.console.ConsoleReader;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class step3_env {
  static final String FN_FLAG = "__FN__";

  public static mal READ(String val) {
    return reader.read_str(val);
  }

  public static mal EVAL(mal val, env.Env env) {
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
        String key = ((symbol) ast.data.get(1)).val;
        env.set(key, EVAL(ast.data.get(2), env));
        return env.get(key);
      case "let*":
        env.Env innerEnv = new env.Env(env);
        mal binding = ast.data.get(1);
        if (!(binding instanceof list)) {
          throw new RuntimeException();
        }

        list bindingList = (list) binding;
        for (int i = 0; i <= bindingList.data.size() / 2; i += 2) {
          innerEnv.set(((symbol) bindingList.data.get(i)).val, EVAL(((list) binding).data.get(i + 1), innerEnv));
        }

        return EVAL(ast.data.get(2), innerEnv);
      default:
        mal ret = eval_ast(ast, env);
        if (!(ret instanceof list)) {
          throw new RuntimeException();
        }

        list list = (list) ret;
        if (!(list.data.get(0) instanceof fn)) {
          return list;
        }
        fn fn = ((fn) list.data.get(0));
        list args = new list(new ArrayList<>(list.data.subList(1, list.data.size())));
        return fn.apply(args);
    }
  }

  public static mal eval_ast(mal ast, env.Env env) {
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

  public static String PRINT(mal val) {
    return printer.pr_str(val, true);
  }

  public static String rep(String val, env.Env env) {
    return PRINT(EVAL(READ(val), env));
  }

  public static void main(String[] args) throws Exception {

    env.Env repl_env = new env.Env(null);
    repl_env.set(
        "+",
        new fn() {
          @Override
          public mal apply(list args) {
            Long ret = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce(0L, (left, right) -> left + right);
            return new number(ret);
          }
        }
    );

    repl_env.set(
        "-",
        new fn() {
          @Override
          public mal apply(list args) {
            Optional<Long> ret = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce((left, right) -> left - right);
            return new number(ret.get());
          }
        }
    );

    repl_env.set(
        "/",
        new fn() {
          @Override
          public mal apply(list args) {
            Optional<Long> ret = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce((left, right) -> left / right);
            return new number(ret.get());
          }
        }
    );

    repl_env.set(
        "*",
        new fn() {
          @Override
          public mal apply(list args) {
            Long ret = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce(1L, (left, right) -> left * right);
            return new number(ret);
          }
        }
    );

    repl_env.set(
        "def!",
        new symbol("def!")
    );

    repl_env.set(
        "let*",
        new symbol("let*")
    );

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
        // do nothing
      }
    }
  }
}
