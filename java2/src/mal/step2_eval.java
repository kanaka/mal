package mal;

import jline.console.ConsoleReader;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class step2_eval {

  public static mal READ(String val) {
    return reader.read_str(val);
  }

  public static mal EVAL(mal val, Map<String, fun> env) {
    if (!(val instanceof list)) {
      return eval_ast(val, env);
    }

    list ast = (list) val;
    if (ast.data.isEmpty()) {
      return ast;
    }

    mal ret = eval_ast(ast, env);
    if (!(ret instanceof list)) {
      throw new NotFoundException();
    }
    list list = (list) ret;
    if (!(list.data.get(0) instanceof fun)) {
      return list;
    }

    fun fun = ((fun) list.data.get(0));
    list args = new list(new ArrayList<>(list.data.subList(1, list.data.size())));

    return fun.apply(args);
  }

  public static mal eval_ast(mal ast, Map<String, fun> env) {
    if (ast instanceof symbol) {
      if (env.containsKey(((symbol) ast).val)) {
        return env.get(((symbol) ast).val);
      }
      throw new NotFoundException();
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

  public static String rep(String val, Map<String, fun> env) {
    return PRINT(EVAL(READ(val),env));
  }

  public static void main(String[] args) throws Exception {

    Map<String, fun> repl_env = new HashMap<>();
    repl_env.put(
        "+",
        args1 -> {
          Long ret = args1.data.stream()
              .map(item -> ((number) item).val)
              .reduce(0L, (left, right) -> left + right);
          return new number(ret);
        }
    );

    repl_env.put(
        "-",
        args1 -> {
          Optional<Long> ret = args1.data.stream()
              .map(item -> ((number) item).val)
              .reduce((left, right) -> left - right);
          return new number(ret.get());
        }
    );

    repl_env.put(
        "/",
        args1 -> {
          Optional<Long> ret = args1.data.stream()
              .map(item -> ((number) item).val)
              .reduce((left, right) -> left / right);
          return new number(ret.get());
        }
    );

    repl_env.put(
        "*",
        args1 -> {
          Long ret = args1.data.stream()
              .map(item -> ((number) item).val)
              .reduce(1L, (left, right) -> left * right);
          return new number(ret);
        }
    );

    while(true) {
      ConsoleReader reader = new ConsoleReader();
      String line = reader.readLine("user> ");
      if (line == null) {
        break;
      }
      try {
        System.out.println(rep(line, repl_env));
      } catch (reader.EOFException e) {
        System.out.println(e.msg);
      } catch (NotFoundException e) {
        System.out.println("NOT FOUND");
      } catch (Exception e) {
        // do nothing
      }
    }
  }

  @FunctionalInterface
  interface fun extends mal {
    mal apply(list args);
  }

  static class NotFoundException extends RuntimeException {
  }

}
