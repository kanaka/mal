package mal;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.stream.Collectors;

class core {
  static Map<String, mv> ns = new HashMap<>();

  static {
    ns.put(
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

    ns.put(
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

    ns.put(
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

    ns.put(
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

    ns.put(
        "prn",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            System.out.println(f1.toString(true));
            return new nil();
          }
        }
    );

    ns.put(
        "list",
        new fn() {
          @Override
          public mal apply(list args) {
            return args;
          }
        }
    );

    ns.put(
        "list?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof list && !(args.get(0) instanceof vector) && !(args.get(0) instanceof hash_map) ? new True() : new False();
          }
        }
    );

    ns.put(
        "empty?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof list && ((list) args.get(0)).size() == 0 ? new True() : new False();
          }
        }
    );

    ns.put(
        "count",
        new fn() {
          @Override
          public mal apply(list args) {
            if (!(args.get(0) instanceof list)) {
              return new number(0L);
            }
            return new number(((long) ((list) args.get(0)).size()));
          }
        }
    );

    ns.put(
        "=",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0).equals(args.get(1)) ? new True() : new False();
          }
        }
    );

    ns.put(
        "<",
        new fn() {
          @Override
          public mal apply(list args) {
            Optional<Long> reduce = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce((left, right) -> {
                  if (left < right) {
                    return right;
                  }
                  return Long.MAX_VALUE;
                });
            return reduce.get() == Long.MAX_VALUE ? new False() : new True();
          }
        }
    );

    ns.put(
        "<=",
        new fn() {
          @Override
          public mal apply(list args) {
            Optional<Long> reduce = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce((left, right) -> {
                  if (left <= right) {
                    return right;
                  }
                  return Long.MAX_VALUE;
                });
            return reduce.get() == Long.MAX_VALUE ? new False() : new True();
          }
        }
    );

    ns.put(
        ">",
        new fn() {
          @Override
          public mal apply(list args) {
            Optional<Long> reduce = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce((left, right) -> {
                  if (left > right) {
                    return right;
                  }
                  return Long.MIN_VALUE;
                });
            return reduce.get() == Long.MIN_VALUE ? new False() : new True();
          }
        }
    );

    ns.put(
        ">=",
        new fn() {
          @Override
          public mal apply(list args) {
            Optional<Long> reduce = args.data.stream()
                .map(item -> ((number) item).val)
                .reduce((left, right) -> {
                  if (left >= right) {
                    return right;
                  }
                  return Long.MIN_VALUE;
                });
            return reduce.get() == Long.MIN_VALUE ? new False() : new True();
          }
        }
    );

    ns.put(
        "pr-str",
        new fn() {
          @Override
          public mal apply(list args) {
            return new str(args.data
                .stream()
                .map(it -> printer.pr_str(it, true))
                .collect(Collectors.joining(" ")));
          }
        }
    );

    ns.put(
        "str",
        new fn() {
          @Override
          public mal apply(list args) {
            return new str(args.data
                .stream()
                .map(it -> printer.pr_str(it, false))
                .collect(Collectors.joining("")));
          }
        }
    );

    ns.put(
        "prn",
        new fn() {
          @Override
          public mal apply(list args) {
            System.out.println(args.data
                .stream()
                .map(it -> printer.pr_str(it, true))
                .collect(Collectors.joining(" ")));
            return new nil();
          }
        }
    );

    ns.put(
        "println",
        new fn() {
          @Override
          public mal apply(list args) {
            System.out.println(args.data
                .stream()
                .map(it -> printer.pr_str(it, false))
                .collect(Collectors.joining(" ")));
            return new nil();
          }
        }
    );

    ns.put(
        "read-string",
        new fn() {
          @Override
          public mal apply(list args) {
            return reader.read_str(((str) args.get(0)).val);
          }
        }
    );

    ns.put(
        "slurp",
        new fn() {
          @Override
          public mal apply(list args) {
            String name = ((str) args.get(0)).val;
            try {
              return new str(new Scanner(new File(name)).useDelimiter("\\Z").next() + "\n");
            } catch (FileNotFoundException e) {
              throw new RuntimeException(e.getMessage());
            }
          }
        }
    );

    ns.put(
        "atom",
        new fn() {
          @Override
          public mal apply(list args) {
            return new atom(args.get(0));
          }
        }
    );

    ns.put(
        "atom?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof atom ? new True() : new False();
          }
        }
    );

    ns.put(
        "deref",
        new fn() {
          @Override
          public mal apply(list args) {
            return ((atom) args.get(0)).val;
          }
        }
    );

    ns.put(
        "reset!",
        new fn() {
          @Override
          public mal apply(list args) {
            return ((atom) args.get(0)).val = args.get(1);
          }
        }
    );

    ns.put(
        "swap!",
        new fn() {
          @Override
          public mal apply(list args) {
            atom atom = (atom) args.get(0);
            list rest = args.rest().rest();
            rest.data.add(0, atom.val);

            fn fn = (fn) args.get(1);
            atom.val = fn.apply(rest);
            return atom.val;
          }
        }
    );

    ns.put(
        "cons",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            list f2 = (list) args.get(1);

            List<mal> target = new ArrayList<>();
            target.add(f1);
            target.addAll(f2.data);
            return new list(target);
          }
        }
    );

    ns.put(
        "concat",
        new fn() {
          @Override
          public mal apply(list args) {
            List<mal> targets = new ArrayList<>();
            for (mal malType : args.data) {
              targets.addAll(((list) malType).data);
            }
            return new list(targets);
          }
        }
    );

    ns.put(
        "nth",
        new fn() {
          @Override
          public mal apply(list args) {
            list f1 = (list) args.get(0);
            number f2 = (number) args.get(1);
            if (f1.size() <= f2.val) {
              throw new core.OutOfIndexException();
            }
            return f1.get(f2.val.intValue());
          }

        }
    );

    ns.put(
        "first",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            if (f1 instanceof nil) {
              return new nil();
            }
            list ml = ((list) f1);
            return ml.size() > 0 ? ml.get(0) : new nil();
          }
        }
    );

    ns.put(
        "rest",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            if (!(f1 instanceof list)) {
              return new list();
            }

            list list = (list) f1;
            if (list.size() == 0) {
              return new list();
            }

            return ((list) args.get(0)).rest();
          }
        }
    );

    ns.put(
        "throw",
        new fn() {
          @Override
          public mal apply(list args) {
            throw new mal_exception(args.get(0));
          }
        }
    );

    ns.put(
        "apply",
        new fn() {
          @Override
          public mal apply(list args) {
            mal func = args.get(0);

            list params = new list();
            for (mal item : args.rest().data) {
              if (item instanceof list) {
                for (mal innerItem : ((list) item).data) {
                  params.add(innerItem);
                }
              } else {
                params.add(item);
              }
            }

            return ((fn) func).apply(params);
          }
        }
    );

    ns.put(
        "map",
        new fn() {
          @Override
          public mal apply(list args) {
            mal func = args.get(0);

            List<mal> rets = ((list) args.rest().get(0)).data.stream()
                .map(it -> {
                      list params = new list();
                      params.add(it);
                      return ((fn) func).apply(params);
                    }
                ).collect(Collectors.toList());
            return new list(rets);
          }
        }
    );

    ns.put(
        "nil?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof nil ? new True() : new False();
          }
        }
    );

    ns.put(
        "true?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof True ? new True() : new False();
          }
        }
    );

    ns.put(
        "false?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof False ? new True() : new False();
          }
        }
    );

    ns.put(
        "symbol?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof symbol ? new True() : new False();
          }
        }
    );

    ns.put(
        "symbol",
        new fn() {
          @Override
          public mal apply(list args) {
            return new symbol(args.get(0).toString());
          }
        }
    );

    ns.put(
        "keyword?",
        new fn() {
          @Override
          public mal apply(list args) {
            if (args.get(0) instanceof str && ((str) args.get(0)).val.startsWith(":")) {
              return new True();
            }
            return args.get(0) instanceof keyword ? new True() : new False();
          }
        }
    );

    ns.put(
        "keyword",
        new fn() {
          @Override
          public mal apply(list args) {
            if (args.get(0) instanceof keyword) {
              return args.get(0);
            }

            return new keyword(":" + args.get(0).toString());
          }
        }
    );

    ns.put(
        "vector",
        new fn() {
          @Override
          public mal apply(list args) {
            return new vector(args.data);
          }
        }
    );

    ns.put(
        "vector?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof vector ? new True() : new False();
          }
        }
    );

    ns.put(
        "hash-map",
        new fn() {
          @Override
          public mal apply(list args) {
            return new hash_map(args.data);
          }
        }
    );

    ns.put(
        "map?",
        new fn() {
          @Override
          public mal apply(list args) {
            return args.get(0) instanceof hash_map ? new True() : new False();
          }
        }
    );

    ns.put(
        "keys",
        new fn() {
          @Override
          public mal apply(list args) {
            hash_map map = (hash_map) args.get(0);
            return map.keys();
          }
        }
    );

    ns.put(
        "vals",
        new fn() {
          @Override
          public mal apply(list args) {
            hash_map map = (hash_map) args.get(0);
            return map.vals();
          }
        }
    );

    ns.put(
        "get",
        new fn() {
          @Override
          public mal apply(list args) {
            if (!(args.get(0) instanceof hash_map)) {
              return new nil();
            }
            return ((hash_map) args.get(0)).get(args.get(1).toString());
          }
        }
    );

    ns.put(
        "contains?",
        new fn() {
          @Override
          public mal apply(list args) {
            return ((hash_map) args.get(0)).contains(args.get(1).toString());
          }
        }
    );

    ns.put(
        "sequential?",
        new fn() {
          @Override
          public mal apply(list args) {
            String s = args.get(0).toString();
            return s.startsWith("(") || s.startsWith("[") ? new True() : new False();
          }
        }
    );

    ns.put(
        "assoc",
        new fn() {
          @Override
          public mal apply(list args) {
            hash_map map = (hash_map) args.get(0);

            List<mal> data = new ArrayList<>(map.data);

            data.addAll(args.rest().data);
            return new hash_map(data);
          }
        }
    );

    ns.put(
        "dissoc",
        new fn() {
          @Override
          public mal apply(list args) {
            hash_map map = (hash_map) args.get(0);
            Map<String, mal> data = new HashMap<String, mal>(map.map);

            for (mal key : args.rest().data) {
              if (data.containsKey(key.toString())) {
                data.remove(key.toString());
              }
            }
            List<mal> dd = new ArrayList<mal>();
            data.forEach((key, val) -> {
              if (!key.startsWith(":")) {
                dd.add(new str(key));
              } else {
                dd.add(new keyword(key));
              }
              dd.add(val);
            });

            return new hash_map(dd);
          }
        }
    );

    ns.put(
        "readline",
        new fn() {
          @Override
          public mal apply(list args) {
            String prompt = ((str) args.get(0)).val;

            System.out.print(prompt);
            BufferedReader buffer = new BufferedReader(new InputStreamReader(System.in));
            String line = null;
            try {
              line = buffer.readLine();
            } catch (IOException e) {
              e.printStackTrace();
            }
            if (line == null) {
              return new nil();
            }
            return new str(line);
          }
        }
    );

    ns.put(
        "meta",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            if (!(f1 instanceof mv)) {
              return new nil();
            }
            mv meta = ((mv) f1).meta;
            if (meta == null) {
              return new nil();
            }
            return meta;
          }
        }
    );

    ns.put(
        "with-meta",
        new fn() {
          @Override
          public mal apply(list args) {
            mv copy = ((mv) args.get(0)).copy();
            copy.meta = ((mv) args.get(1));
            return copy;
          }
        }
    );

    ns.put(
        "string?",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            return f1 instanceof str && !((str) f1).val.startsWith(":") ? new True() : new False();
          }
        }
    );

    ns.put(
        "conj",
        new fn() {
          @Override
          public mal apply(list args) {
            mv copy = ((mv) args.get(0)).copy();

            if (copy instanceof vector) {
              for (int i = 1; i < args.data.size(); i++) {
                ((vector) copy).data.add(args.get(i));
              }
              return copy;
            }

            for (int i = 1; i < args.data.size(); i++) {
              ((list) copy).data.add(0, args.get(i));
            }

            return copy;
          }
        }
    );

    ns.put(
        "seq",
        new fn() {
          @Override
          public mal apply(list args) {
            mal f1 = args.get(0);
            if (f1 instanceof nil) {
              return new nil();
            }

            if (f1 instanceof str) {
              String val = ((str) f1).val;
              if (val.isEmpty()) {
                return new nil();
              }
              List<mal> strs = new ArrayList<>();
              for (int i = 0; i < val.length(); i++) {
                strs.add(new str(String.valueOf(val.charAt(i))));
              }

              return new list(strs);
            }

            list list = (list) f1;
            if (list.size() == 0) {
              return new nil();
            }

            return new list(list.data);
          }
        }
    );

    ns.put(
        "time-ms",
        new fn() {
          @Override
          public mal apply(list args) {
            return new number((System.currentTimeMillis()));
          }
        }
    );
  }

  public static class OutOfIndexException extends RuntimeException {
  }
}
