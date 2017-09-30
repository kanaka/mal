package mal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class types {
}

/**
 * the god, all mal type be impl this.
 */
interface mal{
  default String toString(boolean print_readably) {
    return this.toString();
  }
}

abstract class mv implements mal {
  mv meta;

  abstract mv copy();
}

class list extends mv {
  final List<mal> data;

  list() {
    this.data = new ArrayList<>();
  }

  list(List<mal> data) {
    this.data = data;
  }

  mal add(mal item) {
    this.data.add(item);
    return item;
  }

  @Override
  public String toString() {
    if (data.isEmpty()) {
      return "()";
    }

    return data.stream()
        .map(mal -> mal.toString(true))
        .collect(Collectors.joining(" ", "(", ")"));
  }

  list rest() {
    if (this.data.size() <= 1) {
      return new list();
    }

    return new list(new ArrayList<>(this.data.subList(1, this.data.size())));
  }

  int size() {
    return this.data.size();
  }

  mal get(int index) {
    if (index >= this.data.size()) {
      return new nil();
    }
    return this.data.get(index);
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof list) || obj instanceof hash_map) {
      return false;
    }
    list target = (list) obj;


    if (target.size() != this.size()) {
      return false;
    }

    for (int i = 0; i < this.size(); i++) {
      if (!this.get(i).equals(target.get(i))) {
        return false;
      }
    }

    return true;
  }

  @Override
  mv copy() {
    return new list(this.data);
  }
}

class vector extends list {
  vector() {
    super();
  }

  vector(List<mal> data) {
    super(data);
  }

  @Override
  public String toString() {
    if (data.isEmpty()) {
      return "[]";
    }

    return data.stream()
        .map(mal -> mal.toString(true))
        .collect(Collectors.joining(" ", "[", "]"));
  }

  @Override
  mv copy() {
    return new vector(this.data);
  }
}

class symbol extends mv {
  final String val;

  symbol(String val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val;
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof symbol && Objects.equals(((symbol) obj).val, this.val);
  }

  @Override
  mv copy() {
    return new symbol(this.val);
  }
}

class hash_map extends list {
  Map<String, mal> map = new HashMap<>();

  hash_map() {
    super();
  }

  hash_map(List<mal> data) {
    super(data);
    this.init();
  }

  private void init() {
    for (int i = 0; i < this.data.size(); i += 2) {
      mal mal = this.data.get(i);
      String key = null;
      if (mal instanceof keyword) {
        key = ((keyword) mal).val;
      } else if (mal instanceof str) {
        key = ((str) mal).val;
      }
      if (key == null) {
        continue;
      }
      map.put(key, this.data.get(i + 1));
    }
  }

  mal get(String keyword) {
    if (!this.map.containsKey(keyword)) {
      return new nil();
    }
    return this.map.get(keyword);
  }

  list keys() {
    List<mal> data = this.map.keySet().stream()
        .map(key -> {
          if (key.startsWith(":")) {
            return new keyword(key);
          }
          return new str(key);
        })
        .collect(Collectors.toList());
    return new list(data);
  }

  mal vals() {
    return new list(new ArrayList<>(this.map.values()));
  }

  bool contains(String keyword) {
    return this.map.containsKey(keyword) ? new True() : new False();
  }

  @Override
  public String toString() {
    if (data.isEmpty()) {
      return "{}";
    }

    return data.stream()
        .map(mal -> mal.toString(true))
        .collect(Collectors.joining(" ", "{", "}"));
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof hash_map)) {
      return false;
    }

    hash_map map = (hash_map) obj;

    for (String key : this.map.keySet()) {
      if (!map.map.containsKey(key)) {
        return false;
      }

      if (!this.map.get(key).equals(map.map.get(key))) {
        return false;
      }
    }

    return true;
  }

  @Override
  mv copy() {
    return new hash_map(data);
  }
}

class str extends mv {
  final String val;

  str(String val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return this.val;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof keyword) {
      return false;
    }
    return obj instanceof str && this.val.equals(((str) obj).val);
  }

  @Override
  mv copy() {
    return new str(val);
  }
}

class keyword extends str {
  keyword(String val) {
    super(val);
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof keyword && this.val.equals(((keyword) obj).val);
  }

  @Override
  public int hashCode() {
    return this.val.hashCode();
  }

  @Override
  mv copy() {
    return new keyword(val);
  }
}

class number extends mv {
  final Long val;

  number(Long val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val.toString();
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof number && Objects.equals(this.val, ((number) obj).val);
  }

  @Override
  mv copy() {
    return new number(val);
  }
}

class nil extends mv {

  @Override
  public String toString() {
    return "nil";
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof nil;
  }

  @Override
  mv copy() {
    return new nil();
  }
}

abstract class bool extends mv {
}

class True extends bool {

  @Override
  public String toString(boolean print_readably) {
    return this.toString();
  }

  @Override
  public String toString() {
    return "true";
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof True;
  }

  @Override
  mv copy() {
    return new True();
  }
}

class False extends bool {
  @Override
  public String toString(boolean print_readably) {
    return this.toString();
  }

  @Override
  public String toString() {
    return "false";
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof False;
  }

  @Override
  mv copy() {
    return new False();
  }
}

class atom extends mv {
  mal val;

  atom(mal val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return "(atom " + val.toString() + ")";
  }

  @Override
  mv copy() {
    return new atom(val);
  }
}

class mal_exception extends RuntimeException implements mal {
  mal val;

  mal_exception(mal val) {
    this.val = val;
  }
  mal_exception(String message) {
    super(message);
  }

  @Override
  public String toString() {
    if (val != null) {
      return printer.pr_str(val, true);
    }
    return this.getMessage();
  }
}

@FunctionalInterface
interface ILambda {
  mal apply(list args);
}

//@FunctionalInterface
//interface fun extends ILambda, mal {
//  @Override
//  default String toString(boolean print_readably) {
//    return "#<function>";
//  }
//}

abstract class fn extends mv implements ILambda, Cloneable {
  mal ast;
  env.Env env;
  list args;
  boolean is_macro;

  fn() {}

  fn(mal ast, env.Env env, list args, mv meta) {
    this.ast = ast;
    this.env = env;
    this.args = args;
    this.meta = meta;
  }

  @Override
  public String toString(boolean print_readably) {
    return toString();
  }

  @Override
  public String toString() {
    return "#<function>";
  }

  @Override
  mv copy() {
    try {
      fn clone = (fn) this.clone();
      clone.ast = ast;
      clone.env = env;
      clone.args = args;
      clone.is_macro = is_macro;
      return clone;
    } catch (CloneNotSupportedException e) {
      e.printStackTrace();
      throw new mal_exception("Could not copy fn");
    }
  }
}
