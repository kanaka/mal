package mal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

class env {

  static class Env implements mal {
    Env outer;
    Map<String, mal> data;

    Env(Env outer) {
      this.data = new HashMap<String, mal>();
      this.outer = outer;
    }

    Env(Env outer, list binds, list exprs) {
      this.data = new HashMap<String, mal>();
      this.outer = outer;

      for (int i = 0; i < binds.data.size(); i++) {
        String token = ((symbol) binds.data.get(i)).val;

        if (token.startsWith("&")) {
          String key = ((symbol) binds.data.get(i + 1)).val;

          if (exprs.data.size() < i) {
            break;
          }

          ArrayList<mal> mals = new ArrayList<>(exprs.data.subList(i, exprs.data.size()));
          list value = new list(mals);
          this.data.put(key, value);
          break;
        }

        this.data.put(token, exprs.data.get(i));
      }
    }

    Env set(String key, mal val) {
      data.put(key, val);
      return this;
    }

    mal find(String key) {
      if (data.containsKey(key)) {
        return this;
      }

      if (outer != null) {
        return outer.find(key);
      }

      return new nil();
    }

    mal get(String key) {
      mal env = this.find(key);
      if (env instanceof nil) {
        throw new NotFoundException("symbol not found");
      }

      Env realEnv = (Env) env;
      return realEnv.data.get(key);
    }
  }

  static class NotFoundException extends RuntimeException {
    NotFoundException(String msg) {
      super(msg);
    }
  }
}
