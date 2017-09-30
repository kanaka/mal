package mal;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class reader {

  static mal read_str(String val) {
    return read_form(new Reader(tokenizer(val)));
  }

  static mal read_form(Reader reader) {
    String token = reader.peek();
    if (token == null) {
      throw new EOFException();
    }
    mal form;
    switch (token) {
      case "'":
      reader.next();
      return read_shorthand(new symbol("quote"), read_form(reader));
      case "`":
      reader.next();
      return read_shorthand(new symbol("quasiquote"), read_form(reader));
      case "~":
      reader.next();
      return read_shorthand(new symbol("unquote"), read_form(reader));
      case "@":
      reader.next();
      return read_shorthand(new symbol("deref"), read_form(reader));
      case "~@":
      reader.next();
      return read_shorthand(new symbol("splice-unquote"), read_form(reader));
      case "^":
      reader.next();
      mal meta = read_form(reader);
      mal data = read_form(reader);
      return read_with_meta(new symbol("with-meta"), meta, data);
      case "(":
      case "[":
      case "{":
        form = read_list(reader);
        break;
      default:
        form = read_atom(reader);
    }
    return form;
  }

  private static list read_with_meta(symbol symbol, mal meta, mal data) {
    list list = new list();
    list.add(symbol);
    list.add(data);
    list.add(meta);
    return list;
  }

  static list read_shorthand(symbol symbol, mal data) {
    list list = new list();
    list.add(symbol);
    list.add(data);
    return list;
  }

  static list read_list(Reader reader) {
    String token = reader.next();
    switch (token) {
      case "[":
        vector vector = new vector();

        while (!reader.peek().equals("]")) {
          vector.add(read_form(reader));

          if (reader.peek() == null) {
            throw new EOFException("]");
          }
        }
        reader.next();
        return vector;
      case "{":
        List<mal> data = new ArrayList<mal>();
        while (!reader.peek().equals("}")) {
          data.add(read_form(reader));

          if (reader.peek() == null) {
            throw new EOFException("}");
          }
        }
        reader.next();

        return new hash_map(data);
      default:
        list list = new list();
        while (!reader.peek().equals(")")) {
          list.add(read_form(reader));

          if (reader.peek() == null) {
            throw new EOFException(")");
          }
        }
        reader.next();
        return list;
    }
  }

  static mal read_atom(Reader reader) {
    String token = reader.next();

    try {
      return new number(Long.parseLong(token));
    } catch (NumberFormatException e) {
      // do nothing
    }

    if (token.startsWith("\"")) {
      String content = token.substring(1, token.length() - 1);

      Pattern pattern = Pattern.compile("\\\\(.)");
      Matcher matcher = pattern.matcher(content);

      StringBuffer sb = new StringBuffer();
      while (matcher.find()) {
        if (matcher.group(1).equals("n")) {
          matcher.appendReplacement(sb, "\n");
        }else if (matcher.group(1).equals("\\")) {
          matcher.appendReplacement(sb, "\\\\");
        }else {
          matcher.appendReplacement(sb, matcher.group(1));
        }
      }
      matcher.appendTail(sb);
      return new str(sb.toString());
    }

    if (token.startsWith(":")) {
      return new keyword(token);
    }

    switch (token) {
      case "true":
        return new True();
      case "false":
        return new False();
      case "nil":
        return new nil();
      default:
        return new symbol(token);
    }
  }

  static List<String> tokenizer(String val) {
    List<String> tokens = new ArrayList<>();
    Pattern pattern = Pattern.compile("[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)");
    Matcher matcher = pattern.matcher(val);

    while (matcher.find()) {
      String token = matcher.group(1);
      if (token != null &&
          !token.equals("") &&
          !(token.charAt(0) == ';')) {
        tokens.add(token);
      }
    }

    return tokens;
  }

  static class Reader {
    List<String> tokens;
    int position;

    Reader(List<String> tokens) {
      this.tokens = tokens;
      this.position = 0;
    }

    String next() {
      if (position == tokens.size()) {
        return null;
      }
      String token = tokens.get(position);
      position++;
      return token;
    }

    String peek() {
      if (position == tokens.size()) {
        return null;
      }
      return tokens.get(position);
    }
  }

  static class EOFException extends RuntimeException {
    final String msg;

    EOFException() {
      this("");
    }

    EOFException(String msg) {
      this.msg = msg;
    }
  }
}
