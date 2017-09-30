package mal;

import jline.console.ConsoleReader;

public class step1_read_print {

  public static mal READ(String val) {
    return reader.read_str(val);
  }

  public static mal EVAL(mal val) {
    return val;
  }

  public static String PRINT(mal val) {
    return printer.pr_str(val, true);
  }

  public static String rep(String val) {
    return PRINT(EVAL(READ(val)));
  }

  public static void main(String[] args) throws Exception {
    while(true) {
      ConsoleReader reader = new ConsoleReader();
      String line = reader.readLine("user> ");
      if (line == null) {
        break;
      }
      try {
        System.out.println(rep(line));
      } catch (reader.EOFException e) {
        System.out.println(e.msg);
      }
    }
  }
}
