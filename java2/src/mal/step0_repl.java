package mal;

import jline.console.ConsoleReader;

public class step0_repl {

  public static String READ(String val) {
    return val;
  }

  public static String EVAL(String val) {
    return val;
  }

  public static String PRINT(String val) {
    return val;
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
      System.out.println(rep(line));
    }
  }
}
