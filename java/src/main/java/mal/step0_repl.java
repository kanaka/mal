package mal;

import java.io.IOException;

import mal.readline;

public class step0_repl {
    // read
    public static String READ(String str) {
        return str;
    }

    // eval
    public static String EVAL(String ast, String env) {
        return ast;
    }

    // print
    public static String PRINT(String exp) {
        return exp;
    }

    // repl
    public static String RE(String env, String str) {
        return EVAL(READ(str), env);
    }

    public static void main(String[] args) {
        String prompt = "user> ";

        if (args.length > 0 && args[0].equals("--raw")) {
            readline.mode = readline.Mode.JAVA;
        }
        while (true) {
            String line;
            try {
                line = readline.readline(prompt);
                if (line == null) { continue; }
            } catch (readline.EOFException e) {
                break;
            } catch (IOException e) {
                System.out.println("IOException: " + e.getMessage());
                break;
            }
            System.out.println(PRINT(RE(null, line)));
        }
    }
}
