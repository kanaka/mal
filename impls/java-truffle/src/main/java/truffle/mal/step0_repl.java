package truffle.mal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class step0_repl {
    private static String READ(String s) {
        return s;
    }

    private static void PRINT(String s) {
        System.out.println(s);
    }

    private static void rep(String s) {
        PRINT(READ(s));
    }

    public static void main(String[] args) throws IOException {
        boolean done = false;
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        while (!done) {
            System.out.print("user> ");
            String s = reader.readLine();
            if (s == null) {
                done = true;
            } else {
                rep(s);
            }
        }
    }
}