package truffle.mal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class step1_read_print {

    public static void main(String[] args) throws IOException {
        boolean done = false;
        var reader = new BufferedReader(new InputStreamReader(System.in));
        while (!done) {
            System.out.print("user> ");
            String s = reader.readLine();
            if (s == null) {
                done = true;
            } else {
                try {
                    System.out.println(Printer.prStr(Reader.readStr(s), true));
                } catch (MalException ex) {
                    System.out.println(ex.getMessage());
                }
            }
        }
    }
}
