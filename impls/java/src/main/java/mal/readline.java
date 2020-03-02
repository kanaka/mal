package mal;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.BufferedWriter;
import java.io.FileWriter;

import java.io.File;
import com.google.common.io.Files;
import java.nio.charset.StandardCharsets;
import java.util.List;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

class readline {
    public enum Mode { JNA, JAVA }
    static Mode mode = Mode.JNA;

    static String HISTORY_FILE = null;
    static Boolean historyLoaded = false;

    static {
        HISTORY_FILE = System.getProperty("user.home") + "/.mal-history";
    }

    public static class EOFException extends Exception {
    }

    public interface RLLibrary extends Library {
        // Select a library to use.
        // WARNING: GNU readline is GPL.

        // GNU readline (GPL)
            RLLibrary INSTANCE = (RLLibrary)
                Native.loadLibrary("readline", RLLibrary.class);
        // Libedit (BSD)
//            RLLibrary INSTANCE = (RLLibrary)
//                Native.loadLibrary("edit", RLLibrary.class);

        String readline(String prompt);
        void add_history(String line);
    }

    public static void loadHistory(String filename) {
        File file = new File(filename);
        try {
            List<String> lines = Files.readLines(file,
                                                StandardCharsets.UTF_8);
            for (String line : lines) {
                RLLibrary.INSTANCE.add_history(line);
            }
        } catch (IOException e) {
            // ignore
        }
    }

    public static void appendHistory(String filename, String line) {
        try {
            BufferedWriter w;
            w = new BufferedWriter(new FileWriter(filename, true));
            w.append(line + "\n");
            w.close();
        } catch (IOException e) {
            // ignore
        }
    }

    public static String jna_readline(String prompt)
            throws EOFException, IOException {
        if (!historyLoaded) {
            loadHistory(HISTORY_FILE);
        }
        String line = RLLibrary.INSTANCE.readline(prompt);
        if (line == null) {
            throw new EOFException();
        }
        RLLibrary.INSTANCE.add_history(line);
        appendHistory(HISTORY_FILE, line);
        return line;
    }

    // Just java readline (no history, or line editing)
    public static String java_readline(String prompt)
            throws EOFException, IOException {
        System.out.print(prompt);
        BufferedReader buffer=new BufferedReader(new InputStreamReader(System.in));
        String line=buffer.readLine();
        if (line == null) {
            throw new EOFException();
        }
        return line;
    }

    public static String readline(String prompt)
            throws EOFException, IOException {
        if (mode == Mode.JNA) {
            return jna_readline(prompt);
        } else {
            return java_readline(prompt);
        }
    }
}
