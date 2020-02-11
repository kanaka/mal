class Mal.Main : GLib.Object {
    public static string? READ() {
        string? line = Readline.readline("user> ");
        if (line != null) {
            if (line.length > 0)
                Readline.History.add(line);
        } else {
            stdout.printf("\n");
        }
        return line;
    }

    public static string EVAL(string expr) {
        return expr;
    }

    public static void PRINT(string value) {
        stdout.printf("%s\n", value);
    }

    public static bool rep() {
        string? line = READ();
        if (line == null)
            return false;
        if (line.length > 0) {
            string value = EVAL(line);
            PRINT(value);
        }
        return true;
    }

    public static int main(string[] args) {
        while (rep());
        return 0;
    }
}
