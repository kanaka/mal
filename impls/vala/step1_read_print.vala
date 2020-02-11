class Mal.Main : GLib.Object {
    static bool eof;

    static construct {
        eof = false;
    }

    public static Mal.Val? READ() {
        string? line = Readline.readline("user> ");
        if (line != null) {
            if (line.length > 0)
                Readline.History.add(line);

            try {
                return Reader.read_str(line);
            } catch (Mal.Error err) {
                GLib.stderr.printf("%s\n", err.message);
                return null;
            }
        } else {
            stdout.printf("\n");
            eof = true;
            return null;
        }
    }

    public static Mal.Val EVAL(Mal.Val expr) {
        return expr;
    }

    public static void PRINT(Mal.Val value) {
        stdout.printf("%s\n", pr_str(value));
    }

    public static void rep() {
        Mal.Val? val = READ();
        if (val != null) {
            val = EVAL(val);
            PRINT(val);
            GC.Core.maybe_collect();
        }
    }

    public static int main(string[] args) {
        while (!eof)
            rep();
        return 0;
    }
}
