import Compat;

class Step0_repl {
    // READ
    static function READ(str:String) {
        return str;
    }

    // EVAL
    static function EVAL(ast:String, env:String) {
        return ast;
    }

    // PRINT
    static function PRINT(exp:String) {
        return exp;
    }

    // repl
    static function rep(line:String) {
        return PRINT(EVAL(READ(line), ""));
    }

    public static function main() {
        while (true) {
            try {
                var line = Compat.readline("user> ");
                Compat.println(rep(line));
            } catch (exc:haxe.io.Eof) {
                Compat.exit(0);
            } catch (exc:Dynamic) {
                Compat.println(exc);
            }
        }
    }
}
