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
        #if js
            #error "JS not supported yet"
        #end
        while (true) {
            try {
                Sys.print("user> ");
                var line = Sys.stdin().readLine();
                Sys.println(rep(line));
            } catch (exc:haxe.io.Eof) {
                Sys.exit(0);
            } catch (exc:Dynamic) {
                Sys.println(exc);
            }
        }
    }
}
