import Compat;
import types.Types.MalType;
import reader.*;
import printer.*;

class Step1_read_print {
    // READ
    static function READ(str:String):MalType {
        return Reader.read_str(str);
    }

    // EVAL
    static function EVAL(ast:MalType, env:String) {
        return ast;
    }

    // PRINT
    static function PRINT(exp:MalType):String {
        return Printer.pr_str(exp, true);
    }

    // repl
    static function rep(line:String) {
        return PRINT(EVAL(READ(line), ""));
    }

    public static function main() {
        while (true) {
            try {
                var line = Compat.readline("user> ");
                if (line == "") { continue; }
                Compat.println(rep(line));
            } catch (exc:BlankLine) {
                continue;
            } catch (exc:haxe.io.Eof) {
                Compat.exit(0);
            } catch (exc:Dynamic) {
                Compat.println(exc);
            }
        }
    }
}
