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
        #if js
            #error "JS not supported yet"
        #end
        while (true) {
            try {
                Sys.print("user> ");
                var line = Sys.stdin().readLine();
                if (line == "") { continue; }
                Sys.println(rep(line));
            } catch (exc:BlankLine) {
                continue;
            } catch (exc:haxe.io.Eof) {
                Sys.exit(0);
            } catch (exc:Dynamic) {
                Sys.println(exc);
            }
        }
    }
}
