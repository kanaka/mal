#if js
    @:native("console")
    extern class Console {
        public static function log(s:Dynamic):Void;
    }

    @:native("process")
    extern class Process {
        public static var argv(default,null):Array<String>;
        public static function exit(code:Int):Void;
    }

    @:jsRequire("fs")
    extern class FS {
        static function readFileSync(filename:String,
                                     options:{encoding:String}):String;
    }

    @:jsRequire("./node_readline")
    extern class RL {
        static function readline(prompt:String):Null<String>;
    }
#end

class Compat {
    public static function println(s:String) {
        #if js
            Console.log(s);
        #else
            Sys.println(s);
        #end
    }

    public static function slurp(filename:String) {
        #if js
            return FS.readFileSync(filename, {encoding: "utf-8"});
        #else
            return sys.io.File.getContent(filename);
        #end
    }

    public static function exit(code:Int) {
        #if js
            Process.exit(0);
        #else
            Sys.exit(0);
        #end
    }

    public static function cmdline_args() {
        #if js
            return Process.argv.slice(2);
        #else
            return Sys.args();
        #end
    }

    public static function readline(prompt:String) {
        #if js
            var line = RL.readline("user> ");
            if (line == null) { throw new haxe.io.Eof(); }
        #else
            Sys.print("user> ");
            var line = Sys.stdin().readLine();
        #end
        return line;
    }


}
