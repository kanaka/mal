def nwise(n):
    def _nwise:
        if length <= n then 
            .
        else
            .[0:n], (.[n:] | _nwise)
        end;
    _nwise;

def jqmal_error(e):
    error("JqMAL :: " + e);

def is_jqmal_error:
    startswith("JqMAL :: ");

def wrap(kind):
    {
        kind: kind,
        value: .
    };

def wrap2(kind; opts):
    opts + {
        kind: kind,
        value: .
    };

def _extern(options):
    {command: .} 
    | debug
    | if options.nowait | not then
        input | fromjson
      else
        null
      end; # oof

def issue_extern(cmd; options):
    {cmd: cmd, args: .}
    # | (tostring | debug) as $ignore
    | _extern(options);

def issue_extern(cmd):
    issue_extern(cmd; {});

def _print:
    debug;

def _write_to_file(name):
    . as $value
    | [(name|tojson), (.|tojson), (false|tojson)]
    | issue_extern("fwrite"; {nowait: true})
    | $value;

def _append_to_file(name):
    . as $value
    | [(name|tojson), (.|tojson), (true|tojson)]
    | issue_extern("fwrite"; {nowait: true})
    | $value;

def trap:
    _write_to_file("trap_reason.json") | jqmal_error("trap");