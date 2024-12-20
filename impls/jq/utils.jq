def nwise(n):
    def _nwise:
        if length <= n then 
            .
        else
            .[0:n], (.[n:] | _nwise)
        end;
    _nwise;

def abs(x):
    if x < 0 then 0 - x else x end;

def jqmal_error(e):
    error("JqMAL Exception :: " + e);

def is_jqmal_error:
    startswith("JqMAL Exception :: ");

def wrap(kind):
    {
        kind: kind,
        value: .
    };

def find_free_references(keys):
    def _refs:
      if . == null then [] else
        . as $dot
        | if .kind == "symbol" then
            if keys | contains([$dot.value]) then [] else [$dot.value] end
        else if "list" == $dot.kind then
            if $dot.value|length == 0 then
                []
            else
                # if - scan args
                # def! - scan body
                # let* - add keys sequentially, scan body
                # fn* - add keys, scan body
                # quote - []
                # quasiquote - ???
                $dot.value[0] as $head
                | if $head.kind == "symbol" then
                    (
                        select($head.value == "if") | $dot.value[1:] | map(_refs) | reduce .[] as $x ([]; . + $x)
                    ) // (
                        select($head.value == "def!") | $dot.value[2] | _refs
                    ) // (
                        select($head.value == "let*") | $dot.value[2] | find_free_references(($dot.value[1].value as $value | ([ range(0; $value|length; 2) ] | map(select(. % 2 == 0) | $value[.].value))) + keys)
                    ) // (
                        select($head.value == "fn*") | $dot.value[2] | find_free_references(($dot.value[1].value | map(.value)) + keys)
                    ) // (
                        select($head.value == "quote") | []
                    ) // (
                        select($head.value == "quasiquote") | []
                    ) // ($dot.value | map(_refs) | reduce .[] as $x ([]; . + $x))
                  else
                    [ $dot.values[1:][] | _refs ]
                  end
                end
        else if "vector" == $dot.kind then
            ($dot.value | map(_refs) | reduce .[] as $x ([]; . + $x))
        else if "hashmap" == $dot.kind then
            ([$dot.value | to_entries[] | ({kind: .value.kkind, value: .key}, .value.value) ] | map(_refs) | reduce .[] as $x ([]; . + $x))
        else
            []
        end end end end
      end;
    _refs | unique;

# The following IO actions are implemented in rts.py.

def __readline:
    ["readline", .] | debug | input;

# The output is not very interesting.
# 'input' here only ensures that the python process has printed the
# message before any further output by the jq process.
def _display:
    ["display", .] | debug | input;

def slurp:
    ["slurp", .] | debug | input;
