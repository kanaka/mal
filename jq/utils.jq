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