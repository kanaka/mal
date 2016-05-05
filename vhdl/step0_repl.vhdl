entity step0_repl is
end entity step0_repl;

library STD;
use STD.textio.all;
library WORK;
use WORK.pkg_readline.all;

architecture test of step0_repl is
  function mal_READ(str: in string) return string is
  begin
    return str;
  end function mal_READ;

  function EVAL(ast: in string; env: in string) return string is
  begin
    return ast;
  end function EVAL;

  function mal_PRINT(exp: in string) return string is
  begin
    return exp;
  end function mal_PRINT;

  function REP(str: in string) return string is
  begin
    return mal_PRINT(EVAL(mal_READ(str), ""));
  end function REP;

  procedure repl is
    variable is_eof: boolean;
    variable input_line: line;
  begin
    loop
      mal_readline("user> ", is_eof, input_line);
      exit when is_eof;
      next when input_line'length = 0;
      mal_printline(REP(input_line.all));
    end loop;
    mal_printline("");
  end procedure repl;

begin
  repl;
end architecture test;
