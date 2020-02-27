library STD;
use STD.textio.all;

package pkg_readline is
  procedure mal_printline(l: string);
  procedure mal_readline(prompt: string; eof_detected: out boolean; l: inout line);
end package pkg_readline;

package body pkg_readline is
  type charfile is file of character;
  file stdout_char: charfile open write_mode is "STD_OUTPUT";

  procedure mal_printstr(l: string) is
  begin
    for i in l'range loop
      write(stdout_char, l(i));
    end loop;
  end procedure mal_printstr;

  procedure mal_printline(l: string) is
  begin
    mal_printstr(l);
    write(stdout_char, LF);
  end procedure mal_printline;

  procedure mal_readline(prompt: string; eof_detected: out boolean; l: inout line) is
  begin
    mal_printstr(prompt);
    if endfile(input) then
      eof_detected := true;
    else
      readline(input, l);
      eof_detected := false;
    end if;
  end procedure mal_readline;
end package body pkg_readline;
