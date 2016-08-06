library STD;
use STD.textio.all;
library WORK;
use WORK.types.all;

package printer is
  procedure pr_str(ast: inout mal_val_ptr; readable: in boolean; result: out line);
  procedure pr_seq(start_ch: in string; end_ch: in string; delim: in string; a_seq: inout mal_seq_ptr; readable: in boolean; result: out line);
end package printer;

package body printer is

  procedure pr_string(val: inout line; readable: in boolean; result: out line) is
    variable s: line;
    variable src_i, dst_i: integer;
  begin
    if readable then
      s := new string(1 to val'length * 2);
      dst_i := 0;
      for src_i in val'range loop
        dst_i := dst_i + 1;
        case val(src_i) is
          when LF =>
            s(dst_i) := '\';
            dst_i := dst_i + 1;
            s(dst_i) := 'n';
          when '"' =>
            s(dst_i) := '\';
            dst_i := dst_i + 1;
            s(dst_i) := '"';
          when '\' =>
            s(dst_i) := '\';
            dst_i := dst_i + 1;
            s(dst_i) := '\';
          when others =>
            s(dst_i) := val(src_i);
        end case;
      end loop;
      result := new string'("" & '"' & s(1 to dst_i) & '"');
      deallocate(s);
    else
      result := val;
    end if;
  end;

  procedure pr_str(ast: inout mal_val_ptr; readable: in boolean; result: out line) is
    variable l: line;
  begin
    case ast.val_type is
      when mal_nil =>
        result := new string'("nil");
      when mal_true =>
        result := new string'("true");
      when mal_false =>
        result := new string'("false");
      when mal_number =>
        write(l, ast.number_val);
        result := l;
      when mal_symbol =>
        result := ast.string_val;
      when mal_string =>
        pr_string(ast.string_val, readable, result);
      when mal_keyword =>
        result := new string'(":" & ast.string_val.all);
      when mal_list =>
        pr_seq("(", ")", " ", ast.seq_val, readable, result);
      when mal_vector =>
        pr_seq("[", "]", " ", ast.seq_val, readable, result);
      when mal_hashmap =>
        pr_seq("{", "}", " ", ast.seq_val, readable, result);
      when mal_atom =>
        pr_str(ast.seq_val(0), true, l);
        result := new string'("(atom " & l.all & ")");
      when mal_nativefn =>
        result := new string'("#<NativeFunction:" & ast.string_val.all & ">");
      when mal_fn =>
        result := new string'("#<MalFunction>");
    end case;
  end procedure pr_str;

  procedure pr_seq(start_ch: in string; end_ch: in string; delim: in string; a_seq: inout mal_seq_ptr; readable: in boolean; result: out line) is
    variable s, element_s: line;
  begin
    s := new string'(start_ch);
    for i in a_seq'range loop
      pr_str(a_seq(i), readable, element_s);
      if i = 0 then
        s := new string'(s.all & element_s.all);
      else
        s := new string'(s.all & delim & element_s.all);
      end if;
    end loop;
    s := new string'(s.all & end_ch);
    result := s;
  end procedure pr_seq;

end package body printer;
