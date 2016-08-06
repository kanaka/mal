library STD;
use STD.textio.all;
library WORK;
use WORK.types.all;
use WORK.env.all;
use WORK.reader.all;
use WORK.printer.all;
use WORK.pkg_readline.all;

package core is
  procedure eval_native_func(func_sym: inout mal_val_ptr; args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr);
  procedure define_core_functions(e: inout env_ptr);
end package core;

package body core is

  procedure fn_equal(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable is_equal: boolean;
  begin
    equal_q(args.seq_val(0), args.seq_val(1), is_equal);
    new_boolean(is_equal, result);
  end procedure fn_equal;

  procedure fn_throw(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    err := args.seq_val(0);
  end procedure fn_throw;

  procedure fn_nil_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_nil, result);
  end procedure fn_nil_q;

  procedure fn_true_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_true, result);
  end procedure fn_true_q;

  procedure fn_false_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_false, result);
  end procedure fn_false_q;

  procedure fn_string_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_string, result);
  end procedure fn_string_q;

  procedure fn_symbol(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_symbol(args.seq_val(0).string_val, result);
  end procedure fn_symbol;

  procedure fn_symbol_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_symbol, result);
  end procedure fn_symbol_q;

  procedure fn_keyword(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_keyword(args.seq_val(0).string_val, result);
  end procedure fn_keyword;

  procedure fn_keyword_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_keyword, result);
  end procedure fn_keyword_q;

  procedure fn_pr_str(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable s: line;
  begin
    pr_seq("", "", " ", args.seq_val, true, s);
    new_string(s, result);
  end procedure fn_pr_str;

  procedure fn_str(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable s: line;
  begin
    pr_seq("", "", "", args.seq_val, false, s);
    new_string(s, result);
  end procedure fn_str;

  procedure fn_prn(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable s: line;
  begin
    pr_seq("", "", " ", args.seq_val, true, s);
    mal_printline(s.all);
    new_nil(result);
  end procedure fn_prn;

  procedure fn_println(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable s: line;
  begin
    pr_seq("", "", " ", args.seq_val, false, s);
    mal_printline(s.all);
    new_nil(result);
  end procedure fn_println;

  procedure fn_read_string(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable ast: mal_val_ptr;
  begin
    read_str(args.seq_val(0).string_val.all, ast, err);
    if ast = null then
      new_nil(result);
    else
      result := ast;
    end if;
  end procedure fn_read_string;

  procedure fn_readline(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable input_line: line;
    variable is_eof: boolean;
  begin
    mal_readline(args.seq_val(0).string_val.all, is_eof, input_line);
    if is_eof then
      new_nil(result);
    else
      new_string(input_line, result);
    end if;
  end procedure fn_readline;

  procedure fn_slurp(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    file f: text;
    variable status: file_open_status;
    variable save_content, content, one_line: line;
  begin
    file_open(status, f, external_name => args.seq_val(0).string_val.all, open_kind => read_mode);
    if status = open_ok then
      content := new string'("");
      while not endfile(f) loop
        readline(f, one_line);
        save_content := content;
        content := new string'(save_content.all & one_line.all & LF);
        deallocate(save_content);
      end loop;
      file_close(f);
      new_string(content, result);
    else
      new_string("Error opening file", err);
    end if;
  end procedure fn_slurp;

  procedure fn_lt(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).number_val < args.seq_val(1).number_val, result);
  end procedure fn_lt;

  procedure fn_lte(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).number_val <= args.seq_val(1).number_val, result);
  end procedure fn_lte;

  procedure fn_gt(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).number_val > args.seq_val(1).number_val, result);
  end procedure fn_gt;

  procedure fn_gte(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).number_val >= args.seq_val(1).number_val, result);
  end procedure fn_gte;

  procedure fn_add(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_number(args.seq_val(0).number_val + args.seq_val(1).number_val, result);
  end procedure fn_add;

  procedure fn_sub(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_number(args.seq_val(0).number_val - args.seq_val(1).number_val, result);
  end procedure fn_sub;

  procedure fn_mul(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_number(args.seq_val(0).number_val * args.seq_val(1).number_val, result);
  end procedure fn_mul;

  procedure fn_div(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_number(args.seq_val(0).number_val / args.seq_val(1).number_val, result);
  end procedure fn_div;

  -- Define physical types (c_seconds64, c_microseconds64) because these are
  -- represented as 64-bit words when passed to C functions
  type c_seconds64 is range 0 to 1E16
    units
      c_sec;
    end units c_seconds64;

  type c_microseconds64 is range 0 to 1E6
    units
      c_usec;
    end units c_microseconds64;

  type c_timeval is record
    tv_sec: c_seconds64;
    tv_usec: c_microseconds64;
  end record c_timeval;

  -- Leave enough room for two 64-bit words
  type c_timezone is record
    dummy_1: c_seconds64;
    dummy_2: c_seconds64;
  end record c_timezone;

  function gettimeofday(tv: c_timeval; tz: c_timezone) return integer;
  attribute foreign of gettimeofday: function is "VHPIDIRECT gettimeofday";

  function gettimeofday(tv: c_timeval; tz: c_timezone) return integer is
  begin
    assert false severity failure;
  end function gettimeofday;

  -- Returns the number of milliseconds since 2000-01-01 00:00:00 UTC because
  -- a standard VHDL integer is 32-bit and therefore cannot hold the number of
  -- milliseconds since 1970-01-01.
  procedure fn_time_ms(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable tv: c_timeval;
    variable dummy: c_timezone;
    variable rc: integer;
    constant utc_2000_01_01: c_seconds64 := 946684800 c_sec; -- UNIX time at 2000-01-01 00:00:00 UTC
  begin
    rc := gettimeofday(tv, dummy);
    new_number(((tv.tv_sec - utc_2000_01_01) / 1 c_sec) * 1000 + (tv.tv_usec / 1000 c_usec), result);
  end procedure fn_time_ms;

  procedure fn_list(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    result := args;
  end procedure fn_list;

  procedure fn_list_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_list, result);
  end procedure fn_list_q;

  procedure fn_vector(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    args.val_type := mal_vector;
    result := args;
  end procedure fn_vector;

  procedure fn_vector_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_vector, result);
  end procedure fn_vector_q;

  procedure fn_hash_map(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    args.val_type := mal_hashmap;
    result := args;
  end procedure fn_hash_map;

  procedure fn_map_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(args.seq_val(0).val_type = mal_hashmap, result);
  end procedure fn_map_q;

  procedure fn_assoc(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable new_hashmap: mal_val_ptr;
    variable i: integer;
  begin
    hashmap_copy(args.seq_val(0), new_hashmap);
    i := 1;
    while i < args.seq_val'length loop
      hashmap_put(new_hashmap, args.seq_val(i), args.seq_val(i + 1));
      i := i + 2;
    end loop;
    result := new_hashmap;
  end procedure fn_assoc;

  procedure fn_dissoc(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable new_hashmap: mal_val_ptr;
    variable i: integer;
  begin
    hashmap_copy(args.seq_val(0), new_hashmap);
    for i in 1 to args.seq_val'high loop
      hashmap_delete(new_hashmap, args.seq_val(i));
    end loop;
    result := new_hashmap;
  end procedure fn_dissoc;

  procedure fn_get(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable a1: mal_val_ptr := args.seq_val(1);
    variable val: mal_val_ptr;
  begin
    if a0.val_type = mal_nil then
      new_nil(result);
    else
      hashmap_get(a0, a1, val);
      if val = null then
        new_nil(result);
      else
        result := val;
      end if;
    end if;
  end procedure fn_get;

  procedure fn_contains_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable a1: mal_val_ptr := args.seq_val(1);
    variable found: boolean;
  begin
    hashmap_contains(a0, a1, found);
    new_boolean(found, result);
  end procedure fn_contains_q;

  procedure fn_keys(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable seq: mal_seq_ptr;
  begin
    seq := new mal_seq(0 to a0.seq_val'length / 2 - 1);
    for i in seq'range loop
      seq(i) := a0.seq_val(i * 2);
    end loop;
    new_seq_obj(mal_list, seq, result);
  end procedure fn_keys;

  procedure fn_vals(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable seq: mal_seq_ptr;
  begin
    seq := new mal_seq(0 to a0.seq_val'length / 2 - 1);
    for i in seq'range loop
      seq(i) := a0.seq_val(i * 2 + 1);
    end loop;
    new_seq_obj(mal_list, seq, result);
  end procedure fn_vals;

  procedure cons_helper(a0: inout mal_val_ptr; a1: inout mal_val_ptr; result: out mal_val_ptr) is
    variable seq: mal_seq_ptr;
  begin
    seq := new mal_seq(0 to a1.seq_val'length);
    seq(0) := a0;
    seq(1 to seq'length - 1) := a1.seq_val(0 to a1.seq_val'length - 1);
    new_seq_obj(mal_list, seq, result);
  end procedure cons_helper;

  procedure fn_cons(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable a1: mal_val_ptr := args.seq_val(1);
    variable seq: mal_seq_ptr;
  begin
    cons_helper(a0, a1, result);
  end procedure fn_cons;

  procedure fn_sequential_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_boolean(is_sequential_type(args.seq_val(0).val_type), result);
  end procedure fn_sequential_q;

  procedure fn_concat(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable seq: mal_seq_ptr;
    variable i: integer;
  begin
    seq := new mal_seq(0 to -1);
    for i in args.seq_val'range loop
      seq := new mal_seq'(seq.all & args.seq_val(i).seq_val.all);
    end loop;
    new_seq_obj(mal_list, seq, result);
  end procedure fn_concat;

  procedure fn_nth(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable lst_seq: mal_seq_ptr := args.seq_val(0).seq_val;
    variable index: integer := args.seq_val(1).number_val;
  begin
    if index >= lst_seq'length then
      new_string("nth: index out of range", err);
    else
      result := lst_seq(index);
    end if;
  end procedure fn_nth;

  procedure fn_first(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
  begin
    if a0.val_type = mal_nil or a0.seq_val'length = 0 then
      new_nil(result);
    else
      result := a0.seq_val(0);
    end if;
  end procedure fn_first;

  procedure fn_rest(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable seq: mal_seq_ptr;
    variable new_list: mal_val_ptr;
  begin
    if a0.val_type = mal_nil or a0.seq_val'length = 0 then
      seq := new mal_seq(0 to -1);
      new_seq_obj(mal_list, seq, result);
    else
      seq_drop_prefix(a0, 1, new_list);
      new_list.val_type := mal_list;
      result := new_list;
    end if;
  end procedure fn_rest;

  procedure fn_empty_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable is_empty: boolean;
  begin
    case args.seq_val(0).val_type is
      when mal_nil => new_boolean(true, result);
      when mal_list | mal_vector => new_boolean(args.seq_val(0).seq_val'length = 0, result);
      when others => new_string("empty?: invalid argument type", err);
    end case;
  end procedure fn_empty_q;

  procedure fn_count(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable count: integer;
  begin
    case args.seq_val(0).val_type is
      when mal_nil => new_number(0, result);
      when mal_list | mal_vector => new_number(args.seq_val(0).seq_val'length, result);
      when others => new_string("count: invalid argument type", err);
    end case;
  end procedure fn_count;

  procedure fn_conj(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable r: mal_val_ptr;
    variable seq: mal_seq_ptr;
  begin
    case a0.val_type is
      when mal_list =>
        r := a0;
        for i in 1 to args.seq_val'high loop
          cons_helper(args.seq_val(i), r, r);
        end loop;
        result := r;
      when mal_vector =>
        seq := new mal_seq(0 to a0.seq_val'length + args.seq_val'length - 2);
        seq(0 to a0.seq_val'high) := a0.seq_val(a0.seq_val'range);
        seq(a0.seq_val'high + 1 to seq'high) := args.seq_val(1 to args.seq_val'high);
        new_seq_obj(mal_vector, seq, result);
      when others =>
        new_string("conj requires list or vector", err);
    end case;
  end procedure fn_conj;

  procedure fn_seq(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable new_seq: mal_seq_ptr;
  begin
    case a0.val_type is
      when mal_string =>
        if a0.string_val'length = 0 then
          new_nil(result);
        else
          new_seq := new mal_seq(0 to a0.string_val'length - 1);
          for i in new_seq'range loop
            new_string("" & a0.string_val(i + 1), new_seq(i));
          end loop;
          new_seq_obj(mal_list, new_seq, result);
        end if;
      when mal_list =>
        if a0.seq_val'length = 0 then
          new_nil(result);
        else
          result := a0;
        end if;
      when mal_vector =>
        if a0.seq_val'length = 0 then
          new_nil(result);
        else
          new_seq_obj(mal_list, a0.seq_val, result);
        end if;
      when mal_nil =>
        new_nil(result);
      when others =>
        new_string("seq requires string or list or vector or nil", err);
    end case;
  end procedure fn_seq;

  procedure fn_meta(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable meta_val: mal_val_ptr;
  begin
    meta_val := args.seq_val(0).meta_val;
    if meta_val = null then
      new_nil(result);
    else
      result := meta_val;
    end if;
  end procedure fn_meta;

  procedure fn_with_meta(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
  begin
    result := new mal_val'(val_type => a0.val_type, number_val => a0.number_val, string_val => a0.string_val, seq_val => a0.seq_val, func_val => a0.func_val, meta_val => args.seq_val(1));
  end procedure fn_with_meta;

  procedure fn_atom(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
  begin
    new_atom(args.seq_val(0), result);
  end procedure fn_atom;

  procedure fn_atom_q(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
  begin
    new_boolean(a0.val_type = mal_atom, result);
  end procedure fn_atom_q;

  procedure fn_deref(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
  begin
    result := a0.seq_val(0);
  end procedure fn_deref;

  procedure fn_reset(args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable a0: mal_val_ptr := args.seq_val(0);
    variable a1: mal_val_ptr := args.seq_val(1);
  begin
    a0.seq_val(0) := a1;
    result := a1;
  end procedure fn_reset;

  procedure eval_native_func(func_sym: inout mal_val_ptr; args: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable f: line;
  begin
    if func_sym.val_type /= mal_nativefn then
      new_string("not a native function!", err);
      return;
    end if;
    f := func_sym.string_val;
    if    f.all = "="           then fn_equal(args, result, err);
    elsif f.all = "throw"       then fn_throw(args, result, err);
    elsif f.all = "nil?"        then fn_nil_q(args, result, err);
    elsif f.all = "true?"       then fn_true_q(args, result, err);
    elsif f.all = "false?"      then fn_false_q(args, result, err);
    elsif f.all = "string?"     then fn_string_q(args, result, err);
    elsif f.all = "symbol"      then fn_symbol(args, result, err);
    elsif f.all = "symbol?"     then fn_symbol_q(args, result, err);
    elsif f.all = "keyword"     then fn_keyword(args, result, err);
    elsif f.all = "keyword?"    then fn_keyword_q(args, result, err);
    elsif f.all = "pr-str"      then fn_pr_str(args, result, err);
    elsif f.all = "str"         then fn_str(args, result, err);
    elsif f.all = "prn"         then fn_prn(args, result, err);
    elsif f.all = "println"     then fn_println(args, result, err);
    elsif f.all = "read-string" then fn_read_string(args, result, err);
    elsif f.all = "readline"    then fn_readline(args, result, err);
    elsif f.all = "slurp"       then fn_slurp(args, result, err);
    elsif f.all = "<"           then fn_lt(args, result, err);
    elsif f.all = "<="          then fn_lte(args, result, err);
    elsif f.all = ">"           then fn_gt(args, result, err);
    elsif f.all = ">="          then fn_gte(args, result, err);
    elsif f.all = "+"           then fn_add(args, result, err);
    elsif f.all = "-"           then fn_sub(args, result, err);
    elsif f.all = "*"           then fn_mul(args, result, err);
    elsif f.all = "/"           then fn_div(args, result, err);
    elsif f.all = "time-ms"     then fn_time_ms(args, result, err);
    elsif f.all = "list"        then fn_list(args, result, err);
    elsif f.all = "list?"       then fn_list_q(args, result, err);
    elsif f.all = "vector"      then fn_vector(args, result, err);
    elsif f.all = "vector?"     then fn_vector_q(args, result, err);
    elsif f.all = "hash-map"    then fn_hash_map(args, result, err);
    elsif f.all = "map?"        then fn_map_q(args, result, err);
    elsif f.all = "assoc"       then fn_assoc(args, result, err);
    elsif f.all = "dissoc"      then fn_dissoc(args, result, err);
    elsif f.all = "get"         then fn_get(args, result, err);
    elsif f.all = "contains?"   then fn_contains_q(args, result, err);
    elsif f.all = "keys"        then fn_keys(args, result, err);
    elsif f.all = "vals"        then fn_vals(args, result, err);
    elsif f.all = "sequential?" then fn_sequential_q(args, result, err);
    elsif f.all = "cons"        then fn_cons(args, result, err);
    elsif f.all = "concat"      then fn_concat(args, result, err);
    elsif f.all = "nth"         then fn_nth(args, result, err);
    elsif f.all = "first"       then fn_first(args, result, err);
    elsif f.all = "rest"        then fn_rest(args, result, err);
    elsif f.all = "empty?"      then fn_empty_q(args, result, err);
    elsif f.all = "count"       then fn_count(args, result, err);
    elsif f.all = "conj"        then fn_conj(args, result, err);
    elsif f.all = "seq"         then fn_seq(args, result, err);
    elsif f.all = "meta"        then fn_meta(args, result, err);
    elsif f.all = "with-meta"   then fn_with_meta(args, result, err);
    elsif f.all = "atom"        then fn_atom(args, result, err);
    elsif f.all = "atom?"       then fn_atom_q(args, result, err);
    elsif f.all = "deref"       then fn_deref(args, result, err);
    elsif f.all = "reset!"      then fn_reset(args, result, err);
    else
      result := null;
    end if;
  end procedure eval_native_func;

  procedure define_core_function(e: inout env_ptr; func_name: in string) is
    variable sym: mal_val_ptr;
    variable fn: mal_val_ptr;
  begin
    new_symbol(func_name, sym);
    new_nativefn(func_name, fn);
    env_set(e, sym, fn);
  end procedure define_core_function;

  procedure define_core_functions(e: inout env_ptr) is
    variable is_eof: boolean;
    variable input_line, result, err: line;
    variable sym: mal_val_ptr;
    variable fn: mal_val_ptr;
    variable outer: env_ptr;
    variable repl_env: env_ptr;
  begin
    define_core_function(e, "=");
    define_core_function(e, "throw");
    define_core_function(e, "nil?");
    define_core_function(e, "true?");
    define_core_function(e, "false?");
    define_core_function(e, "string?");
    define_core_function(e, "symbol");
    define_core_function(e, "symbol?");
    define_core_function(e, "keyword");
    define_core_function(e, "keyword?");
    define_core_function(e, "pr-str");
    define_core_function(e, "str");
    define_core_function(e, "prn");
    define_core_function(e, "println");
    define_core_function(e, "read-string");
    define_core_function(e, "readline");
    define_core_function(e, "slurp");
    define_core_function(e, "<");
    define_core_function(e, "<=");
    define_core_function(e, ">");
    define_core_function(e, ">=");
    define_core_function(e, "+");
    define_core_function(e, "-");
    define_core_function(e, "*");
    define_core_function(e, "/");
    define_core_function(e, "time-ms");
    define_core_function(e, "list");
    define_core_function(e, "list?");
    define_core_function(e, "vector");
    define_core_function(e, "vector?");
    define_core_function(e, "hash-map");
    define_core_function(e, "map?");
    define_core_function(e, "assoc");
    define_core_function(e, "dissoc");
    define_core_function(e, "get");
    define_core_function(e, "contains?");
    define_core_function(e, "keys");
    define_core_function(e, "vals");
    define_core_function(e, "sequential?");
    define_core_function(e, "cons");
    define_core_function(e, "concat");
    define_core_function(e, "nth");
    define_core_function(e, "first");
    define_core_function(e, "rest");
    define_core_function(e, "empty?");
    define_core_function(e, "count");
    define_core_function(e, "apply");   -- implemented in the stepN_XXX files
    define_core_function(e, "map");     -- implemented in the stepN_XXX files
    define_core_function(e, "conj");
    define_core_function(e, "seq");
    define_core_function(e, "meta");
    define_core_function(e, "with-meta");
    define_core_function(e, "atom");
    define_core_function(e, "atom?");
    define_core_function(e, "deref");
    define_core_function(e, "reset!");
    define_core_function(e, "swap!");   -- implemented in the stepN_XXX files
  end procedure define_core_functions;

end package body core;
