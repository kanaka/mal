library STD;
use STD.textio.all;

package types is

  procedure debugline(l: inout line);
  procedure debug(str: in string);
  procedure debug(ch: in character);
  procedure debug(i: in integer);

  type mal_type_tag is (mal_nil, mal_true, mal_false, mal_number,
                        mal_symbol, mal_string, mal_keyword,
                        mal_list, mal_vector, mal_hashmap,
                        mal_atom, mal_nativefn, mal_fn);

  -- Forward declarations
  type mal_val;
  type mal_seq;
  type mal_func;
  type env_record;

  type mal_val_ptr is access mal_val;
  type mal_seq_ptr is access mal_seq;
  type mal_func_ptr is access mal_func;
  type env_ptr is access env_record;

  type mal_val is record
    val_type: mal_type_tag;
    number_val: integer;    -- For types: number
    string_val: line;       -- For types: symbol, string, keyword, nativefn
    seq_val: mal_seq_ptr;   -- For types: list, vector, hashmap, atom
    func_val: mal_func_ptr; -- For fn
    meta_val: mal_val_ptr;
  end record mal_val;

  type mal_seq is array (natural range <>) of mal_val_ptr;

  type mal_func is record
    f_body: mal_val_ptr;
    f_args: mal_val_ptr;
    f_env: env_ptr;
    f_is_macro: boolean;
  end record mal_func;

  type env_record is record
    outer: env_ptr;
    data: mal_val_ptr;
  end record env_record;

  procedure new_nil(obj: out mal_val_ptr);
  procedure new_true(obj: out mal_val_ptr);
  procedure new_false(obj: out mal_val_ptr);
  procedure new_boolean(b: in boolean; obj: out mal_val_ptr);
  procedure new_number(v: in integer; obj: out mal_val_ptr);
  procedure new_symbol(name: in string; obj: out mal_val_ptr);
  procedure new_symbol(name: inout line; obj: out mal_val_ptr);
  procedure new_string(name: in string; obj: out mal_val_ptr);
  procedure new_string(name: inout line; obj: out mal_val_ptr);
  procedure new_keyword(name: in string; obj: out mal_val_ptr);
  procedure new_keyword(name: inout line; obj: out mal_val_ptr);
  procedure new_nativefn(name: in string; obj: out mal_val_ptr);
  procedure new_fn(body_ast: inout mal_val_ptr; args: inout mal_val_ptr; env: inout env_ptr; obj: out mal_val_ptr);
  procedure new_seq_obj(seq_type: in mal_type_tag; seq: inout mal_seq_ptr; obj: out mal_val_ptr);
  procedure new_one_element_list(val: inout mal_val_ptr; obj: out mal_val_ptr);
  procedure new_empty_hashmap(obj: out mal_val_ptr);
  procedure new_atom(val: inout mal_val_ptr; obj: out mal_val_ptr);

  procedure hashmap_copy(hashmap: inout mal_val_ptr; obj: out mal_val_ptr);
  procedure hashmap_get(hashmap: inout mal_val_ptr; key: inout mal_val_ptr; val: out mal_val_ptr);
  procedure hashmap_contains(hashmap: inout mal_val_ptr; key: inout mal_val_ptr; ok: out boolean);
  procedure hashmap_put(hashmap: inout mal_val_ptr; key: inout mal_val_ptr; val: inout mal_val_ptr);
  procedure hashmap_delete(hashmap: inout mal_val_ptr; key: inout mal_val_ptr);
  procedure seq_drop_prefix(src: inout mal_val_ptr; prefix_length: in integer; result: out mal_val_ptr);
  function is_sequential_type(t: in mal_type_tag) return boolean;
  procedure equal_q(a: inout mal_val_ptr; b: inout mal_val_ptr; result: out boolean);
end package types;

package body types is

  procedure debugline(l: inout line) is
    variable l2: line;
  begin
    l2 := new string(1 to 7 + l'length);
    l2(1 to l2'length) := "DEBUG: " & l.all;
    writeline(output, l2);
  end procedure debugline;

  procedure debug(str: in string) is
    variable d: line;
  begin
    write(d, str);
    debugline(d);
  end procedure debug;

  procedure debug(ch: in character) is
    variable d: line;
  begin
    write(d, ch);
    debugline(d);
  end procedure debug;

  procedure debug(i: in integer) is
    variable d: line;
  begin
    write(d, i);
    debugline(d);
  end procedure debug;

  procedure new_nil(obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_nil, number_val => 0, string_val => null, seq_val => null, func_val => null, meta_val => null);
  end procedure new_nil;

  procedure new_true(obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_true, number_val => 0, string_val => null, seq_val => null, func_val => null, meta_val => null);
  end procedure new_true;

  procedure new_false(obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_false, number_val => 0, string_val => null, seq_val => null, func_val => null, meta_val => null);
  end procedure new_false;

  procedure new_boolean(b: in boolean; obj: out mal_val_ptr) is
  begin
    if b then
      new_true(obj);
    else
      new_false(obj);
    end if;
  end procedure new_boolean;

  procedure new_number(v: in integer; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_number, number_val => v, string_val => null, seq_val => null, func_val => null, meta_val => null);
  end procedure new_number;

  procedure new_symbol(name: in string; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_symbol, number_val => 0, string_val => new string'(name), seq_val => null, func_val => null, meta_val => null);
  end procedure new_symbol;

  procedure new_symbol(name: inout line; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_symbol, number_val => 0, string_val => name, seq_val => null, func_val => null, meta_val => null);
  end procedure new_symbol;

  procedure new_string(name: in string; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_string, number_val => 0, string_val => new string'(name), seq_val => null, func_val => null, meta_val => null);
  end procedure new_string;

  procedure new_string(name: inout line; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_string, number_val => 0, string_val => name, seq_val => null, func_val => null, meta_val => null);
  end procedure new_string;

  procedure new_keyword(name: in string; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_keyword, number_val => 0, string_val => new string'(name), seq_val => null, func_val => null, meta_val => null);
  end procedure new_keyword;

  procedure new_keyword(name: inout line; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_keyword, number_val => 0, string_val => name, seq_val => null, func_val => null, meta_val => null);
  end procedure new_keyword;

  procedure new_nativefn(name: in string; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => mal_nativefn, number_val => 0, string_val => new string'(name), seq_val => null, func_val => null, meta_val => null);
  end procedure new_nativefn;

  procedure new_fn(body_ast: inout mal_val_ptr; args: inout mal_val_ptr; env: inout env_ptr; obj: out mal_val_ptr) is
    variable f: mal_func_ptr;
  begin
    f := new mal_func'(f_body => body_ast, f_args => args, f_env => env, f_is_macro => false);
    obj := new mal_val'(val_type => mal_fn, number_val => 0, string_val => null, seq_val => null, func_val => f, meta_val => null);
  end procedure new_fn;

  procedure new_seq_obj(seq_type: in mal_type_tag; seq: inout mal_seq_ptr; obj: out mal_val_ptr) is
  begin
    obj := new mal_val'(val_type => seq_type, number_val => 0, string_val => null, seq_val => seq, func_val => null, meta_val => null);
  end procedure new_seq_obj;

  procedure new_one_element_list(val: inout mal_val_ptr; obj: out mal_val_ptr) is
    variable seq: mal_seq_ptr;
  begin
    seq := new mal_seq(0 to 0);
    seq(0) := val;
    new_seq_obj(mal_list, seq, obj);
  end procedure new_one_element_list;

  procedure new_empty_hashmap(obj: out mal_val_ptr) is
    variable seq: mal_seq_ptr;
  begin
    seq := new mal_seq(0 to -1);
    new_seq_obj(mal_hashmap, seq, obj);
  end procedure new_empty_hashmap;

  procedure new_atom(val: inout mal_val_ptr; obj: out mal_val_ptr) is
    variable atom_seq: mal_seq_ptr;
  begin
    atom_seq := new mal_seq(0 to 0);
    atom_seq(0) := val;
    new_seq_obj(mal_atom, atom_seq, obj);
  end procedure new_atom;

  procedure hashmap_copy(hashmap: inout mal_val_ptr; obj: out mal_val_ptr) is
    variable new_seq: mal_seq_ptr;
  begin
    new_seq := new mal_seq(hashmap.seq_val'range);
    new_seq(new_seq'range) := hashmap.seq_val(hashmap.seq_val'range);
    new_seq_obj(mal_hashmap, new_seq, obj);
  end procedure hashmap_copy;

  procedure hashmap_get(hashmap: inout mal_val_ptr; key: inout mal_val_ptr; val: out mal_val_ptr) is
    variable i: natural;
    variable curr_key: mal_val_ptr;
  begin
    i := 0;
    while i < hashmap.seq_val'length loop
      curr_key := hashmap.seq_val(i);
      if key.val_type = curr_key.val_type and key.string_val.all = curr_key.string_val.all then
        val := hashmap.seq_val(i + 1);
        return;
      end if;
      i := i + 2;
    end loop;
    val := null;
  end procedure hashmap_get;

  procedure hashmap_contains(hashmap: inout mal_val_ptr; key: inout mal_val_ptr; ok: out boolean) is
    variable val: mal_val_ptr;
  begin
    hashmap_get(hashmap, key, val);
    if val = null then
      ok := false;
    else
      ok := true;
    end if;
  end procedure hashmap_contains;

  procedure hashmap_put(hashmap: inout mal_val_ptr; key: inout mal_val_ptr; val: inout mal_val_ptr) is
    variable i: natural;
    variable curr_key: mal_val_ptr;
    variable new_seq: mal_seq_ptr;
  begin
    i := 0;
    while i < hashmap.seq_val'length loop
      curr_key := hashmap.seq_val(i);
      if key.val_type = curr_key.val_type and key.string_val.all = curr_key.string_val.all then
        hashmap.seq_val(i + 1) := val;
        return;
      end if;
      i := i + 2;
    end loop;
    -- Not found so far, need to extend the seq
    new_seq := new mal_seq(0 to hashmap.seq_val'length + 1);
    for i in hashmap.seq_val'range loop
      new_seq(i) := hashmap.seq_val(i);
    end loop;
    new_seq(new_seq'length - 2) := key;
    new_seq(new_seq'length - 1) := val;
    deallocate(hashmap.seq_val);
    hashmap.seq_val := new_seq;
  end procedure hashmap_put;

  procedure hashmap_delete(hashmap: inout mal_val_ptr; key: inout mal_val_ptr) is
    variable i, dst_i: natural;
    variable curr_key: mal_val_ptr;
    variable new_seq: mal_seq_ptr;
    variable found: boolean;
  begin
    hashmap_contains(hashmap, key, found);
    if not found then
      return;
    end if;
    i := 0;
    dst_i := 0;
    new_seq := new mal_seq(0 to hashmap.seq_val'high - 2);
    while i < hashmap.seq_val'length loop
      curr_key := hashmap.seq_val(i);
      if key.val_type = curr_key.val_type and key.string_val.all = curr_key.string_val.all then
        i := i + 2;
      else
        new_seq(dst_i to dst_i + 1) := hashmap.seq_val(i to i + 1);
        dst_i := dst_i + 2;
        i := i + 2;
      end if;
    end loop;
    deallocate(hashmap.seq_val);
    hashmap.seq_val := new_seq;
  end procedure hashmap_delete;

  procedure seq_drop_prefix(src: inout mal_val_ptr; prefix_length: in integer; result: out mal_val_ptr) is
    variable seq: mal_seq_ptr;
  begin
    seq := new mal_seq(0 to src.seq_val'length - 1 - prefix_length);
    for i in seq'range loop
      seq(i) := src.seq_val(i + prefix_length);
    end loop;
    new_seq_obj(src.val_type, seq, result);
  end procedure seq_drop_prefix;

  function is_sequential_type(t: in mal_type_tag) return boolean is
  begin
    return t = mal_list or t = mal_vector;
  end function is_sequential_type;

  procedure equal_seq_q(a: inout mal_val_ptr; b: inout mal_val_ptr; result: out boolean) is
    variable i: integer;
    variable is_element_equal: boolean;
  begin
    if a.seq_val'length = b.seq_val'length then
      for i in a.seq_val'range loop
        equal_q(a.seq_val(i), b.seq_val(i), is_element_equal);
        if not is_element_equal then
          result := false;
          return;
        end if;
      end loop;
      result := true;
    else
      result := false;
    end if;
  end procedure equal_seq_q;

  procedure equal_hashmap_q(a: inout mal_val_ptr; b: inout mal_val_ptr; result: out boolean) is
    variable i: integer;
    variable is_value_equal: boolean;
    variable b_val: mal_val_ptr;
  begin
    if a.seq_val'length = b.seq_val'length then
      i := 0;
      while i < a.seq_val'length loop
        hashmap_get(b, a.seq_val(i), b_val);
        if b_val = null then
          result := false;
          return;
        else
          equal_q(a.seq_val(i + 1), b_val, is_value_equal);
          if not is_value_equal then
            result := false;
            return;
          end if;
        end if;
        i := i + 2;
      end loop;
      result := true;
    else
      result := false;
    end if;
  end procedure equal_hashmap_q;

  procedure equal_q(a: inout mal_val_ptr; b: inout mal_val_ptr; result: out boolean) is
  begin
    if is_sequential_type(a.val_type) and is_sequential_type(b.val_type) then
      equal_seq_q(a, b, result);
    elsif a.val_type = b.val_type then
      case a.val_type is
        when mal_nil | mal_true | mal_false =>
          result := true;
        when mal_number =>
          result := a.number_val = b.number_val;
        when mal_symbol | mal_string | mal_keyword =>
          result := a.string_val.all = b.string_val.all;
        when mal_hashmap =>
          equal_hashmap_q(a, b, result);
        when mal_atom =>
          equal_q(a.seq_val(0), b.seq_val(0), result);
        when others =>
          result := false;
      end case;
    else
      result := false;
    end if;
  end procedure equal_q;
end package body types;
