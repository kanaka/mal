library STD;
use STD.textio.all;
library WORK;
use WORK.types.all;

package env is
  procedure new_env(e: out env_ptr; an_outer: inout env_ptr);
  procedure new_env(e: out env_ptr; an_outer: inout env_ptr; binds: inout mal_val_ptr; exprs: inout mal_val_ptr);
  procedure env_set(e: inout env_ptr; key: inout mal_val_ptr; val: inout mal_val_ptr);
  procedure env_get(e: inout env_ptr; key: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr);
end package env;

package body env is
  procedure new_env(e: out env_ptr; an_outer: inout env_ptr) is
    variable null_list: mal_val_ptr;
  begin
    null_list := null;
    new_env(e, an_outer, null_list, null_list);
  end procedure new_env;

  procedure new_env(e: out env_ptr; an_outer: inout env_ptr; binds: inout mal_val_ptr; exprs: inout mal_val_ptr) is
    variable the_data, more_exprs: mal_val_ptr;
    variable i: integer;
  begin
    new_empty_hashmap(the_data);
    if binds /= null then
      for i in binds.seq_val'range loop
        if binds.seq_val(i).string_val.all = "&" then
          seq_drop_prefix(exprs, i, more_exprs);
          hashmap_put(the_data, binds.seq_val(i + 1), more_exprs);
          exit;
        else
          hashmap_put(the_data, binds.seq_val(i), exprs.seq_val(i));
        end if;
      end loop;
    end if;
    e := new env_record'(outer => an_outer, data => the_data);
  end procedure new_env;

  procedure env_set(e: inout env_ptr; key: inout mal_val_ptr; val: inout mal_val_ptr) is
  begin
    hashmap_put(e.data, key, val);
  end procedure env_set;

  procedure env_find(e: inout env_ptr; key: inout mal_val_ptr; found_env: out env_ptr) is
    variable found: boolean;
  begin
    hashmap_contains(e.data, key, found);
    if found then
      found_env := e;
    else
      if e.outer = null then
        found_env := null;
      else
        env_find(e.outer, key, found_env);
      end if;
    end if;
  end procedure env_find;

  procedure env_get(e: inout env_ptr; key: inout mal_val_ptr; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable found_env: env_ptr;
  begin
    env_find(e, key, found_env);
    if found_env = null then
      new_string("'" & key.string_val.all & "' not found", err);
      result := null;
      return;
    end if;
    hashmap_get(found_env.data, key, result);
  end procedure env_get;

end package body env;
