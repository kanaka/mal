library STD;
use STD.textio.all;
library WORK;
use WORK.types.all;

package reader is
  procedure read_str(s: in string; result: out mal_val_ptr; err: out mal_val_ptr);
end package reader;

package body reader is

  type token_list is array(natural range <>) of line;
  type token_list_ptr is access token_list;

  function is_eol_char(c: in character) return boolean is
  begin
    case c is
      when LF | CR => return true;
      when others => return false;
    end case;
  end function is_eol_char;

  function is_separator_char(c: in character) return boolean is
  begin
    case c is
      when LF | CR | ' ' | '[' | ']' | '{' | '}' | '(' | ')' |
           ''' | '"' | '`' | ',' | ';' => return true;
      when others => return false;
    end case;
  end function is_separator_char;

  procedure next_token(str: in string; pos: in positive; token: inout line; next_start_pos: out positive; ok: out boolean) is
    variable ch: character;
    variable tmppos: positive;
  begin
    token := new string'("");
    if pos > str'length then
      ok := false;
      return;
    end if;

    ch := str(pos);

    case ch is
      when ' ' | ',' | LF | CR | HT =>
        next_start_pos := pos + 1;
        token := new string'("");
        ok := true;
        return;

      when '[' | ']' | '{' | '}' | '(' | ')' | ''' | '`' | '^' | '@' =>
        next_start_pos := pos + 1;
        token := new string'("" & ch);
        ok := true;
        return;

      when '~' =>
        if str(pos + 1) = '@' then
          next_start_pos := pos + 2;
          token := new string'("~@");
        else
          next_start_pos := pos + 1;
          token := new string'("~");
        end if;
        ok := true;
        return;

      when ';' =>
        tmppos := pos + 1;
        while tmppos <= str'length and not is_eol_char(str(tmppos)) loop
          tmppos := tmppos + 1;
        end loop;
        next_start_pos := tmppos;
        token := new string'("");
        ok := true;
        return;

      when '"' =>
        tmppos := pos + 1;
        while tmppos < str'length and str(tmppos) /= '"' loop
          if str(tmppos) = '\' then
            tmppos := tmppos + 2;
          else
            tmppos := tmppos + 1;
          end if;
        end loop;
        token := new string(1 to (tmppos - pos + 1));
        token(1 to (tmppos - pos + 1)) := str(pos to tmppos);
        next_start_pos := tmppos + 1;
        ok := true;
        return;

      when others =>
        tmppos := pos;
        while tmppos <= str'length and not is_separator_char(str(tmppos)) loop
          tmppos := tmppos + 1;
        end loop;
        token := new string(1 to (tmppos - pos));
        token(1 to (tmppos - pos)) := str(pos to tmppos - 1);
        next_start_pos := tmppos;
        ok := true;
        return;

    end case;

    ok := false;
  end procedure next_token;

  function tokenize(str: in string) return token_list_ptr is
    variable next_pos: positive := 1;
    variable ok: boolean := true;
    variable tokens: token_list_ptr;
    variable t: line;
  begin
    while ok loop
      next_token(str, next_pos, t, next_pos, ok);
      if t'length > 0 then
        if tokens = null then
          tokens := new token_list(0 to 0);
          tokens(0) := t;
        else
          tokens := new token_list'(tokens.all & t);
        end if;
      end if;
    end loop;
    return tokens;
  end function tokenize;

  type reader_class is record
    tokens: token_list_ptr;
    pos: natural;
  end record reader_class;

  procedure reader_new(r: inout reader_class; a_tokens: inout token_list_ptr) is
  begin
    r := (tokens => a_tokens, pos => 0);
  end procedure reader_new;

  procedure reader_peek(r: inout reader_class; token: out line) is
  begin
    if r.pos < r.tokens'length then
      token := r.tokens(r.pos);
    else
      token := null;
    end if;
  end procedure reader_peek;

  procedure reader_next(r: inout reader_class; token: out line) is
  begin
    reader_peek(r, token);
    r.pos := r.pos + 1;
  end procedure reader_next;

  -- Forward declaration
  procedure read_form(r: inout reader_class; result: out mal_val_ptr; err: out mal_val_ptr);

  function is_digit(c: in character) return boolean is
  begin
    case c is
      when '0' to '9' => return true;
      when others => return false;
    end case;
  end function is_digit;

  function unescape_char(c: in character) return character is
  begin
    case c is
      when 'n' => return LF;
      when others => return c;
    end case;
  end function unescape_char;

  procedure unescape_string_token(token: inout line; result: out line) is
    variable s: line;
    variable src_i, dst_i: integer;
  begin
    s := new string(1 to token'length);
    dst_i := 0;
    src_i := 2; -- skip the initial quote
    while src_i <= token'length - 1 loop
      dst_i := dst_i + 1;
      if token(src_i) = '\' then
        s(dst_i) := unescape_char(token(src_i + 1));
        src_i := src_i + 2;
      else
        s(dst_i) := token(src_i);
        src_i := src_i + 1;
      end if;
    end loop;
    result := new string'(s(1 to dst_i));
    deallocate(s);
  end procedure unescape_string_token;

  procedure read_atom(r: inout reader_class; result: out mal_val_ptr) is
    variable token, s: line;
    variable num: integer;
    variable ch: character;
  begin
    reader_next(r, token);
    if token.all = "nil" then
      new_nil(result);
    elsif token.all = "true" then
      new_true(result);
    elsif token.all = "false" then
      new_false(result);
    else
      ch := token(1);
      case ch is
        when '-' =>
          if token'length > 1 and is_digit(token(2)) then
            read(token, num);
            new_number(num, result);
          else
            new_symbol(token, result);
          end if;
        when '0' to '9' =>
          read(token, num);
          new_number(num, result);
        when ':' =>
          s := new string(1 to token'length - 1);
          s(1 to s'length) := token(2 to token'length);
          new_keyword(s, result);
        when '"' =>
          unescape_string_token(token, s);
          new_string(s, result);
        when others =>
          new_symbol(token, result);
      end case;
    end if;
  end procedure read_atom;

  procedure read_sequence(list_type: in mal_type_tag; end_ch: in string; r: inout reader_class; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable token: line;
    variable element, sub_err: mal_val_ptr;
    variable seq: mal_seq_ptr;
  begin
    reader_next(r, token); -- Consume the open paren
    reader_peek(r, token);
    seq := new mal_seq(0 to -1);
    while token /= null and token.all /= end_ch loop
      read_form(r, element, sub_err);
      if sub_err /= null then
        err := sub_err;
        result := null;
        return;
      end if;
      seq := new mal_seq'(seq.all & element);
      reader_peek(r, token);
    end loop;
    if token = null then
      new_string("expected '" & end_ch & "', got EOF", err);
      result := null;
      return;
    end if;
    reader_next(r, token); -- Consume the close paren
    new_seq_obj(list_type, seq, result);
  end procedure read_sequence;

  procedure reader_macro(r: inout reader_class; result: out mal_val_ptr; err: out mal_val_ptr; sym_name: in string) is
    variable token, sym_line: line;
    variable seq: mal_seq_ptr;
    variable rest, rest_err: mal_val_ptr;
  begin
    reader_next(r, token);
    seq := new mal_seq(0 to 1);
    sym_line := new string'(sym_name);
    new_symbol(sym_line, seq(0));
    read_form(r, rest, rest_err);
    if rest_err /= null then
      err := rest_err;
      result := null;
      return;
    end if;
    seq(1) := rest;
    new_seq_obj(mal_list, seq, result);
  end procedure reader_macro;

  procedure with_meta_reader_macro(r: inout reader_class; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable token, sym_line: line;
    variable seq: mal_seq_ptr;
    variable meta, rest, rest_err: mal_val_ptr;
  begin
    reader_next(r, token);
    seq := new mal_seq(0 to 2);
    sym_line := new string'("with-meta");
    new_symbol(sym_line, seq(0));
    read_form(r, meta, rest_err);
    if rest_err /= null then
      err := rest_err;
      result := null;
      return;
    end if;
    read_form(r, rest, rest_err);
    if rest_err /= null then
      err := rest_err;
      result := null;
      return;
    end if;
    seq(1) := rest;
    seq(2) := meta;
    new_seq_obj(mal_list, seq, result);
  end procedure with_meta_reader_macro;

  procedure read_form(r: inout reader_class; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable token: line;
    variable ch: character;
  begin
    reader_peek(r, token);
    ch := token(1);
    case ch is
      when ''' => reader_macro(r, result, err, "quote");
      when '`' => reader_macro(r, result, err, "quasiquote");
      when '~' =>
        if token'length = 1 then
          reader_macro(r, result, err, "unquote");
        else
          if token(2) = '@' then
            reader_macro(r, result, err, "splice-unquote");
          else
            new_string("Unknown token", err);
          end if;
        end if;
      when '^' => with_meta_reader_macro(r, result, err);
      when '@' => reader_macro(r, result, err, "deref");
      when '(' => read_sequence(mal_list, ")", r, result, err);
      when ')' => new_string("unexcepted ')'", err);
      when '[' => read_sequence(mal_vector, "]", r, result, err);
      when ']' => new_string("unexcepted ']'", err);
      when '{' => read_sequence(mal_hashmap, "}", r, result, err);
      when '}' => new_string("unexcepted '}'", err);
      when others => read_atom(r, result);
    end case;
  end procedure read_form;

  procedure read_str(s: in string; result: out mal_val_ptr; err: out mal_val_ptr) is
    variable tokens: token_list_ptr;
    variable r: reader_class;
  begin
    tokens := tokenize(s);
    if tokens = null or tokens'length = 0 then
      result := null;
      err := null;
      return;
    end if;
    reader_new(r, tokens);
    read_form(r, result, err);
  end procedure read_str;

end package body reader;
