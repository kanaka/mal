
-- general functions

CREATE FUNCTION mal_equal(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_equal_Q(args[1], args[2]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_throw(args integer[]) RETURNS integer AS $$
BEGIN
    -- TODO: Only throws strings. Without subtransactions, all changes
    -- to DB up to this point get rolled back so the object being
    -- thrown dissapears.
    RAISE EXCEPTION '%', pr_str(args[1], false);
END; $$ LANGUAGE plpgsql;


-- scalar functions

CREATE FUNCTION mal_nil_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_nil_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_true_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_true_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_false_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_false_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_string_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_string_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_symbol(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _symbolv(_valueToString(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_symbol_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_symbol_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_keyword(args integer[]) RETURNS integer AS $$
BEGIN
    IF _keyword_Q(args[1]) THEN
        RETURN args[1];
    ELSE
        RETURN _keywordv(_valueToString(args[1]));
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_keyword_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_keyword_Q(args[1]));
END; $$ LANGUAGE plpgsql;


-- string functions

CREATE FUNCTION mal_pr_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _stringv(pr_str_array(args, ' ', true));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _stringv(pr_str_array(args, '', false));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_prn(args integer[]) RETURNS integer AS $$
BEGIN
    PERFORM writeline(pr_str_array(args, ' ', true));
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_println(args integer[]) RETURNS integer AS $$
BEGIN
    PERFORM writeline(pr_str_array(args, ' ', false));
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_read_string(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN read_str(_valueToString(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_readline(args integer[]) RETURNS integer AS $$
DECLARE
    input  varchar;
BEGIN
    input := readline(_valueToString(args[1]));
    IF input IS NULL THEN
        RETURN 0;  -- nil
    END IF;
    RETURN _stringv(rtrim(input, E'\n'));
END; $$ LANGUAGE plpgsql;


-- See:
-- http://shuber.io/reading-from-the-filesystem-with-postgres/
CREATE FUNCTION mal_slurp(args integer[]) RETURNS integer AS $$
DECLARE
    fname    varchar;
    tmp      varchar;
    cmd      varchar;
    lines    varchar[];
    content  varchar;
BEGIN
    fname := _valueToString(args[1]);
    IF fname NOT LIKE '/%' THEN
        fname := _valueToString(env_vget(0, '*PWD*')) || '/' || fname;
    END IF;

    tmp := CAST(round(random()*1000000) AS varchar);

    EXECUTE format('CREATE TEMP TABLE %I (content text)', tmp);
    cmd := format('sed ''s/\\/\\\\/g'' %L', fname);
    EXECUTE format('COPY %I FROM PROGRAM %L', tmp, cmd);
    EXECUTE format('SELECT ARRAY(SELECT content FROM %I)', tmp) INTO lines;
    EXECUTE format('DROP TABLE %I', tmp);

    content := array_to_string(lines, E'\n') || E'\n';
    RETURN _stringv(content);
END; $$ LANGUAGE plpgsql;


-- number functions

-- integer comparison
CREATE FUNCTION mal_intcmp(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a bigint; b bigint; result boolean;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('SELECT $1 %s $2;', op) INTO result USING a, b;
    RETURN _wraptf(result);
END; $$ LANGUAGE plpgsql;

-- integer operation
CREATE FUNCTION mal_intop(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a bigint; b bigint; result bigint;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('SELECT $1 %s $2;', op) INTO result USING a, b;
    RETURN _numToValue(result);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_lt(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('<', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_lte(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('<=', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_gt(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('>', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_gte(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('>=', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_add(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('+', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_subtract(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('-', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_multiply(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('*', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_divide(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('/', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_time_ms(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _numToValue(
        CAST(date_part('epoch', clock_timestamp()) * 1000 AS bigint));
END; $$ LANGUAGE plpgsql;


-- collection functions

CREATE FUNCTION mal_list(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _list(args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_list_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_list_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_vector(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _vector(args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_vector_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_vector_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_hash_map(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _hash_map(args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_map_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_hash_map_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_assoc(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _assoc_BANG(_clone(args[1]), args[2:array_length(args, 1)]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_dissoc(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _dissoc_BANG(_clone(args[1]), args[2:array_length(args, 1)]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_get(args integer[]) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    IF _type(args[1]) = 0 THEN  -- nil
        RETURN 0;
    ELSE
        result := _get(args[1], _valueToString(args[2]));
        IF result IS NULL THEN RETURN 0; END IF;
        RETURN result;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_contains_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_contains_Q(args[1], _valueToString(args[2])));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_keys(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _list(_keys(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_vals(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _list(_vals(args[1]));
END; $$ LANGUAGE plpgsql;



-- sequence functions

CREATE FUNCTION mal_sequential_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_sequential_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_cons(args integer[]) RETURNS integer AS $$
DECLARE
    lst   integer[];
BEGIN
    lst := array_prepend(args[1], _valueToArray(args[2]));
    RETURN _list(lst);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_concat(args integer[]) RETURNS integer AS $$
DECLARE
    lst     integer;
    result  integer[] = ARRAY[]::integer[];
BEGIN
    FOREACH lst IN ARRAY args LOOP
        result := array_cat(result, _valueToArray(lst));
    END LOOP;
    RETURN _list(result);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_nth(args integer[]) RETURNS integer AS $$
DECLARE
    idx  integer;
BEGIN
    SELECT val_int INTO idx FROM value WHERE value_id = args[2];
    IF idx >= _count(args[1]) THEN
        RAISE EXCEPTION 'nth: index out of range';
    END IF;
    RETURN _nth(args[1], idx);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_first(args integer[]) RETURNS integer AS $$
BEGIN
    IF _nil_Q(args[1]) THEN
        RETURN 0; -- nil
    ELSIF _count(args[1]) = 0 THEN
        RETURN 0; -- nil
    ELSE
        RETURN _first(args[1]);
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_rest(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _rest(args[1]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_empty_Q(args integer[]) RETURNS integer AS $$
BEGIN
    IF _sequential_Q(args[1]) AND _count(args[1]) = 0 THEN
        RETURN 2;
    ELSE
        RETURN 1;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_count(args integer[]) RETURNS integer AS $$
BEGIN
    IF _sequential_Q(args[1]) THEN
        RETURN _numToValue(_count(args[1]));
    ELSIF _nil_Q(args[1]) THEN
        RETURN _numToValue(0);
    ELSE
        RAISE EXCEPTION 'count called on non-sequence';
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_apply(args integer[]) RETURNS integer AS $$
DECLARE
    alen   integer;
    fargs  integer[];
BEGIN
    alen := array_length(args, 1);
    fargs := array_cat(args[2:alen-1], _valueToArray(args[alen]));
    RETURN _apply(args[1], fargs);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_map(args integer[]) RETURNS integer AS $$
DECLARE
    x       integer;
    result  integer[];
BEGIN
    FOREACH x IN ARRAY _valueToArray(args[2])
    LOOP
        result := array_append(result, _apply(args[1], ARRAY[x]));
    END LOOP;
    return _list(result);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_conj(args integer[]) RETURNS integer AS $$
DECLARE
    type  integer;
BEGIN
    type := _type(args[1]);
    CASE
    WHEN type = 8 THEN -- list
        RETURN _list(array_cat(array_reverse(args[2:array_length(args, 1)]),
                               _valueToArray(args[1])));
    WHEN type = 9 THEN -- vector
        RETURN _vector(array_cat(_valueToArray(args[1]),
                                 args[2:array_length(args, 1)]));
    ELSE
        RAISE EXCEPTION 'conj: called on non-sequence';
    END CASE;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_seq(args integer[]) RETURNS integer AS $$
DECLARE
    type  integer;
    vid   integer;
    str   varchar;
    chr   varchar;
    seq   integer[];
BEGIN
    type := _type(args[1]);
    CASE
    WHEN type = 8 THEN -- list
        IF _count(args[1]) = 0 THEN RETURN 0; END IF; -- nil
        RETURN args[1];
    WHEN type = 9 THEN -- vector
        IF _count(args[1]) = 0 THEN RETURN 0; END IF; -- nil
        -- clone and modify to a list
        vid := _clone(args[1]);
        UPDATE value SET type_id = 8 WHERE value_id = vid;
        RETURN vid;
    WHEN type = 5 THEN -- string
        str := _valueToString(args[1]);
        IF char_length(str) = 0 THEN RETURN 0; END IF; -- nil
        FOREACH chr IN ARRAY regexp_split_to_array(str, '') LOOP
            seq := array_append(seq, _stringv(chr));
        END LOOP;
        RETURN _list(seq);
    WHEN type = 0 THEN -- nil
        RETURN 0; -- nil
    ELSE
        RAISE EXCEPTION 'seq: called on non-sequence';
    END CASE;
END; $$ LANGUAGE plpgsql;


-- meta functions

CREATE FUNCTION mal_meta(args integer[]) RETURNS integer AS $$
DECLARE
    m  integer;
BEGIN
    SELECT meta_id INTO m FROM value WHERE value_id = args[1];
    IF m IS NULL THEN
        RETURN 0;
    ELSE
        RETURN m;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_with_meta(args integer[]) RETURNS integer AS $$
DECLARE
    vid  integer;
BEGIN
    vid := _clone(args[1]);
    UPDATE value SET meta_id = args[2]
        WHERE value_id = vid;
    RETURN vid;
END; $$ LANGUAGE plpgsql;



-- atom functions

CREATE FUNCTION mal_atom(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _atom(args[1]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_atom_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_atom_Q(args[1]));
END; $$ LANGUAGE plpgsql;


CREATE FUNCTION mal_deref(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _deref(args[1]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_reset_BANG(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _reset_BANG(args[1], args[2]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_swap_BANG(args integer[]) RETURNS integer AS $$
DECLARE
    atm    integer;
    fargs  integer[];
BEGIN
    atm := args[1];
    fargs := array_cat(ARRAY[_deref(atm)], args[3:array_length(args, 1)]);
    RETURN _reset_BANG(atm, _apply(args[2], fargs));
END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------

-- repl_env is environment 0

INSERT INTO env (env_id, outer_id, data)
    VALUES (0, NULL, hstore(ARRAY[
        '=',           _function('mal_equal'),
        'throw',       _function('mal_throw'),

        'nil?',        _function('mal_nil_Q'),
        'true?',       _function('mal_true_Q'),
        'false?',      _function('mal_false_Q'),
        'string?',     _function('mal_string_Q'),
        'symbol',      _function('mal_symbol'),
        'symbol?',     _function('mal_symbol_Q'),
        'keyword',     _function('mal_keyword'),
        'keyword?',    _function('mal_keyword_Q'),

        'pr-str',      _function('mal_pr_str'),
        'str',         _function('mal_str'),
        'prn',         _function('mal_prn'),
        'println',     _function('mal_println'),
        'read-string', _function('mal_read_string'),
        'readline',    _function('mal_readline'),
        'slurp',       _function('mal_slurp'),

        '<',           _function('mal_lt'),
        '<=',          _function('mal_lte'),
        '>',           _function('mal_gt'),
        '>=',          _function('mal_gte'),
        '+',           _function('mal_add'),
        '-',           _function('mal_subtract'),
        '*',           _function('mal_multiply'),
        '/',           _function('mal_divide'),
        'time-ms',     _function('mal_time_ms'),

        'list',        _function('mal_list'),
        'list?',       _function('mal_list_Q'),
        'vector',      _function('mal_vector'),
        'vector?',     _function('mal_vector_Q'),
        'hash-map',    _function('mal_hash_map'),
        'map?',        _function('mal_map_Q'),
        'assoc',       _function('mal_assoc'),
        'dissoc',      _function('mal_dissoc'),
        'get',         _function('mal_get'),
        'contains?',   _function('mal_contains_Q'),
        'keys',        _function('mal_keys'),
        'vals',        _function('mal_vals'),

        'sequential?', _function('mal_sequential_Q'),
        'cons',        _function('mal_cons'),
        'concat',      _function('mal_concat'),
        'nth',         _function('mal_nth'),
        'first',       _function('mal_first'),
        'rest',        _function('mal_rest'),
        'empty?',      _function('mal_empty_Q'),
        'count',       _function('mal_count'),
        'apply',       _function('mal_apply'),
        'map',         _function('mal_map'),

        'conj',        _function('mal_conj'),
        'seq',         _function('mal_seq'),

        'meta',        _function('mal_meta'),
        'with-meta',   _function('mal_with_meta'),
        'atom',        _function('mal_atom'),
        'atom?',       _function('mal_atom_Q'),
        'deref',       _function('mal_deref'),
        'reset!',      _function('mal_reset_BANG'),
        'swap!',       _function('mal_swap_BANG')
        ]));
