
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
BEGIN
    RETURN _stringv(rtrim(readline(_valueToString(args[1])), E'\n'));
END; $$ LANGUAGE plpgsql;


-- See:
-- http://shuber.io/reading-from-the-filesystem-with-postgres/
CREATE FUNCTION mal_slurp(args integer[]) RETURNS integer AS $$
DECLARE
    fname    varchar;
    tmp      varchar;
    lines    varchar[];
    content  varchar;
BEGIN
    fname := _valueToString(args[1]);
    IF fname NOT LIKE '/%' THEN
        fname := _valueToString(env_vget(0, '*PWD*')) || '/' || fname;
    END IF;

    tmp := CAST(round(random()*1000000) AS varchar);

    EXECUTE format('CREATE TEMP TABLE %I (content text)', tmp);
    EXECUTE format('COPY %I FROM %L', tmp, fname);
    EXECUTE format('SELECT ARRAY(SELECT content FROM %I)', tmp) INTO lines;
    EXECUTE format('DROP TABLE %I', tmp);

    content := array_to_string(lines, E'\n') || E'\n';
    RETURN _stringv(content);
END; $$ LANGUAGE plpgsql;


-- number functions

-- integer comparison
CREATE FUNCTION mal_intcmp(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a integer; b integer; result boolean;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('SELECT $1 %s $2;', op) INTO result USING a, b;
    RETURN _wraptf(result);
END; $$ LANGUAGE plpgsql;

-- integer operation
CREATE FUNCTION mal_intop(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a integer; b integer; result integer;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('INSERT INTO value (type_id, val_int) VALUES (3, $1 %s $2)
                    RETURNING value_id;', op) INTO result USING a, b;
    RETURN result;
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


-- collection functions

CREATE FUNCTION mal_list(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _collection(args, 8);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_list_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_list_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_vector(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _collection(args, 9);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_vector_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_vector_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_hash_map(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _collection(args, 10);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_map_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_hash_map_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_assoc(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _append(_clone(args[1]), args[2:array_length(args, 1)]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_dissoc(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _dissoc(_clone(args[1]), args[2:array_length(args, 1)]);
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
    RETURN _collection(_keys(args[1]), 8);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_vals(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _collection(_vals(args[1]), 8);
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
    RETURN _collection(lst, 8);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION mal_concat(args integer[]) RETURNS integer AS $$
DECLARE
    lst     integer;
    result  integer[];
BEGIN
    FOREACH lst IN ARRAY args
    LOOP
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
    return _collection(result, 8);
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

CREATE FUNCTION core_def(VARIADIC kvs varchar[]) RETURNS void AS $$
DECLARE
    idx  integer;
    vid  integer;
BEGIN
    idx := 1;
    WHILE idx < array_length(kvs, 1)
    LOOP
        INSERT INTO value (type_id, function_name) VALUES (11, kvs[idx+1])
            RETURNING value_id INTO vid;
        INSERT INTO env_data (env_id, key, value_id) VALUES (0, kvs[idx], vid);
        idx := idx + 2;
    END LOOP;
END; $$ LANGUAGE plpgsql;


-- repl_env is environment 0

INSERT INTO env (env_id, outer_id) VALUES (0, NULL);

-- core namespace

SELECT core_def(
    '=',           'mal_equal',
    'throw',       'mal_throw',

    'nil?',        'mal_nil_Q',
    'true?',       'mal_true_Q',
    'false?',      'mal_false_Q',
    'string?',     'mal_string_Q',
    'symbol',      'mal_symbol',
    'symbol?',     'mal_symbol_Q',
    'keyword',     'mal_keyword',
    'keyword?',    'mal_keyword_Q',

    'pr-str',      'mal_pr_str',
    'str',         'mal_str',
    'prn',         'mal_prn',
    'println',     'mal_println',
    'read-string', 'mal_read_string',
    'readline',    'mal_readline',
    'slurp',       'mal_slurp',

    '<',           'mal_lt',
    '<=',          'mal_lte',
    '>',           'mal_gt',
    '>=',          'mal_gte',
    '+',           'mal_add',
    '-',           'mal_subtract',
    '*',           'mal_multiply',
    '/',           'mal_divide');

-- split since calls are limited to 100 arguments
SELECT core_def(
    'list',        'mal_list',
    'list?',       'mal_list_Q',
    'vector',      'mal_vector',
    'vector?',     'mal_vector_Q',
    'hash-map',    'mal_hash_map',
    'map?',        'mal_map_Q',
    'assoc',       'mal_assoc',
    'dissoc',      'mal_dissoc',
    'get',         'mal_get',
    'contains?',   'mal_contains_Q',
    'keys',        'mal_keys',
    'vals',        'mal_vals',

    'sequential?', 'mal_sequential_Q',
    'cons',        'mal_cons',
    'concat',      'mal_concat',
    'nth',         'mal_nth',
    'first',       'mal_first',
    'rest',        'mal_rest',
    'empty?',      'mal_empty_Q',
    'count',       'mal_count',
    'apply',       'mal_apply',
    'map',         'mal_map',

    'meta',        'mal_meta',
    'with-meta',   'mal_with_meta',
    'atom',        'mal_atom',
    'atom?',       'mal_atom_Q',
    'deref',       'mal_deref',
    'reset!',      'mal_reset_BANG',
    'swap!',       'mal_swap_BANG');
