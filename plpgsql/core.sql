
-- general functions

CREATE OR REPLACE FUNCTION mal_equal(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_equal_Q(args[1], args[2]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_equal');

CREATE OR REPLACE FUNCTION mal_throw(args integer[]) RETURNS integer AS $$
BEGIN
    -- TODO: Only throws strings. Without subtransactions, all changes
    -- to DB up to this point get rolled back so the object being
    -- thrown dissapears.
    RAISE EXCEPTION '%', pr_str(args[1], false);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_throw');


-- scalar functions

CREATE OR REPLACE FUNCTION mal_nil_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_nil_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_nil_Q');

CREATE OR REPLACE FUNCTION mal_false_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_false_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_false_Q');

CREATE OR REPLACE FUNCTION mal_true_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_true_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_true_Q');

CREATE OR REPLACE FUNCTION mal_symbol(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _symbolv(_vstring(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_symbol');

CREATE OR REPLACE FUNCTION mal_symbol_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_symbol_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_symbol_Q');

CREATE OR REPLACE FUNCTION mal_keyword(args integer[]) RETURNS integer AS $$
BEGIN
    IF _keyword_Q(args[1]) THEN
        RETURN args[1];
    ELSE
        RETURN _keywordv(_vstring(args[1]));
    END IF;
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_keyword');

CREATE OR REPLACE FUNCTION mal_keyword_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_keyword_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_keyword_Q');


-- string functions

CREATE OR REPLACE FUNCTION mal_pr_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _stringv(pr_str_array(args, ' ', true));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_pr_str');

CREATE OR REPLACE FUNCTION mal_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _stringv(pr_str_array(args, '', false));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_str');

CREATE OR REPLACE FUNCTION mal_prn(args integer[]) RETURNS integer AS $$
BEGIN
    --RAISE NOTICE '%', pr_str_array(args, ' ', true);
    PERFORM writeline(pr_str_array(args, ' ', true));
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_prn');

CREATE OR REPLACE FUNCTION mal_println(args integer[]) RETURNS integer AS $$
BEGIN
    --RAISE NOTICE '%', pr_str_array(args, ' ', false);
    PERFORM writeline(pr_str_array(args, ' ', false));
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_println');

CREATE OR REPLACE FUNCTION mal_read_string(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN read_str(_vstring(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_read_string');

-- See:
-- http://shuber.io/reading-from-the-filesystem-with-postgres/
CREATE OR REPLACE FUNCTION mal_slurp(args integer[]) RETURNS integer AS $$
DECLARE
    fname    varchar;
    tmp      varchar;
    lines    varchar[];
    content  varchar;
BEGIN
    fname := _vstring(args[1]);
    IF fname NOT LIKE '/%' THEN
        fname := _vstring(env_vget(0, '*PWD*')) || '/' || fname;
    END IF;

    tmp := CAST(round(random()*1000000) AS varchar);

    EXECUTE format('CREATE TEMP TABLE %I (content text)', tmp);
    EXECUTE format('COPY %I FROM %L', tmp, fname);
    EXECUTE format('SELECT ARRAY(SELECT content FROM %I)', tmp) INTO lines;
    EXECUTE format('DROP TABLE %I', tmp);

    content := array_to_string(lines, E'\n') || E'\n';
    RETURN _stringv(content);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_slurp');


-- number functions

-- integer comparison
CREATE OR REPLACE FUNCTION mal_intcmp(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a integer; b integer; result boolean;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('SELECT $1 %s $2;', op) INTO result USING a, b;
    RETURN _wraptf(result);
END; $$ LANGUAGE plpgsql;

-- integer operation
CREATE OR REPLACE FUNCTION mal_intop(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a integer; b integer; result integer;
BEGIN
    SELECT val_int INTO a FROM value WHERE value_id = args[1];
    SELECT val_int INTO b FROM value WHERE value_id = args[2];
    EXECUTE format('INSERT INTO value (type_id, val_int) VALUES (3, $1 %s $2)
                    RETURNING value_id;', op) INTO result USING a, b;
    RETURN result;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION mal_lt(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('<', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_lt');

CREATE OR REPLACE FUNCTION mal_lte(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('<=', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_lte');

CREATE OR REPLACE FUNCTION mal_gt(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('>', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_gt');

CREATE OR REPLACE FUNCTION mal_gte(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intcmp('>=', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_gte');

CREATE OR REPLACE FUNCTION mal_add(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('+', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_add');

CREATE OR REPLACE FUNCTION mal_subtract(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('-', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_subtract');

CREATE OR REPLACE FUNCTION mal_multiply(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('*', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_multiply');

CREATE OR REPLACE FUNCTION mal_divide(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN mal_intop('/', args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_divide');


-- collection functions

CREATE OR REPLACE FUNCTION mal_list(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _seq(args, 8);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_list');

CREATE OR REPLACE FUNCTION mal_list_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_list_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_list_Q');

CREATE OR REPLACE FUNCTION mal_vector(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _seq(args, 9);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_vector');

CREATE OR REPLACE FUNCTION mal_vector_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_vector_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_vector_Q');


-- sequence functions

CREATE OR REPLACE FUNCTION mal_sequential_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_sequential_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_sequential_Q');

CREATE OR REPLACE FUNCTION mal_cons(args integer[]) RETURNS integer AS $$
DECLARE
    lst   integer[];
BEGIN
    lst := array_prepend(args[1], _valueToArray(args[2]));
    RETURN _seq(lst, 8);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_cons');

CREATE OR REPLACE FUNCTION mal_concat(args integer[]) RETURNS integer AS $$
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
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_concat');

CREATE OR REPLACE FUNCTION mal_nth(args integer[]) RETURNS integer AS $$
DECLARE
    idx  integer;
BEGIN
    SELECT val_int INTO idx FROM value WHERE value_id = args[2];
    IF idx >= _count(args[1]) THEN
        RAISE EXCEPTION 'nth: index out of range';
    END IF;
    RETURN _nth(args[1], idx);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_nth');

CREATE OR REPLACE FUNCTION mal_first(args integer[]) RETURNS integer AS $$
BEGIN
    IF _nil_Q(args[1]) THEN
        RETURN 0; -- nil
    ELSIF _count(args[1]) = 0 THEN
        RETURN 0; -- nil
    ELSE
        RETURN _first(args[1]);
    END IF;
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_first');

CREATE OR REPLACE FUNCTION mal_rest(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _rest(args[1]);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_rest');

CREATE OR REPLACE FUNCTION mal_empty_Q(args integer[]) RETURNS integer AS $$
BEGIN
    IF _sequential_Q(args[1]) AND _count(args[1]) = 0 THEN
        RETURN 2;
    ELSE
        RETURN 1;
    END IF;
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_empty_Q');

CREATE OR REPLACE FUNCTION mal_count(args integer[]) RETURNS integer AS $$
BEGIN
    IF _sequential_Q(args[1]) THEN
        RETURN _numToValue(_count(args[1]));
    ELSIF _nil_Q(args[1]) THEN
        RETURN _numToValue(0);
    ELSE
        RAISE EXCEPTION 'count called on non-sequence';
    END IF;
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_count');

CREATE OR REPLACE FUNCTION mal_apply(args integer[]) RETURNS integer AS $$
DECLARE
    alen   integer;
    fargs  integer[];
BEGIN
    alen := array_length(args, 1);
    fargs := array_cat(args[2:alen-1], _valueToArray(args[alen]));
    RETURN _apply(args[1], fargs);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_apply');

CREATE OR REPLACE FUNCTION mal_map(args integer[]) RETURNS integer AS $$
DECLARE
    x       integer;
    result  integer[];
BEGIN
    FOREACH x IN ARRAY _valueToArray(args[2])
    LOOP
        result := array_append(result, _apply(args[1], ARRAY[x]));
    END LOOP;
    return _seq(result, 8);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_map');


-- atom functions

CREATE OR REPLACE FUNCTION mal_atom(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _atom(args[1]);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_atom');

-- atom functions
CREATE OR REPLACE FUNCTION mal_atom_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_atom_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_atom_Q');


CREATE OR REPLACE FUNCTION mal_deref(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _deref(args[1]);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_deref');

CREATE OR REPLACE FUNCTION mal_reset_BANG(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _reset_BANG(args[1], args[2]);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_reset_BANG');

CREATE OR REPLACE FUNCTION mal_swap_BANG(args integer[]) RETURNS integer AS $$
DECLARE
    atm    integer;
    fargs  integer[];
BEGIN
    atm := args[1];
    fargs := array_cat(ARRAY[_deref(atm)], args[3:array_length(args, 1)]);
    RETURN _reset_BANG(atm, _apply(args[2], fargs));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_swap_BANG');


-- repl_env is environment 0

INSERT INTO env (env_id, outer_id) VALUES (0, NULL);


-- core namespace

INSERT INTO env_data (env_id, key, value_id) VALUES
    (0, '=',  (SELECT value_id FROM value WHERE function_name = 'mal_equal')),
    (0, 'throw',  (SELECT value_id FROM value WHERE function_name = 'mal_throw')),

    (0, 'nil?', (SELECT value_id FROM value WHERE function_name = 'mal_nil_Q')),
    (0, 'false?', (SELECT value_id FROM value WHERE function_name = 'mal_false_Q')),
    (0, 'true?', (SELECT value_id FROM value WHERE function_name = 'mal_true_Q')),
    (0, 'symbol', (SELECT value_id FROM value WHERE function_name = 'mal_symbol')),
    (0, 'symbol?', (SELECT value_id FROM value WHERE function_name = 'mal_symbol_Q')),
    (0, 'keyword', (SELECT value_id FROM value WHERE function_name = 'mal_keyword')),
    (0, 'keyword?', (SELECT value_id FROM value WHERE function_name = 'mal_keyword_Q')),

    (0, 'pr-str',  (SELECT value_id FROM value WHERE function_name = 'mal_pr_str')),
    (0, 'str',  (SELECT value_id FROM value WHERE function_name = 'mal_str')),
    (0, 'prn',  (SELECT value_id FROM value WHERE function_name = 'mal_prn')),
    (0, 'println',  (SELECT value_id FROM value WHERE function_name = 'mal_println')),
    (0, 'read-string',  (SELECT value_id FROM value WHERE function_name = 'mal_read_string')),
    (0, 'slurp',  (SELECT value_id FROM value WHERE function_name = 'mal_slurp')),

    (0, '<',  (SELECT value_id FROM value WHERE function_name = 'mal_lt')),
    (0, '<=', (SELECT value_id FROM value WHERE function_name = 'mal_lte')),
    (0, '>',  (SELECT value_id FROM value WHERE function_name = 'mal_gt')),
    (0, '>=', (SELECT value_id FROM value WHERE function_name = 'mal_gte')),
    (0, '+',  (SELECT value_id FROM value WHERE function_name = 'mal_add')),
    (0, '-',  (SELECT value_id FROM value WHERE function_name = 'mal_subtract')),
    (0, '*',  (SELECT value_id FROM value WHERE function_name = 'mal_multiply')),
    (0, '/',  (SELECT value_id FROM value WHERE function_name = 'mal_divide')),

    (0, 'list', (SELECT value_id FROM value WHERE function_name = 'mal_list')),
    (0, 'list?', (SELECT value_id FROM value WHERE function_name = 'mal_list_Q')),
    (0, 'vector', (SELECT value_id FROM value WHERE function_name = 'mal_vector')),
    (0, 'vector?', (SELECT value_id FROM value WHERE function_name = 'mal_vector_Q')),

    (0, 'sequential?', (SELECT value_id FROM value WHERE function_name = 'mal_sequential_Q')),
    (0, 'cons', (SELECT value_id FROM value WHERE function_name = 'mal_cons')),
    (0, 'concat', (SELECT value_id FROM value WHERE function_name = 'mal_concat')),
    (0, 'nth', (SELECT value_id FROM value WHERE function_name = 'mal_nth')),
    (0, 'first', (SELECT value_id FROM value WHERE function_name = 'mal_first')),
    (0, 'rest', (SELECT value_id FROM value WHERE function_name = 'mal_rest')),
    (0, 'empty?', (SELECT value_id FROM value WHERE function_name = 'mal_empty_Q')),
    (0, 'count', (SELECT value_id FROM value WHERE function_name = 'mal_count')),
    (0, 'apply', (SELECT value_id FROM value WHERE function_name = 'mal_apply')),
    (0, 'map', (SELECT value_id FROM value WHERE function_name = 'mal_map')),

    (0, 'atom', (SELECT value_id FROM value WHERE function_name = 'mal_atom')),
    (0, 'atom?', (SELECT value_id FROM value WHERE function_name = 'mal_atom_Q')),
    (0, 'deref', (SELECT value_id FROM value WHERE function_name = 'mal_deref')),
    (0, 'reset!', (SELECT value_id FROM value WHERE function_name = 'mal_reset_BANG')),
    (0, 'swap!', (SELECT value_id FROM value WHERE function_name = 'mal_swap_BANG'))
    ;

