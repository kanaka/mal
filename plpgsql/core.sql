
CREATE OR REPLACE FUNCTION mal_equal(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_equal_Q(args[1], args[2]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_equal');


-- string functions
CREATE OR REPLACE FUNCTION mal_pr_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _string(pr_str_array(args, ' ', true));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_pr_str');

CREATE OR REPLACE FUNCTION mal_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _string(pr_str_array(args, '', false));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_str');

CREATE OR REPLACE FUNCTION mal_prn(args integer[]) RETURNS integer AS $$
BEGIN
    RAISE NOTICE '%', pr_str_array(args, ' ', true);
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_prn');

CREATE OR REPLACE FUNCTION mal_println(args integer[]) RETURNS integer AS $$
BEGIN
    RAISE NOTICE '%', pr_str_array(args, ' ', false);
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
    RETURN _string(content);
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
    RETURN _arrayToValue(args);
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_list');

CREATE OR REPLACE FUNCTION mal_list_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN _wraptf(_list_Q(args[1]));
END; $$ LANGUAGE plpgsql;
INSERT INTO value (type_id, function_name) VALUES (11, 'mal_list_Q');


-- sequence functions

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

    (0, 'empty?', (SELECT value_id FROM value WHERE function_name = 'mal_empty_Q')),
    (0, 'count', (SELECT value_id FROM value WHERE function_name = 'mal_count')),

    (0, 'atom', (SELECT value_id FROM value WHERE function_name = 'mal_atom')),
    (0, 'atom?', (SELECT value_id FROM value WHERE function_name = 'mal_atom_Q')),
    (0, 'deref', (SELECT value_id FROM value WHERE function_name = 'mal_deref')),
    (0, 'reset!', (SELECT value_id FROM value WHERE function_name = 'mal_reset_BANG')),
    (0, 'swap!', (SELECT value_id FROM value WHERE function_name = 'mal_swap_BANG'))
    ;

