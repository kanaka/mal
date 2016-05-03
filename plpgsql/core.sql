CREATE SCHEMA core;

-- general functions

CREATE FUNCTION core.equal(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._equal_Q(args[1], args[2]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.throw(args integer[]) RETURNS integer AS $$
BEGIN
    -- TODO: Only throws strings. Without subtransactions, all changes
    -- to DB up to this point get rolled back so the object being
    -- thrown dissapears.
    RAISE EXCEPTION '%', printer.pr_str(args[1], false);
END; $$ LANGUAGE plpgsql;


-- scalar functions

CREATE FUNCTION core.nil_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._nil_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.true_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._true_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.false_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._false_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.string_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._string_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.symbol(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._symbolv(types._valueToString(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.symbol_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._symbol_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.keyword(args integer[]) RETURNS integer AS $$
BEGIN
    IF types._keyword_Q(args[1]) THEN
        RETURN args[1];
    ELSE
        RETURN types._keywordv(types._valueToString(args[1]));
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.keyword_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._keyword_Q(args[1]));
END; $$ LANGUAGE plpgsql;


-- string functions

CREATE FUNCTION core.pr_str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._stringv(printer.pr_str_array(args, ' ', true));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.str(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._stringv(printer.pr_str_array(args, '', false));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.prn(args integer[]) RETURNS integer AS $$
BEGIN
    PERFORM io.writeline(printer.pr_str_array(args, ' ', true));
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.println(args integer[]) RETURNS integer AS $$
BEGIN
    PERFORM io.writeline(printer.pr_str_array(args, ' ', false));
    RETURN 0; -- nil
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.read_string(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN reader.read_str(types._valueToString(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.readline(args integer[]) RETURNS integer AS $$
DECLARE
    input  varchar;
BEGIN
    input := io.readline(types._valueToString(args[1]));
    IF input IS NULL THEN
        RETURN 0;  -- nil
    END IF;
    RETURN types._stringv(rtrim(input, E'\n'));
END; $$ LANGUAGE plpgsql;


-- See:
-- http://shuber.io/reading-from-the-filesystem-with-postgres/
CREATE FUNCTION core.slurp(args integer[]) RETURNS integer AS $$
DECLARE
    fname    varchar;
    tmp      varchar;
    cmd      varchar;
    lines    varchar[];
    content  varchar;
BEGIN
    fname := types._valueToString(args[1]);
    IF fname NOT LIKE '/%' THEN
        fname := types._valueToString(envs.vget(0, '*PWD*')) || '/' || fname;
    END IF;

    tmp := CAST(round(random()*1000000) AS varchar);

    EXECUTE format('CREATE TEMP TABLE %I (content text)', tmp);
    cmd := format('sed ''s/\\/\\\\/g'' %L', fname);
    EXECUTE format('COPY %I FROM PROGRAM %L', tmp, cmd);
    EXECUTE format('SELECT ARRAY(SELECT content FROM %I)', tmp) INTO lines;
    EXECUTE format('DROP TABLE %I', tmp);

    content := array_to_string(lines, E'\n') || E'\n';
    RETURN types._stringv(content);
END; $$ LANGUAGE plpgsql;


-- number functions

-- integer comparison
CREATE FUNCTION core.intcmp(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a bigint; b bigint; result boolean;
BEGIN
    SELECT val_int INTO a FROM types.value WHERE value_id = args[1];
    SELECT val_int INTO b FROM types.value WHERE value_id = args[2];
    EXECUTE format('SELECT $1 %s $2;', op) INTO result USING a, b;
    RETURN types._wraptf(result);
END; $$ LANGUAGE plpgsql;

-- integer operation
CREATE FUNCTION core.intop(op varchar, args integer[]) RETURNS integer AS $$
DECLARE a bigint; b bigint; result bigint;
BEGIN
    SELECT val_int INTO a FROM types.value WHERE value_id = args[1];
    SELECT val_int INTO b FROM types.value WHERE value_id = args[2];
    EXECUTE format('SELECT $1 %s $2;', op) INTO result USING a, b;
    RETURN types._numToValue(result);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.lt(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intcmp('<', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.lte(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intcmp('<=', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.gt(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intcmp('>', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.gte(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intcmp('>=', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.add(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intop('+', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.subtract(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intop('-', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.multiply(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intop('*', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.divide(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN core.intop('/', args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.time_ms(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._numToValue(
        CAST(date_part('epoch', clock_timestamp()) * 1000 AS bigint));
END; $$ LANGUAGE plpgsql;


-- collection functions

CREATE FUNCTION core.list(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._list(args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.list_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._list_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.vector(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._vector(args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.vector_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._vector_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.hash_map(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._hash_map(args);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.map_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._hash_map_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.assoc(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._assoc_BANG(types._clone(args[1]),
                             args[2:array_length(args, 1)]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.dissoc(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._dissoc_BANG(types._clone(args[1]),
                              args[2:array_length(args, 1)]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.get(args integer[]) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    IF types._type(args[1]) = 0 THEN  -- nil
        RETURN 0;
    ELSE
        result := types._get(args[1], types._valueToString(args[2]));
        IF result IS NULL THEN RETURN 0; END IF;
        RETURN result;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.contains_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._contains_Q(args[1],
                                           types._valueToString(args[2])));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.keys(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._list(types._keys(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.vals(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._list(types._vals(args[1]));
END; $$ LANGUAGE plpgsql;



-- sequence functions

CREATE FUNCTION core.sequential_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._sequential_Q(args[1]));
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.cons(args integer[]) RETURNS integer AS $$
DECLARE
    lst   integer[];
BEGIN
    lst := array_prepend(args[1], types._valueToArray(args[2]));
    RETURN types._list(lst);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.concat(args integer[]) RETURNS integer AS $$
DECLARE
    lst     integer;
    result  integer[] = ARRAY[]::integer[];
BEGIN
    FOREACH lst IN ARRAY args LOOP
        result := array_cat(result, types._valueToArray(lst));
    END LOOP;
    RETURN types._list(result);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.nth(args integer[]) RETURNS integer AS $$
DECLARE
    idx  integer;
BEGIN
    SELECT val_int INTO idx FROM types.value WHERE value_id = args[2];
    IF idx >= types._count(args[1]) THEN
        RAISE EXCEPTION 'nth: index out of range';
    END IF;
    RETURN types._nth(args[1], idx);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.first(args integer[]) RETURNS integer AS $$
BEGIN
    IF types._nil_Q(args[1]) THEN
        RETURN 0; -- nil
    ELSIF types._count(args[1]) = 0 THEN
        RETURN 0; -- nil
    ELSE
        RETURN types._first(args[1]);
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.rest(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._rest(args[1]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.empty_Q(args integer[]) RETURNS integer AS $$
BEGIN
    IF types._sequential_Q(args[1]) AND types._count(args[1]) = 0 THEN
        RETURN 2;
    ELSE
        RETURN 1;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.count(args integer[]) RETURNS integer AS $$
BEGIN
    IF types._sequential_Q(args[1]) THEN
        RETURN types._numToValue(types._count(args[1]));
    ELSIF types._nil_Q(args[1]) THEN
        RETURN types._numToValue(0);
    ELSE
        RAISE EXCEPTION 'count called on non-sequence';
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.apply(args integer[]) RETURNS integer AS $$
DECLARE
    alen   integer;
    fargs  integer[];
BEGIN
    alen := array_length(args, 1);
    fargs := array_cat(args[2:alen-1], types._valueToArray(args[alen]));
    RETURN types._apply(args[1], fargs);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.map(args integer[]) RETURNS integer AS $$
DECLARE
    x       integer;
    result  integer[];
BEGIN
    FOREACH x IN ARRAY types._valueToArray(args[2])
    LOOP
        result := array_append(result, types._apply(args[1], ARRAY[x]));
    END LOOP;
    return types._list(result);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.conj(args integer[]) RETURNS integer AS $$
DECLARE
    type  integer;
BEGIN
    type := types._type(args[1]);
    CASE
    WHEN type = 8 THEN -- list
        RETURN types._list(array_cat(
                types.array_reverse(args[2:array_length(args, 1)]),
                types._valueToArray(args[1])));
    WHEN type = 9 THEN -- vector
        RETURN types._vector(array_cat(
                types._valueToArray(args[1]),
                args[2:array_length(args, 1)]));
    ELSE
        RAISE EXCEPTION 'conj: called on non-sequence';
    END CASE;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.seq(args integer[]) RETURNS integer AS $$
DECLARE
    type  integer;
    vid   integer;
    str   varchar;
    chr   varchar;
    seq   integer[];
BEGIN
    type := types._type(args[1]);
    CASE
    WHEN type = 8 THEN -- list
        IF types._count(args[1]) = 0 THEN RETURN 0; END IF; -- nil
        RETURN args[1];
    WHEN type = 9 THEN -- vector
        IF types._count(args[1]) = 0 THEN RETURN 0; END IF; -- nil
        -- clone and modify to a list
        vid := types._clone(args[1]);
        UPDATE types.value SET type_id = 8 WHERE value_id = vid;
        RETURN vid;
    WHEN type = 5 THEN -- string
        str := types._valueToString(args[1]);
        IF char_length(str) = 0 THEN RETURN 0; END IF; -- nil
        FOREACH chr IN ARRAY regexp_split_to_array(str, '') LOOP
            seq := array_append(seq, types._stringv(chr));
        END LOOP;
        RETURN types._list(seq);
    WHEN type = 0 THEN -- nil
        RETURN 0; -- nil
    ELSE
        RAISE EXCEPTION 'seq: called on non-sequence';
    END CASE;
END; $$ LANGUAGE plpgsql;


-- meta functions

CREATE FUNCTION core.meta(args integer[]) RETURNS integer AS $$
DECLARE
    m  integer;
BEGIN
    SELECT meta_id INTO m FROM types.value WHERE value_id = args[1];
    IF m IS NULL THEN
        RETURN 0;
    ELSE
        RETURN m;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.with_meta(args integer[]) RETURNS integer AS $$
DECLARE
    vid  integer;
BEGIN
    vid := types._clone(args[1]);
    UPDATE types.value SET meta_id = args[2]
        WHERE value_id = vid;
    RETURN vid;
END; $$ LANGUAGE plpgsql;



-- atom functions

CREATE FUNCTION core.atom(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._atom(args[1]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.atom_Q(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._wraptf(types._atom_Q(args[1]));
END; $$ LANGUAGE plpgsql;


CREATE FUNCTION core.deref(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._deref(args[1]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.reset_BANG(args integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._reset_BANG(args[1], args[2]);
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION core.swap_BANG(args integer[]) RETURNS integer AS $$
DECLARE
    atm    integer;
    fargs  integer[];
BEGIN
    atm := args[1];
    fargs := array_cat(ARRAY[types._deref(atm)], args[3:array_length(args, 1)]);
    RETURN types._reset_BANG(atm, types._apply(args[2], fargs));
END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------

-- repl_env is environment 0

INSERT INTO envs.env (env_id, outer_id, data)
    VALUES (0, NULL, hstore(ARRAY[
        '=',           types._function('core.equal'),
        'throw',       types._function('core.throw'),

        'nil?',        types._function('core.nil_Q'),
        'true?',       types._function('core.true_Q'),
        'false?',      types._function('core.false_Q'),
        'string?',     types._function('core.string_Q'),
        'symbol',      types._function('core.symbol'),
        'symbol?',     types._function('core.symbol_Q'),
        'keyword',     types._function('core.keyword'),
        'keyword?',    types._function('core.keyword_Q'),

        'pr-str',      types._function('core.pr_str'),
        'str',         types._function('core.str'),
        'prn',         types._function('core.prn'),
        'println',     types._function('core.println'),
        'read-string', types._function('core.read_string'),
        'readline',    types._function('core.readline'),
        'slurp',       types._function('core.slurp'),

        '<',           types._function('core.lt'),
        '<=',          types._function('core.lte'),
        '>',           types._function('core.gt'),
        '>=',          types._function('core.gte'),
        '+',           types._function('core.add'),
        '-',           types._function('core.subtract'),
        '*',           types._function('core.multiply'),
        '/',           types._function('core.divide'),
        'time-ms',     types._function('core.time_ms'),

        'list',        types._function('core.list'),
        'list?',       types._function('core.list_Q'),
        'vector',      types._function('core.vector'),
        'vector?',     types._function('core.vector_Q'),
        'hash-map',    types._function('core.hash_map'),
        'map?',        types._function('core.map_Q'),
        'assoc',       types._function('core.assoc'),
        'dissoc',      types._function('core.dissoc'),
        'get',         types._function('core.get'),
        'contains?',   types._function('core.contains_Q'),
        'keys',        types._function('core.keys'),
        'vals',        types._function('core.vals'),

        'sequential?', types._function('core.sequential_Q'),
        'cons',        types._function('core.cons'),
        'concat',      types._function('core.concat'),
        'nth',         types._function('core.nth'),
        'first',       types._function('core.first'),
        'rest',        types._function('core.rest'),
        'empty?',      types._function('core.empty_Q'),
        'count',       types._function('core.count'),
        'apply',       types._function('core.apply'),
        'map',         types._function('core.map'),

        'conj',        types._function('core.conj'),
        'seq',         types._function('core.seq'),

        'meta',        types._function('core.meta'),
        'with-meta',   types._function('core.with_meta'),
        'atom',        types._function('core.atom'),
        'atom?',       types._function('core.atom_Q'),
        'deref',       types._function('core.deref'),
        'reset!',      types._function('core.reset_BANG'),
        'swap!',       types._function('core.swap_BANG')
        ]));
