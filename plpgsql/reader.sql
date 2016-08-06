-- ---------------------------------------------------------
-- reader.sql

CREATE SCHEMA reader;

CREATE FUNCTION reader.tokenize(str varchar) RETURNS varchar[] AS $$
DECLARE
    re varchar = E'[[:space:] ,]*(~@|[\\[\\]{}()\'`~@]|"(?:[\\\\].|[^\\\\"])*"|;[^\n]*|[^\\s \\[\\]{}()\'"`~@,;]*)';
BEGIN
    RETURN ARRAY(SELECT tok FROM
        (SELECT (regexp_matches(str, re, 'g'))[1] AS tok) AS x
        WHERE tok <> '' AND tok NOT LIKE ';%');
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- read_atom:
-- takes a tokens array and position
-- returns new position and value_id
CREATE FUNCTION reader.read_atom(tokens varchar[],
    INOUT pos integer, OUT result integer) AS $$
DECLARE
    str_id  integer;
    str     varchar;
    token   varchar;
BEGIN
    token := tokens[pos];
    pos := pos + 1;
    -- RAISE NOTICE 'read_atom: %', token;
    IF token = 'nil' THEN       -- nil
        result := 0;
    ELSIF token = 'false' THEN  -- false
        result := 1;
    ELSIF token = 'true' THEN   -- true
        result := 2;
    ELSIF token ~ '^-?[0-9][0-9]*$' THEN  -- integer
        -- integer
        INSERT INTO types.value (type_id, val_int)
            VALUES (3, CAST(token AS integer))
            RETURNING value_id INTO result;
    ELSIF token ~ '^".*"' THEN  -- string
        -- string
        str := substring(token FROM 2 FOR (char_length(token)-2));
        str := replace(str, '\"', '"');
        str := replace(str, '\n', E'\n');
        str := replace(str, '\\', E'\\');
        result := types._stringv(str);
    ELSIF token ~ '^:.*' THEN  -- keyword
        -- keyword
        result := types._keywordv(substring(token FROM 2 FOR (char_length(token)-1)));
    ELSE
        -- symbol
        result := types._symbolv(token);
    END IF;
END; $$ LANGUAGE plpgsql;

-- read_seq:
-- takes a tokens array, type (8, 9, 10), first and last characters
-- and position
-- returns new position and value_id for a list (8), vector (9) or
-- hash-map (10)
CREATE FUNCTION reader.read_seq(tokens varchar[], first varchar, last varchar,
    INOUT p integer, OUT items integer[]) AS $$
DECLARE
    token     varchar;
    key       varchar = NULL;
    item_id   integer;
BEGIN
    token := tokens[p];
    p := p + 1;
    IF token <> first THEN
        RAISE EXCEPTION 'expected ''%''', first;
    END IF;
    items := ARRAY[]::integer[];
    LOOP
        IF p > array_length(tokens, 1) THEN
            RAISE EXCEPTION 'expected ''%''', last;
        END IF;
        token := tokens[p];
        IF token = last THEN EXIT; END IF;
        SELECT * FROM reader.read_form(tokens, p) INTO p, item_id;
        items := array_append(items, item_id);
    END LOOP;

    p := p + 1;
END; $$ LANGUAGE plpgsql;

-- read_form:
-- takes a tokens array and position
-- returns new position and value_id
CREATE FUNCTION reader.read_form(tokens varchar[],
    INOUT pos integer, OUT result integer) AS $$
DECLARE
    vid     integer;
    meta    integer;
    token   varchar;
BEGIN
    token := tokens[pos];  -- peek
    CASE
    WHEN token = '''' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, vid;
        result := types._list(ARRAY[types._symbolv('quote'), vid]);
    END;
    WHEN token = '`' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, vid;
        result := types._list(ARRAY[types._symbolv('quasiquote'), vid]);
    END;
    WHEN token = '~' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, vid;
        result := types._list(ARRAY[types._symbolv('unquote'), vid]);
    END;
    WHEN token = '~@' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, vid;
        result := types._list(ARRAY[types._symbolv('splice-unquote'), vid]);
    END;
    WHEN token = '^' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, meta;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, vid;
        result := types._list(ARRAY[types._symbolv('with-meta'), vid, meta]);
    END;
    WHEN token = '@' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM reader.read_form(tokens, pos) INTO pos, vid;
        result := types._list(ARRAY[types._symbolv('deref'), vid]);
    END;

    -- list
    WHEN token = ')' THEN
        RAISE EXCEPTION 'unexpected '')''';
    WHEN token = '(' THEN
    BEGIN
        SELECT p, types._list(items)
            FROM reader.read_seq(tokens, '(', ')', pos) INTO pos, result;
    END;

    -- vector
    WHEN token = ']' THEN
        RAISE EXCEPTION 'unexpected '']''';
    WHEN token = '[' THEN
    BEGIN
        SELECT p, types._vector(items)
            FROM reader.read_seq(tokens, '[', ']', pos) INTO pos, result;
    END;

    -- hash-map
    WHEN token = '}' THEN
        RAISE EXCEPTION 'unexpected ''}''';
    WHEN token = '{' THEN
    BEGIN
        SELECT p, types._hash_map(items)
            FROM reader.read_seq(tokens, '{', '}', pos) INTO pos, result;
    END;

    --
    ELSE
        SELECT * FROM reader.read_atom(tokens, pos) INTO pos, result;
    END CASE;
END; $$ LANGUAGE plpgsql;

-- read_str:
-- takes a string
-- returns a new value_id
CREATE FUNCTION reader.read_str(str varchar) RETURNS integer AS $$
DECLARE
    tokens varchar[];
    pos    integer;
    ast    integer;
BEGIN
    tokens := reader.tokenize(str);
    -- RAISE NOTICE 'read_str first: %', tokens[1];
    pos := 1;
    SELECT * FROM reader.read_form(tokens, pos) INTO pos, ast;
    -- RAISE NOTICE 'pos after read_atom: %', pos;
    RETURN ast;
END; $$ LANGUAGE plpgsql;

