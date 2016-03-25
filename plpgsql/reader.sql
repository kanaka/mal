-- ---------------------------------------------------------
-- reader.sql

CREATE OR REPLACE FUNCTION
    tokenize(str varchar) RETURNS varchar[] AS $$
DECLARE
    re varchar = E'[[:space:] ,]*(~@|[\\[\\]{}()\'`~@]|"(?:[\\\\].|[^\\\\"])*"|;[^\n]*|[^\\s \\[\\]{}()\'"`~@,;]*)';
BEGIN
    RETURN ARRAY(SELECT tok FROM
        (SELECT (regexp_matches(str, re, 'g'))[1] AS tok) AS x
        WHERE tok <> '' AND tok NOT LIKE ';%');
END; $$ LANGUAGE plpgsql;

-- read_atom:
-- takes a tokens array and position
-- returns new position and value_id
CREATE OR REPLACE FUNCTION
    read_atom(tokens varchar[], INOUT pos integer, OUT result integer) AS $$
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
        INSERT INTO value (type_id, val_int)
            VALUES (3, CAST(token AS integer))
            RETURNING value_id INTO result;
    ELSIF token ~ '^".*"' THEN  -- string
        -- string
        str := substring(token FROM 2 FOR (char_length(token)-2));
        str := replace(str, '\"', '"');
        str := replace(str, '\n', E'\n');
        str := replace(str, '\\', E'\\');
        result := _stringv(str);
    ELSIF token ~ '^:.*' THEN  -- keyword
        -- keyword
        result := _keywordv(substring(token FROM 2 FOR (char_length(token)-1)));
    ELSE
        -- symbol
        result := _symbolv(token);
    END IF;
END; $$ LANGUAGE plpgsql;

-- read_coll:
-- takes a tokens array, type (8, 9, 10), first and last characters
-- and position
-- returns new position and value_id for a list (8), vector (9) or
-- hash-map (10)
CREATE OR REPLACE FUNCTION
    read_coll(tokens varchar[], type integer, first varchar, last varchar,
             INOUT pos integer, OUT result integer) AS $$
DECLARE
    coll_id   integer = NULL;
    token     varchar;
    idx       integer = 0;
    key       varchar = NULL;
    item_id   integer;
BEGIN
    token := tokens[pos];
    pos := pos + 1;
    IF token <> first THEN
        RAISE EXCEPTION 'expected ''%''', first;
    END IF;
    LOOP
        IF type = 10 THEN  -- hashmap
            -- key for hash-map
            IF pos > array_length(tokens, 1) THEN
                RAISE EXCEPTION 'expected ''%''', last;
            END IF;
            token := tokens[pos];
            IF token = last THEN EXIT; END IF;
            SELECT * FROM read_form(tokens, pos) INTO pos, item_id;

            SELECT val_string FROM value INTO key WHERE value_id = item_id;
        END IF;

        IF pos > array_length(tokens, 1) THEN
            RAISE EXCEPTION 'expected ''%''', last;
        END IF;
        token := tokens[pos];
        IF token = last THEN EXIT; END IF;
        SELECT * FROM read_form(tokens, pos) INTO pos, item_id;

        IF coll_id IS NULL THEN
            coll_id := (SELECT COALESCE(Max(collection_id), 0) FROM collection)+1;
        END IF;

        -- value for list, vector and hash-map
        INSERT INTO collection (collection_id, idx, key_string, value_id)
            VALUES (coll_id, idx, key, item_id);
        idx := idx + 1;
    END LOOP;

    -- Create new list referencing coll_id
    INSERT INTO value (type_id, collection_id)
        VALUES (type, coll_id)
        RETURNING value_id INTO result;
    pos := pos + 1;
END; $$ LANGUAGE plpgsql;

-- read_form:
-- takes a tokens array and position
-- returns new position and value_id
CREATE OR REPLACE FUNCTION
    read_form(tokens varchar[], INOUT pos integer, OUT result integer) AS $$
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
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list(ARRAY[_symbolv('quote'), vid]);
    END;
    WHEN token = '`' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list(ARRAY[_symbolv('quasiquote'), vid]);
    END;
    WHEN token = '~' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list(ARRAY[_symbolv('unquote'), vid]);
    END;
    WHEN token = '~@' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list(ARRAY[_symbolv('splice-unquote'), vid]);
    END;
    WHEN token = '^' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, meta;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list(ARRAY[_symbolv('with-meta'), vid, meta]);
    END;
    WHEN token = '@' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list(ARRAY[_symbolv('deref'), vid]);
    END;

    -- list
    WHEN token = ')' THEN
        RAISE EXCEPTION 'unexpected '')''';
    WHEN token = '(' THEN
    BEGIN
        SELECT * FROM read_coll(tokens, 8, '(', ')', pos) INTO pos, result;
    END;

    -- vector
    WHEN token = ']' THEN
        RAISE EXCEPTION 'unexpected '']''';
    WHEN token = '[' THEN
    BEGIN
        SELECT * FROM read_coll(tokens, 9, '[', ']', pos) INTO pos, result;
    END;

    -- hash-map
    WHEN token = '}' THEN
        RAISE EXCEPTION 'unexpected ''}''';
    WHEN token = '{' THEN
    BEGIN
        SELECT * FROM read_coll(tokens, 10, '{', '}', pos) INTO pos, result;
    END;

    --
    ELSE
        SELECT * FROM read_atom(tokens, pos) INTO pos, result;
    END CASE;
END; $$ LANGUAGE plpgsql;

-- read_str:
-- takes a string
-- returns a new value_id
CREATE OR REPLACE FUNCTION
    read_str(str varchar) RETURNS integer AS $$
DECLARE
    tokens varchar[];
    pos    integer;
    ast    integer;
BEGIN
    tokens := tokenize(str);
    -- RAISE NOTICE 'read_str first: %', tokens[1];
    pos := 1;
    SELECT * FROM read_form(tokens, pos) INTO pos, ast;
    -- RAISE NOTICE 'pos after read_atom: %', pos;
    RETURN ast;
END; $$ LANGUAGE plpgsql;

