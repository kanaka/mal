-- ---------------------------------------------------------
-- reader.sql

CREATE OR REPLACE FUNCTION
    tokenize(str varchar) RETURNS varchar[] AS $$
DECLARE
    re varchar = E'[[:space:] ,]*(~@|[\\[\\]{}()\'`~@]|"(?:[\\\\].|[^\\\\"])*"|;.*|[^\\s \\[\\]{}()\'"`~@,;]*)';
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
    token   varchar;
BEGIN
    token := tokens[pos];
    pos := pos + 1;
    -- RAISE NOTICE 'read_atom: %', token;
    IF token = 'nil' THEN
        result := 0;
    ELSIF token = 'false' THEN
        result := 1;
    ELSIF token = 'true' THEN
        result := 2;
    ELSIF token ~ '^-?[0-9][0-9]*$' THEN
        -- integer
        INSERT INTO value (type_id, val_int)
            VALUES (3, CAST(token AS integer))
            RETURNING value_id INTO result;
    ELSIF token ~ '^".*"' THEN
        -- string
        INSERT INTO string (value)
            VALUES (substring(token FROM 2 FOR (char_length(token)-2)))
            RETURNING string_id INTO str_id;
        INSERT INTO value (type_id, val_string_id)
            VALUES (5, str_id)
            RETURNING value_id INTO result;
    ELSE
        -- symbol
        INSERT INTO string (value)
            VALUES (token)
            RETURNING string_id INTO str_id;
        INSERT INTO value (type_id, val_string_id)
            VALUES (7, str_id)
            RETURNING value_id INTO result;
    END IF;
END; $$ LANGUAGE plpgsql;

-- read_seq:
-- takes a tokens array, type (8,9), first and last characters and position
-- returns new position and value_id (or a list)
CREATE OR REPLACE FUNCTION
    read_seq(tokens varchar[], type integer, first varchar, last varchar,
             INOUT pos integer, OUT result integer) AS $$
DECLARE
    list_id   integer = NULL;
    token     varchar;
    idx       integer = 0;
    item_id   integer;
BEGIN
    token := tokens[pos];
    pos := pos + 1;
    IF token <> first THEN
        RAISE EXCEPTION 'expected ''%''', first;
    END IF;
    LOOP
        IF pos > array_length(tokens, 1) THEN
            RAISE EXCEPTION 'expected ''%''', last;
        END IF;
        token := tokens[pos];
        IF token = last THEN EXIT; END IF;
        SELECT * FROM read_form(tokens, pos) INTO pos, item_id;
        IF list_id IS NULL THEN
            list_id := (SELECT COALESCE(Max(collection_id), 0) FROM collection)+1;
        END IF;
        INSERT INTO collection (collection_id, idx, value_id)
            VALUES (list_id, idx, item_id);
        idx := idx + 1;
    END LOOP;

    -- Create new list referencing list_id
    INSERT INTO value (type_id, collection_id)
        VALUES (type, list_id)
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
        result := _list2(_symbol('quote'), vid);
    END;
    WHEN token = '`' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list2(_symbol('quasiquote'), vid);
    END;
    WHEN token = '~' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list2(_symbol('unquote'), vid);
    END;
    WHEN token = '~@' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list2(_symbol('splice-unquote'), vid);
    END;
    WHEN token = '^' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, meta;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list3(_symbol('with-meta'), vid, meta);
    END;
    WHEN token = '@' THEN
    BEGIN
        pos := pos + 1;
        SELECT * FROM read_form(tokens, pos) INTO pos, vid;
        result := _list2(_symbol('deref'), vid);
    END;

    -- list
    WHEN token = ')' THEN
        RAISE EXCEPTION 'unexpected '')''';
    WHEN token = '(' THEN
    BEGIN
        SELECT * FROM read_seq(tokens, 8, '(', ')', pos) INTO pos, result;
    END;

    -- vector
    WHEN token = ']' THEN
        RAISE EXCEPTION 'unexpected '']''';
    WHEN token = '[' THEN
    BEGIN
        SELECT * FROM read_seq(tokens, 9, '[', ']', pos) INTO pos, result;
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

