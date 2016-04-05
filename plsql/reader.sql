-- ---------------------------------------------------------
-- reader.sql

PROMPT "reader.sql start";

CREATE OR REPLACE TYPE tokens FORCE AS TABLE OF varchar2(4000);
/

CREATE OR REPLACE TYPE reader FORCE AS OBJECT (
    position  integer,
    toks      tokens,
    MEMBER FUNCTION peek (SELF IN OUT reader) RETURN varchar,
    MEMBER FUNCTION next (SELF IN OUT reader) RETURN varchar
);
/


CREATE OR REPLACE TYPE BODY reader AS
    MEMBER FUNCTION peek (SELF IN OUT reader) RETURN varchar IS
    BEGIN
        IF position > toks.COUNT THEN
            RETURN NULL;
        END IF;
        RETURN toks(position);
    END;
    MEMBER FUNCTION next (SELF IN OUT reader) RETURN varchar IS
    BEGIN
        position := position + 1;
        RETURN toks(position-1);
    END;
END;
/


CREATE OR REPLACE PACKAGE reader_pkg IS
    FUNCTION read_str(str varchar) RETURN mal_type;
END reader_pkg;
/

CREATE OR REPLACE PACKAGE BODY reader_pkg AS

FUNCTION tokenize(str varchar) RETURN tokens IS
    re      varchar2(100) := '[[:space:] ,]*(~@|[][{}()''`~@]|"(([\].|[^\"])*)"|;[^' || chr(10) || ']*|[^][[:space:] {}()''"`~@,;]*)';
    tok     varchar2(4000);
    toks    tokens := tokens();
    cnt     integer;
BEGIN
    cnt := REGEXP_COUNT(str, re);
    FOR I IN 1..cnt LOOP
        tok := REGEXP_SUBSTR(str, re, 1, I, 'm', 1);
        IF tok IS NOT NULL THEN
            toks.extend();
            toks(toks.COUNT) := tok;
            -- stream_writeline('tok: [' || tok || ']');
        END IF;
    END LOOP;
    RETURN toks;
END;

-- read_atom:
-- takes a reader
-- updates reader and returns value
FUNCTION read_atom(rdr IN OUT reader) RETURN mal_type IS
    str_id  integer;
    str     varchar2(4000);
    token   varchar2(4000);
    result  mal_type;
BEGIN
    token := rdr.next();
    -- stream_writeline('read_atom: ' || token);
    IF token = 'nil' THEN       -- nil
        result := mal_type(0);
    ELSIF token = 'false' THEN  -- false
        result := mal_type(1);
    ELSIF token = 'true' THEN   -- true
        result := mal_type(2);
    ELSIF REGEXP_LIKE(token, '^-?[0-9][0-9]*$') THEN  -- integer
        result := mal_int_type(3, CAST(token AS integer));
    ELSIF REGEXP_LIKE(token, '^".*"') THEN  -- string
        -- string
        str := SUBSTR(token, 2, LENGTH(token)-2);
        str := REPLACE(str, '\"', '"');
        str := REPLACE(str, '\n', chr(10));
        str := REPLACE(str, '\\', chr(92));
        result := mal_str_type(5, str);
--     ELSIF token ~ '^:.*' THEN  -- keyword
--         -- keyword
--         result := _keywordv(substring(token FROM 2 FOR (char_length(token)-1)));
    ELSE
        -- symbol
        result := mal_str_type(7, token);
    END IF;
    return result;
END;

-- forward declaration of read_form
FUNCTION read_form(rdr IN OUT reader) RETURN mal_type;

-- read_seq:
-- takes a reader
-- updates reader and returns new mal_type list/vector/hash-map
FUNCTION read_seq(rdr IN OUT reader, type_id integer,
                  first varchar, last varchar)
    RETURN mal_type IS
    token   varchar2(4000);
    items   mal_seq_items_type;
BEGIN
    token := rdr.next();
    IF token <> first THEN
        raise_application_error(-20002,
            'expected ''' || first || '''', TRUE);
    END IF;
    items := mal_seq_items_type();
    LOOP
        token := rdr.peek();
        IF token IS NULL THEN
            raise_application_error(-20002,
                'expected ''' || last || '''', TRUE);
        END IF;
        IF token = last THEN EXIT; END IF;
        items.EXTEND();
        items(items.COUNT) := read_form(rdr);
    END LOOP;
    token := rdr.next();
    RETURN mal_seq_type(type_id, items);
END;

-- read_form:
-- takes a reader
-- updates the reader and returns new mal_type value
FUNCTION read_form(rdr IN OUT reader) RETURN mal_type IS
    token   varchar2(4000);
    meta    mal_type;
BEGIN
     token := rdr.peek();  -- peek
     CASE
    WHEN token = '''' THEN
        token := rdr.next();
        RETURN types_pkg.list(
            mal_str_type(7, 'quote'),
            read_form(rdr));
    WHEN token = '`' THEN
        token := rdr.next();
        RETURN types_pkg.list(
            mal_str_type(7, 'quasiquote'),
            read_form(rdr));
    WHEN token = '~' THEN
        token := rdr.next();
        RETURN types_pkg.list(
            mal_str_type(7, 'unquote'),
            read_form(rdr));
    WHEN token = '~@' THEN
        token := rdr.next();
        RETURN types_pkg.list(
            mal_str_type(7, 'splice-unquote'),
            read_form(rdr));
    WHEN token = '^' THEN
        token := rdr.next();
        meta := read_form(rdr);
        RETURN types_pkg.list(
            mal_str_type(7, 'with-meta'),
            read_form(rdr),
            meta);
    WHEN token = '@' THEN
        token := rdr.next();
        RETURN types_pkg.list(
            mal_str_type(7, 'deref'),
            read_form(rdr));

    -- list
    WHEN token = ')' THEN
        raise_application_error(-20001,
            'unexpected '')''', TRUE);
    WHEN token = '(' THEN
        RETURN read_seq(rdr, 8, '(', ')');
--
--     -- vector
--     WHEN token = ']' THEN
--         RAISE EXCEPTION 'unexpected '']''';
--     WHEN token = '[' THEN
--     BEGIN
--         SELECT p, _vector(items)
--             FROM read_seq(tokens, '[', ']', pos) INTO pos, result;
--     END;
--
--     -- hash-map
--     WHEN token = '}' THEN
--         RAISE EXCEPTION 'unexpected ''}''';
--     WHEN token = '{' THEN
--     BEGIN
--         SELECT p, _hash_map(items)
--             FROM read_seq(tokens, '{', '}', pos) INTO pos, result;
--     END;
--
    --
    ELSE
        RETURN read_atom(rdr);
    END CASE;
END;

-- read_str:
-- takes a string
-- returns a new mal_type value
FUNCTION read_str(str varchar) RETURN mal_type IS
    toks  tokens;
    rdr   reader;
    ast   mal_type;
BEGIN
    toks := tokenize(str);
    rdr := reader(1, toks);
    -- stream_writeline('token 1: ' || rdr.peek());
    RETURN read_form(rdr);
END;

END reader_pkg;
/
show errors;

PROMPT "reader.sql finished";
