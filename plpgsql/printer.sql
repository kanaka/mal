-- ---------------------------------------------------------
-- printer.sql

CREATE OR REPLACE FUNCTION
    pr_str_array(arr integer[], sep varchar, print_readably boolean)
    RETURNS varchar AS $$
DECLARE
    i    integer;
    res  varchar[];
BEGIN
    IF array_length(arr, 1) > 0 THEN
        FOR i IN array_lower(arr, 1) .. array_upper(arr, 1)
        LOOP
            res := array_append(res, pr_str(arr[i], print_readably));
        END LOOP;
        RETURN array_to_string(res, sep);
    ELSE
        RETURN '';
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
    pr_str(ast integer, print_readably boolean DEFAULT true)
    RETURNS varchar AS $$
DECLARE
    re    varchar = E'[[:space:] ,]*(~@|[\\[\\]{}()\'`~@]|"(?:[\\\\].|[^\\\\"])*"|;.*|[^\\s \\[\\]{}()\'"`~@,;]*)';
    type  integer;
    cid   integer;
    vid   integer;
    pid   integer;
    str   varchar;
BEGIN
    -- RAISE NOTICE 'pr_str ast: %', ast;
    SELECT type_id FROM value WHERE value_id = ast INTO type;
    -- RAISE NOTICE 'pr_str type: %', type;
    CASE
    WHEN type = 0 THEN RETURN 'nil';
    WHEN type = 1 THEN RETURN 'false';
    WHEN type = 2 THEN RETURN 'true';
    WHEN type = 3 THEN  -- integer
        RETURN CAST((SELECT val_int
                     FROM value WHERE value_id = ast) as varchar);
    WHEN type = 5 THEN  -- string
        str := _vstring(ast);
        IF print_readably THEN
            str := replace(str, E'\\', '\\');
            str := replace(str, '"', '\"');
            str := replace(str, E'\n', '\n');
            RETURN '"' || str || '"';
        ELSE
            RETURN str;
        END IF;
    WHEN type = 7 THEN  -- symbol
        RETURN _vstring(ast);
    WHEN type = 8 THEN  -- list
        BEGIN
            cid := (SELECT collection_id FROM value WHERE value_id = ast);
            RETURN '(' ||
                   array_to_string(array(
                        SELECT pr_str(c.value_id, print_readably)
                        FROM collection c
                        WHERE c.collection_id = cid), ' ') ||
                   ')';
        END;
    WHEN type = 9 THEN  -- vector
        BEGIN
            cid := (SELECT collection_id FROM value WHERE value_id = ast);
            RETURN '[' ||
                   array_to_string(array(
                        SELECT pr_str(c.value_id, print_readably)
                        FROM collection c
                        WHERE c.collection_id = cid), ' ') ||
                   ']';
        END;
    WHEN type = 11 THEN  -- native function
        RETURN '#<function ' ||
               (SELECT function_name FROM value WHERE value_id = ast) ||
               '>';
    WHEN type = 12 THEN  -- mal function
        BEGIN
            cid := (SELECT collection_id FROM value WHERE value_id = ast);
            SELECT params_id, value_id
                INTO pid, vid
                FROM collection WHERE collection_id = cid;
            RETURN '(fn* ' || pr_str(pid, print_readably) ||
                   ' ' || pr_str(vid, print_readably) || ')';
        END;
    WHEN type = 13 THEN  -- atom
        BEGIN
            cid := (SELECT collection_id FROM value WHERE value_id = ast);
            SELECT value_id INTO vid
                FROM collection WHERE collection_id = cid;
            RETURN '(atom ' || pr_str(vid, print_readably) || ')';
        END;
    ELSE
        RETURN 'unknown';
    END CASE;
END; $$ LANGUAGE plpgsql;
