-- ---------------------------------------------------------
-- printer.sql

CREATE SCHEMA printer;

CREATE FUNCTION printer.pr_str_array(arr integer[],
    sep varchar, print_readably boolean)
    RETURNS varchar AS $$
DECLARE
    i    integer;
    res  varchar[];
BEGIN
    IF array_length(arr, 1) > 0 THEN
        FOR i IN array_lower(arr, 1) .. array_upper(arr, 1)
        LOOP
            res := array_append(res, printer.pr_str(arr[i], print_readably));
        END LOOP;
        RETURN array_to_string(res, sep);
    ELSE
        RETURN '';
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION printer.pr_str(ast integer,
    print_readably boolean DEFAULT true)
    RETURNS varchar AS $$
DECLARE
    re    varchar = E'[[:space:] ,]*(~@|[\\[\\]{}()\'`~@]|"(?:[\\\\].|[^\\\\"])*"|;.*|[^\\s \\[\\]{}()\'"`~@,;]*)';
    type  integer;
    seq   integer[];
    hash  hstore;
    cid   integer;
    vid   integer;
    pid   integer;
    str   varchar;
BEGIN
    -- RAISE NOTICE 'pr_str ast: %', ast;
    SELECT type_id FROM types.value WHERE value_id = ast INTO type;
    -- RAISE NOTICE 'pr_str type: %', type;
    CASE
    WHEN type = 0 THEN RETURN 'nil';
    WHEN type = 1 THEN RETURN 'false';
    WHEN type = 2 THEN RETURN 'true';
    WHEN type = 3 THEN  -- integer
        RETURN CAST((SELECT val_int
                     FROM types.value WHERE value_id = ast) as varchar);
    WHEN type = 5 THEN  -- string
        str := types._valueToString(ast);
        IF chr(CAST(x'7f' AS integer)) = substring(str FROM 1 FOR 1) THEN
            RETURN ':' || substring(str FROM 2 FOR (char_length(str)-1));
        ELSIF print_readably THEN
            str := replace(str, E'\\', '\\');
            str := replace(str, '"', '\"');
            str := replace(str, E'\n', '\n');
            RETURN '"' || str || '"';
        ELSE
            RETURN str;
        END IF;
    WHEN type = 7 THEN  -- symbol
        RETURN types._valueToString(ast);
    WHEN type = 8 THEN  -- list
        BEGIN
            SELECT val_seq INTO seq FROM types.value WHERE value_id = ast;
            RETURN '(' ||
                   array_to_string(array(
                        SELECT printer.pr_str(x, print_readably)
                            FROM unnest(seq) AS x), ' ') ||
                   ')';
        END;
    WHEN type = 9 THEN  -- vector
        BEGIN
            SELECT val_seq INTO seq FROM types.value WHERE value_id = ast;
            RETURN '[' ||
                   array_to_string(array(
                        SELECT printer.pr_str(x, print_readably)
                            FROM unnest(seq) AS x), ' ') ||
                   ']';
        END;
    WHEN type = 10 THEN  -- hash-map
        BEGIN
            SELECT val_hash INTO hash FROM types.value WHERE value_id = ast;
            RETURN '{' ||
                   array_to_string(array(
                        SELECT printer.pr_str(CAST(key AS integer),
                                              print_readably) || ' ' ||
                               printer.pr_str(CAST(value AS integer),
                                              print_readably)
                        FROM each(hash)), ' ') ||
                   '}';
        END;
    WHEN type = 11 THEN  -- native function
        RETURN '#<function ' ||
               (SELECT val_string FROM types.value WHERE value_id = ast) ||
               '>';
    WHEN type = 12 THEN  -- mal function
        BEGIN
            SELECT ast_id, params_id
                INTO vid, pid
                FROM types.value WHERE value_id = ast;
            RETURN '(fn* ' || printer.pr_str(pid, print_readably) ||
                   ' ' || printer.pr_str(vid, print_readably) || ')';
        END;
    WHEN type = 13 THEN  -- atom
        BEGIN
            SELECT val_seq[1] INTO vid
                FROM types.value WHERE value_id = ast;
            RETURN '(atom ' || printer.pr_str(vid, print_readably) || ')';
        END;
    ELSE
        RETURN 'unknown';
    END CASE;
END; $$ LANGUAGE plpgsql;
