-- ---------------------------------------------------------
-- printer.sql

PROMPT "printer start";

CREATE OR REPLACE PACKAGE printer_pkg IS
    FUNCTION pr_str(ast mal_type, print_readably boolean DEFAULT true)
        RETURN varchar;
END printer_pkg;
/

CREATE OR REPLACE PACKAGE BODY printer_pkg AS

FUNCTION pr_str(ast mal_type, print_readably boolean DEFAULT true)
    RETURN varchar IS
    type_id  integer;
    first    integer := 1;
    i        integer;
    seq      mal_seq_type;
    str      varchar2(4000);
BEGIN
    type_id := ast.type_id;
    -- stream_writeline('pr_str type: ' || type_id);
    CASE
    WHEN type_id = 0 THEN RETURN 'nil';
    WHEN type_id = 1 THEN RETURN 'false';
    WHEN type_id = 2 THEN RETURN 'true';
    WHEN type_id = 3 THEN  -- integer
        RETURN CAST(TREAT(ast AS mal_int_type).val_int as varchar);
    WHEN type_id = 5 THEN  -- string
--         str := _valueToString(ast);
--         IF chr(CAST(x'7f' AS integer)) = substring(str FROM 1 FOR 1) THEN
--             RETURN ':' || substring(str FROM 2 FOR (char_length(str)-1));
--         ELSIF print_readably THEN
--             str := replace(str, E'\\', '\\');
--             str := replace(str, '"', '\"');
--             str := replace(str, E'\n', '\n');
--             RETURN '"' || str || '"';
--         ELSE
--             RETURN str;
--         END IF;
        RETURN TREAT(ast AS mal_str_type).val_str;
    WHEN type_id = 7 THEN  -- symbol
        RETURN TREAT(ast AS mal_str_type).val_str;
    WHEN type_id = 8 THEN  -- list
        seq := TREAT(ast AS mal_seq_type);
        str := '(';
        FOR i IN 1..seq.val_seq.COUNT LOOP
            IF first = 1 THEN
                first := 0;
            ELSE
                str := str || ' ';
            END IF;
            str := str || pr_str(seq.val_seq(i));
        END LOOP;
        RETURN str || ')';
--     WHEN type_id = 9 THEN  -- vector
--         BEGIN
--             SELECT val_seq INTO seq FROM value WHERE value_id = ast;
--             RETURN '[' ||
--                    array_to_string(array(
--                         SELECT pr_str(x, print_readably)
--                             FROM unnest(seq) AS x), ' ') ||
--                    ']';
--         END;
--     WHEN type_id = 10 THEN  -- hash-map
--         BEGIN
--             SELECT val_hash INTO hash FROM value WHERE value_id = ast;
--             RETURN '{' ||
--                    array_to_string(array(
--                         SELECT pr_str(CAST(key AS integer), print_readably) ||
--                                ' ' ||
--                                pr_str(CAST(value AS integer), print_readably)
--                         FROM each(hash)), ' ') ||
--                    '}';
--         END;
--     WHEN type_id = 11 THEN  -- native function
--         RETURN '#<function ' ||
--                (SELECT val_string FROM value WHERE value_id = ast) ||
--                '>';
--     WHEN type_id = 12 THEN  -- mal function
--         BEGIN
--             SELECT ast_id, params_id
--                 INTO vid, pid
--                 FROM value WHERE value_id = ast;
--             RETURN '(fn* ' || pr_str(pid, print_readably) ||
--                    ' ' || pr_str(vid, print_readably) || ')';
--         END;
--     WHEN type_id = 13 THEN  -- atom
--         BEGIN
--             SELECT val_seq[1] INTO vid
--                 FROM value WHERE value_id = ast;
--             RETURN '(atom ' || pr_str(vid, print_readably) || ')';
--         END;
    ELSE
        RETURN 'unknown';
    END CASE;
END;

END printer_pkg;
/
show errors;

PROMPT "printer finished";
