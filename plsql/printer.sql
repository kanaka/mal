-- ---------------------------------------------------------
-- printer.sql

PROMPT "printer start";

CREATE OR REPLACE PACKAGE printer_pkg IS
    FUNCTION pr_str(ast mal_type,
                    print_readably boolean DEFAULT TRUE) RETURN varchar;
    FUNCTION pr_str_seq(seq mal_seq_items_type, sep varchar2,
                        print_readably boolean DEFAULT TRUE) RETURN varchar;
END printer_pkg;
/

CREATE OR REPLACE PACKAGE BODY printer_pkg AS

FUNCTION pr_str_seq(seq mal_seq_items_type, sep varchar2,
                    print_readably boolean DEFAULT TRUE) RETURN varchar IS
    first  integer := 1;
    str    varchar2(4000) := '';
BEGIN
    FOR i IN 1..seq.COUNT LOOP
        IF first = 1 THEN
            first := 0;
        ELSE
            str := str || sep;
        END IF;
        str := str || pr_str(seq(i), print_readably);
    END LOOP;
    RETURN str;
END;

FUNCTION pr_str(ast mal_type,
                print_readably boolean DEFAULT TRUE) RETURN varchar IS
    type_id  integer;
    first    integer := 1;
    i        integer;
    str      varchar2(4000);
    malfn    malfunc_type;
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
        str := TREAT(ast as mal_str_type).val_str;
        IF chr(127) = SUBSTR(str, 1, 1) THEN
            RETURN ':' || SUBSTR(str, 2, LENGTH(str)-1);
        ELSIF print_readably THEN
            str := REPLACE(str, chr(92), '\\');
            str := REPLACE(str, '"', '\"');
            str := REPLACE(str, chr(10), '\n');
            RETURN '"' || str || '"';
        ELSE
            RETURN str;
        END IF;
        RETURN TREAT(ast AS mal_str_type).val_str;
    WHEN type_id = 7 THEN  -- symbol
        RETURN TREAT(ast AS mal_str_type).val_str;
    WHEN type_id = 8 THEN  -- list
        RETURN '(' || pr_str_seq(TREAT(ast AS mal_seq_type).val_seq, ' ',
                                 print_readably) || ')';
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
    WHEN type_id = 11 THEN  -- native function
        RETURN '#<function ' ||
               TREAT(ast AS mal_str_type).val_str ||
               '>';
    WHEN type_id = 12 THEN  -- mal function
        malfn := TREAT(ast AS malfunc_type);
        RETURN '(fn* ' || pr_str(malfn.params, print_readably) ||
                ' ' || pr_str(malfn.ast, print_readably) || ')';
    WHEN type_id = 13 THEN  -- atom
        RETURN '(atom ...)';
    ELSE
        RETURN 'unknown';
    END CASE;
END;

END printer_pkg;
/
show errors;

PROMPT "printer finished";
