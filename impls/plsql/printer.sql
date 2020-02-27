-- ---------------------------------------------------------
-- printer.sql

CREATE OR REPLACE PACKAGE printer IS
    FUNCTION pr_str_seq(M IN OUT NOCOPY types.mal_table,
                        H IN OUT NOCOPY types.map_entry_table,
                        seq mal_vals, sep varchar2,
                        print_readably boolean DEFAULT TRUE) RETURN varchar;
    FUNCTION pr_str(M IN OUT NOCOPY types.mal_table,
                    H IN OUT NOCOPY types.map_entry_table,
                    ast integer,
                    print_readably boolean DEFAULT TRUE) RETURN varchar;
END printer;
/
show errors;

CREATE OR REPLACE PACKAGE BODY printer AS

FUNCTION pr_str_seq(M IN OUT NOCOPY types.mal_table,
                    H IN OUT NOCOPY types.map_entry_table,
                    seq mal_vals, sep varchar2,
                    print_readably boolean DEFAULT TRUE) RETURN varchar IS
    first  integer := 1;
    str    CLOB;
BEGIN
    FOR i IN 1..seq.COUNT LOOP
        IF first = 1 THEN
            first := 0;
        ELSE
            str := str || sep;
        END IF;
        str := str || pr_str(M, H, seq(i), print_readably);
    END LOOP;
    RETURN str;
END;

FUNCTION pr_str_map(M IN OUT NOCOPY types.mal_table,
                    H IN OUT NOCOPY types.map_entry_table,
                    midx integer, sep varchar2,
                    print_readably boolean DEFAULT TRUE) RETURN varchar IS
    key    varchar2(256);
    first  integer := 1;
    str    CLOB;
BEGIN
    key := H(midx).FIRST();
    WHILE key IS NOT NULL LOOP
        IF first = 1 THEN
            first := 0;
        ELSE
            str := str || sep;
        END IF;
        str := str || pr_str(M, H, types.string(M, key), print_readably);
        str := str || ' ' || pr_str(M, H, H(midx)(key), print_readably);
        key := H(midx).NEXT(key);
    END LOOP;
    RETURN str;
END;


FUNCTION pr_str(M IN OUT NOCOPY types.mal_table,
                H IN OUT NOCOPY types.map_entry_table,
                ast integer,
                print_readably boolean DEFAULT TRUE) RETURN varchar IS
    type_id  integer;
    first    integer := 1;
    i        integer;
    str      CLOB;
    malfn    mal_func_T;
BEGIN
    type_id := M(ast).type_id;
    -- io.writeline('pr_str type: ' || type_id);
    CASE
    WHEN type_id = 0 THEN RETURN 'nil';
    WHEN type_id = 1 THEN RETURN 'false';
    WHEN type_id = 2 THEN RETURN 'true';
    WHEN type_id = 3 THEN  -- integer
        RETURN CAST(TREAT(M(ast) AS mal_int_T).val_int as varchar);
    WHEN type_id IN (5,6) THEN  -- string
        IF type_id = 5 THEN
            str := TREAT(M(ast) as mal_str_T).val_str;
        ELSE
            str := TREAT(M(ast) as mal_long_str_T).val_long_str;
        END IF;
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
        RETURN TREAT(M(ast) AS mal_str_T).val_str;
    WHEN type_id = 7 THEN  -- symbol
        RETURN TREAT(M(ast) AS mal_str_T).val_str;
    WHEN type_id = 8 THEN  -- list
        RETURN '(' || pr_str_seq(M, H,
                                 TREAT(M(ast) AS mal_seq_T).val_seq, ' ',
                                 print_readably) || ')';
    WHEN type_id = 9 THEN  -- vector
        RETURN '[' || pr_str_seq(M, H,
                                 TREAT(M(ast) AS mal_seq_T).val_seq, ' ',
                                 print_readably) || ']';
    WHEN type_id = 10 THEN  -- hash-map
        RETURN '{' || pr_str_map(M, H,
                                 TREAT(M(ast) AS mal_map_T).map_idx, ' ',
                                 print_readably) || '}';
    WHEN type_id = 11 THEN  -- native function
        RETURN '#<function ' ||
               TREAT(M(ast) AS mal_str_T).val_str ||
               '>';
    WHEN type_id = 12 THEN  -- mal function
        malfn := TREAT(M(ast) AS mal_func_T);
        RETURN '(fn* ' || pr_str(M, H, malfn.params, print_readably) ||
                ' ' || pr_str(M, H, malfn.ast, print_readably) || ')';
    WHEN type_id = 13 THEN  -- atom
        RETURN '(atom ' ||
            pr_str(M, H, TREAT(M(ast) AS mal_atom_T).val, print_readably) ||
            ')';
    ELSE
        RETURN 'unknown';
    END CASE;
END;

END printer;
/
show errors;
