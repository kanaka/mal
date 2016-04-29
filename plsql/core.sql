PROMPT 'core.sql start';

CREATE OR REPLACE TYPE core_ns_type IS TABLE OF varchar2(100);
/

CREATE OR REPLACE PACKAGE core IS

FUNCTION do_core_func(M IN OUT NOCOPY mem_type,
                      fn integer, args mal_seq_items_type) RETURN integer;

FUNCTION get_core_ns RETURN core_ns_type;

END core;
/


CREATE OR REPLACE PACKAGE BODY core AS

-- general functions
FUNCTION equal_Q(M IN OUT NOCOPY mem_type,
                 args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.wraptf(types.equal_Q(M, args(1), args(2)));
END;

-- scalar functiosn
FUNCTION symbol(M IN OUT NOCOPY mem_type,
                val integer) RETURN integer IS
BEGIN
    RETURN types.symbol(M, TREAT(M(val) AS mal_str_type).val_str);
END;


-- string functions
FUNCTION pr_str(M IN OUT NOCOPY mem_type,
                args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.string(M, printer.pr_str_seq(M, args, ' ', TRUE));
END;

FUNCTION str(M IN OUT NOCOPY mem_type,
             args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.string(M, printer.pr_str_seq(M, args, '', FALSE));
END;

FUNCTION prn(M IN OUT NOCOPY mem_type,
             args mal_seq_items_type) RETURN integer IS
BEGIN
    stream_writeline(printer.pr_str_seq(M, args, ' ', TRUE));
    RETURN 1;  -- nil
END;

FUNCTION println(M IN OUT NOCOPY mem_type,
                 args mal_seq_items_type) RETURN integer IS
BEGIN
    stream_writeline(printer.pr_str_seq(M, args, ' ', FALSE));
    RETURN 1;  -- nil
END;

FUNCTION read_string(M IN OUT NOCOPY mem_type,
                     args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN reader.read_str(M, TREAT(M(args(1)) AS mal_str_type).val_str);
END;

FUNCTION readline(M IN OUT NOCOPY mem_type,
                  prompt integer) RETURN integer IS
    input  varchar2(4000);
BEGIN
    input := stream_readline(TREAT(M(prompt) AS mal_str_type).val_str, 0);
    RETURN types.string(M, input);
EXCEPTION WHEN OTHERS THEN
    IF SQLCODE = -20001 THEN  -- io streams closed
        RETURN 1;  -- nil
    ELSE
        RAISE;
    END IF;
END;

FUNCTION slurp(M IN OUT NOCOPY mem_type,
               args mal_seq_items_type) RETURN integer IS
    content  varchar2(4000);
BEGIN
    -- stream_writeline('here1: ' || TREAT(args(1) AS mal_str_type).val_str);
    content := file_open_and_read(TREAT(M(args(1)) AS mal_str_type).val_str);
    content := REPLACE(content, '\n', chr(10));
    RETURN types.string(M, content);
END;


-- numeric functions
FUNCTION lt(M IN OUT NOCOPY mem_type,
            args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.wraptf(TREAT(M(args(1)) AS mal_int_type).val_int <
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION lte(M IN OUT NOCOPY mem_type,
             args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.wraptf(TREAT(M(args(1)) AS mal_int_type).val_int <=
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION gt(M IN OUT NOCOPY mem_type,
            args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.wraptf(TREAT(M(args(1)) AS mal_int_type).val_int >
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION gte(M IN OUT NOCOPY mem_type,
             args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.wraptf(TREAT(M(args(1)) AS mal_int_type).val_int >=
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION add(M IN OUT NOCOPY mem_type,
             args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.int(M, TREAT(M(args(1)) AS mal_int_type).val_int +
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION subtract(M IN OUT NOCOPY mem_type,
                  args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.int(M, TREAT(M(args(1)) AS mal_int_type).val_int -
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION multiply(M IN OUT NOCOPY mem_type,
                  args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.int(M, TREAT(M(args(1)) AS mal_int_type).val_int *
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION divide(M IN OUT NOCOPY mem_type,
                args mal_seq_items_type) RETURN integer IS
BEGIN
    RETURN types.int(M, TREAT(M(args(1)) AS mal_int_type).val_int /
                        TREAT(M(args(2)) AS mal_int_type).val_int);
END;

FUNCTION time_ms(M IN OUT NOCOPY mem_type) RETURN integer IS
    now  integer;
BEGIN
    -- SELECT (SYSDATE - TO_DATE('01-01-1970 00:00:00', 'DD-MM-YYYY HH24:MI:SS')) * 24 * 60 * 60 * 1000
    --    INTO now FROM DUAL;
    SELECT extract(day from(sys_extract_utc(systimestamp) - to_timestamp('1970-01-01', 'YYYY-MM-DD'))) * 86400000 + to_number(to_char(sys_extract_utc(systimestamp), 'SSSSSFF3'))
        INTO now
        FROM dual;
    RETURN types.int(M, now);
END;

-- sequence functions
FUNCTION cons(M IN OUT NOCOPY mem_type,
              args mal_seq_items_type) RETURN integer IS
    new_items  mal_seq_items_type;
    len        integer;
    i          integer;
BEGIN
    new_items := mal_seq_items_type();
    len := types.count(M, args(2));
    new_items.EXTEND(len+1);
    new_items(1) := args(1);
    FOR i IN 1..len LOOP
        new_items(i+1) := TREAT(M(args(2)) AS mal_seq_type).val_seq(i);
    END LOOP;
    RETURN types.seq(M, 8, new_items);
END;

FUNCTION concat(M IN OUT NOCOPY mem_type,
                args mal_seq_items_type) RETURN integer IS
    new_items  mal_seq_items_type;
    cur_len    integer;
    seq_len    integer;
    i          integer;
    j          integer;
BEGIN
    new_items := mal_seq_items_type();
    cur_len := 0;
    FOR i IN 1..args.COUNT() LOOP
        seq_len := types.count(M, args(i));
        new_items.EXTEND(seq_len);
        FOR j IN 1..seq_len LOOP
            new_items(cur_len + j) := types.nth(M, args(i), j-1);
        END LOOP;
        cur_len := cur_len + seq_len;
    END LOOP;
    RETURN types.seq(M, 8, new_items);
END;


-- general native function case/switch
FUNCTION do_core_func(M IN OUT NOCOPY mem_type,
                      fn integer, args mal_seq_items_type) RETURN integer IS
    fname  varchar(100);
    idx    integer;
BEGIN
    IF M(fn).type_id <> 11 THEN
        raise_application_error(-20004,
            'Invalid function call', TRUE);
    END IF;

    fname := TREAT(M(fn) AS mal_str_type).val_str;

    CASE
    WHEN fname = '='           THEN RETURN equal_Q(M, args);

    WHEN fname = 'nil?'    THEN RETURN types.wraptf(args(1) = 1);
    WHEN fname = 'false?'  THEN RETURN types.wraptf(args(1) = 2);
    WHEN fname = 'true?'   THEN RETURN types.wraptf(args(1) = 3);
    WHEN fname = 'symbol'  THEN RETURN symbol(M, args(1));
    WHEN fname = 'symbol?' THEN RETURN types.wraptf(M(args(1)).type_id = 7);

    WHEN fname = 'pr-str'      THEN RETURN pr_str(M, args);
    WHEN fname = 'str'         THEN RETURN str(M, args);
    WHEN fname = 'prn'         THEN RETURN prn(M, args);
    WHEN fname = 'println'     THEN RETURN println(M, args);
    WHEN fname = 'read-string' THEN RETURN read_string(M, args);
    WHEN fname = 'readline'    THEN RETURN readline(M, args(1));
    WHEN fname = 'slurp'       THEN RETURN slurp(M, args);

    WHEN fname = '<'  THEN RETURN lt(M, args);
    WHEN fname = '<=' THEN RETURN lte(M, args);
    WHEN fname = '>'  THEN RETURN gt(M, args);
    WHEN fname = '>=' THEN RETURN gte(M, args);
    WHEN fname = '+'  THEN RETURN add(M, args);
    WHEN fname = '-'  THEN RETURN subtract(M, args);
    WHEN fname = '*'  THEN RETURN multiply(M, args);
    WHEN fname = '/'  THEN RETURN divide(M, args);
    WHEN fname = 'time-ms' THEN RETURN time_ms(M);

    WHEN fname = 'list'  THEN RETURN types.seq(M, 8, args);
    WHEN fname = 'list?' THEN RETURN types.wraptf(M(args(1)).type_id = 8);

    WHEN fname = 'cons'   THEN RETURN cons(M, args);
    WHEN fname = 'concat' THEN RETURN concat(M, args);
    WHEN fname = 'nth'    THEN
        idx := TREAT(M(args(2)) AS mal_int_type).val_int;
        RETURN types.nth(M, args(1), idx);
    WHEN fname = 'first'  THEN
        IF args(1) = 1 OR types.count(M, args(1)) = 0 THEN
            RETURN 1;
        ELSE
            RETURN types.first(M, args(1));
        END IF;
    WHEN fname = 'rest'   THEN
        IF args(1) = 1 OR types.count(M, args(1)) = 0 THEN
            RETURN types.list(M);
        ELSE
            RETURN types.slice(M, args(1), 1);
        END IF;
    WHEN fname = 'empty?' THEN
        RETURN types.wraptf(0 = types.count(M, args(1)));
    WHEN fname = 'count' THEN
        IF M(args(1)).type_id = 0 THEN
            RETURN types.int(M, 0);
        ELSE
            RETURN types.int(M, types.count(M, args(1)));
        END IF;

    WHEN fname = 'atom' THEN
        RETURN types.atom_new(M, args(1));
    WHEN fname = 'atom?' THEN
        RETURN types.wraptf(M(args(1)).type_id = 13);
    WHEN fname = 'deref' THEN
        RETURN TREAT(M(args(1)) AS mal_atom_type).val;
    WHEN fname = 'reset!' THEN
        M(args(1)) := mal_atom_type(13, args(2));
        RETURN args(2);

    ELSE raise_application_error(-20004,
            'Invalid function call', TRUE);
    END CASE;
END;

FUNCTION get_core_ns RETURN core_ns_type IS
BEGIN
    RETURN core_ns_type(
        '=',
        'throw',

        'nil?',
        'true?',
        'false?',
        'symbol',
        'symbol?',

        'pr-str',
        'str',
        'prn',
        'println',
        'read-string',
        'readline',
        'slurp',

        '<',
        '<=',
        '>',
        '>=',
        '+',
        '-',
        '*',
        '/',
        'time-ms',

        'list',
        'list?',

        'cons',
        'concat',
        'nth',
        'first',
        'rest',
        'empty?',
        'count',
        'apply',
        'map',

        -- defined in step do_builtin function
        'atom',
        'atom?',
        'deref',
        'reset!',
        'swap!');
END;

END core;
/
show errors;

PROMPT 'core.sql finished';
