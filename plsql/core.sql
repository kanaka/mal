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

    WHEN fname = 'pr-str'      THEN RETURN pr_str(M, args);
    WHEN fname = 'str'         THEN RETURN str(M, args);
    WHEN fname = 'prn'         THEN RETURN prn(M, args);
    WHEN fname = 'println'     THEN RETURN println(M, args);
    WHEN fname = 'read-string' THEN RETURN read_string(M, args);
    WHEN fname = 'slurp'       THEN RETURN slurp(M, args);

    WHEN fname = '<'  THEN RETURN lt(M, args);
    WHEN fname = '<=' THEN RETURN lte(M, args);
    WHEN fname = '>'  THEN RETURN gt(M, args);
    WHEN fname = '>=' THEN RETURN gte(M, args);
    WHEN fname = '+'  THEN RETURN add(M, args);
    WHEN fname = '-'  THEN RETURN subtract(M, args);
    WHEN fname = '*'  THEN RETURN multiply(M, args);
    WHEN fname = '/'  THEN RETURN divide(M, args);

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

        'pr-str',
        'str',
        'prn',
        'println',
        'read-string',
        'slurp',

        '<',
        '<=',
        '>',
        '>=',
        '+',
        '-',
        '*',
        '/',

        'list',
        'list?',

        'cons',
        'concat',
        'nth',
        'first',
        'rest',
        'empty?',
        'count',

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
