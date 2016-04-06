PROMPT 'core.sql start';

CREATE OR REPLACE TYPE core_ns_type IS TABLE OF varchar2(100);
/

CREATE OR REPLACE PACKAGE core_pkg IS

FUNCTION do_core_func(fn mal_type, args mal_seq_items_type) RETURN mal_type;

FUNCTION get_core_ns RETURN core_ns_type;

END core_pkg;
/


CREATE OR REPLACE PACKAGE BODY core_pkg AS

-- general functions
FUNCTION equal_Q(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN types_pkg.wraptf(types_pkg.equal_Q(args(1), args(2)));
END;

-- string functions
FUNCTION pr_str(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_str_type(5, printer_pkg.pr_str_seq(args, ' ', TRUE));
END;

FUNCTION str(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_str_type(5, printer_pkg.pr_str_seq(args, '', FALSE));
END;

FUNCTION prn(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    stream_writeline(printer_pkg.pr_str_seq(args, ' ', TRUE));
    RETURN mal_type(0);
END;

FUNCTION println(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    stream_writeline(printer_pkg.pr_str_seq(args, ' ', FALSE));
    RETURN mal_type(0);
END;

FUNCTION read_string(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN reader_pkg.read_str(TREAT(args(1) AS mal_str_type).val_str);
END;

FUNCTION slurp(args mal_seq_items_type) RETURN mal_type IS
    content  varchar2(4000);
BEGIN
    -- stream_writeline('here1: ' || TREAT(args(1) AS mal_str_type).val_str);
    content := file_open_and_read(TREAT(args(1) AS mal_str_type).val_str);
    content := REPLACE(content, '\n', chr(10));
    RETURN mal_str_type(5, content);
END;


-- numeric functions
FUNCTION lt(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN types_pkg.wraptf(TREAT(args(1) AS mal_int_type).val_int <
                            TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION lte(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN types_pkg.wraptf(TREAT(args(1) AS mal_int_type).val_int <=
                            TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION gt(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN types_pkg.wraptf(TREAT(args(1) AS mal_int_type).val_int >
                            TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION gte(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN types_pkg.wraptf(TREAT(args(1) AS mal_int_type).val_int >=
                            TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION add(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int +
                           TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION subtract(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int -
                           TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION multiply(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int *
                           TREAT(args(2) AS mal_int_type).val_int);
END;

FUNCTION divide(args mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_int_type(3, TREAT(args(1) AS mal_int_type).val_int /
                           TREAT(args(2) AS mal_int_type).val_int);
END;

-- general native function case/switch
FUNCTION do_core_func(fn mal_type, args mal_seq_items_type) RETURN mal_type IS
    fname  varchar(100);
BEGIN
    IF fn.type_id <> 11 THEN
        raise_application_error(-20004,
            'Invalid function call', TRUE);
    END IF;

    fname := TREAT(fn AS mal_str_type).val_str;

    CASE
    WHEN fname = '='           THEN RETURN equal_Q(args);

    WHEN fname = 'pr-str'      THEN RETURN pr_str(args);
    WHEN fname = 'str'         THEN RETURN str(args);
    WHEN fname = 'prn'         THEN RETURN prn(args);
    WHEN fname = 'println'     THEN RETURN println(args);
    WHEN fname = 'read-string' THEN RETURN read_string(args);
    WHEN fname = 'slurp'       THEN RETURN slurp(args);

    WHEN fname = '<'  THEN RETURN lt(args);
    WHEN fname = '<=' THEN RETURN lte(args);
    WHEN fname = '>'  THEN RETURN gt(args);
    WHEN fname = '>=' THEN RETURN gte(args);
    WHEN fname = '+' THEN RETURN add(args);
    WHEN fname = '-' THEN RETURN subtract(args);
    WHEN fname = '*' THEN RETURN multiply(args);
    WHEN fname = '/' THEN RETURN divide(args);

    WHEN fname = 'list' THEN RETURN types_pkg.list(args);
    WHEN fname = 'list?' THEN RETURN types_pkg.wraptf(args(1).type_id = 8);

    WHEN fname = 'empty?' THEN
        RETURN types_pkg.wraptf(0 = types_pkg.count(args(1)));
    WHEN fname = 'count' THEN
        IF args(1).type_id = 0 THEN
            RETURN mal_int_type(3, 0);
        ELSE
            RETURN mal_int_type(3, types_pkg.count(args(1)));
        END IF;

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

        'empty?',
        'count',

        -- defined in step do_builtin function
        'atom',
        'atom?',
        'deref',
        'reset!',
        'swap!');
END;

END core_pkg;
/
show errors;

PROMPT 'core.sql finished';
