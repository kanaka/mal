-- ---------------------------------------------------------
-- persistent values

BEGIN
  EXECUTE IMMEDIATE 'DROP TYPE mal_T FORCE';
EXCEPTION
  WHEN OTHERS THEN IF SQLCODE != -4043 THEN RAISE; END IF;
END;
/

-- list of types for type_id
-- 0:  nil
-- 1:  false
-- 2:  true
-- 3:  integer
-- 4:  float
-- 5:  string
-- 6:  long string (CLOB)
-- 7:  symbol
-- 8:  list
-- 9:  vector
-- 10: hashmap
-- 11: function
-- 12: malfunc
-- 13: atom

-- nil (0), false (1), true (2)
CREATE OR REPLACE TYPE mal_T FORCE AS OBJECT (
    type_id  integer
) NOT FINAL;
/

-- general nested table of mal values (integers)
-- used frequently for argument passing
CREATE OR REPLACE TYPE mal_vals FORCE AS TABLE OF integer;
/


-- integer (3)
CREATE OR REPLACE TYPE mal_int_T FORCE UNDER mal_T (
    val_int  integer
) FINAL;
/

-- string/keyword (5,6), symbol (7)
CREATE OR REPLACE TYPE mal_str_T FORCE UNDER mal_T (
    val_str  varchar2(4000)
) NOT FINAL;
/

CREATE OR REPLACE TYPE mal_long_str_T FORCE UNDER mal_str_T (
    val_long_str  CLOB  -- long character object (for larger than 4000 chars)
) FINAL;
/
show errors;

-- list (8), vector (9)
CREATE OR REPLACE TYPE mal_seq_T FORCE UNDER mal_T (
    val_seq  mal_vals,
    meta     integer
) FINAL;
/

CREATE OR REPLACE TYPE mal_map_T FORCE UNDER mal_T (
    map_idx  integer,  -- index into map entry table
    meta     integer
) FINAL;
/

-- malfunc (12)
CREATE OR REPLACE TYPE mal_func_T FORCE UNDER mal_T (
    ast       integer,
    params    integer,
    env       integer,
    is_macro  integer,
    meta      integer
) FINAL;
/

-- atom (13)
CREATE OR REPLACE TYPE mal_atom_T FORCE UNDER mal_T (
    val  integer  -- index into mal_table
);
/


-- ---------------------------------------------------------

CREATE OR REPLACE PACKAGE types IS
    -- memory pool for mal_objects (non-hash-map)
    TYPE mal_table IS TABLE OF mal_T;

    -- memory pool for hash-map objects
    TYPE map_entry IS TABLE OF integer INDEX BY varchar2(256);
    TYPE map_entry_table IS TABLE OF map_entry;

    -- general functions
    FUNCTION mem_new RETURN mal_table;

    FUNCTION tf(val boolean) RETURN integer;
    FUNCTION equal_Q(M IN OUT NOCOPY mal_table,
                     H IN OUT NOCOPY map_entry_table,
                     a integer, b integer) RETURN boolean;

    FUNCTION clone(M IN OUT NOCOPY mal_table,
                   H IN OUT NOCOPY map_entry_table,
                   obj integer,
                   meta integer DEFAULT 1) RETURN integer;

    -- scalar functions
    FUNCTION int(M IN OUT NOCOPY mal_table, num integer) RETURN integer;
    FUNCTION string(M IN OUT NOCOPY mal_table, name varchar) RETURN integer;
    FUNCTION string_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean;
    FUNCTION symbol(M IN OUT NOCOPY mal_table, name varchar) RETURN integer;
    FUNCTION keyword(M IN OUT NOCOPY mal_table, name varchar) RETURN integer;
    FUNCTION keyword_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean;
    FUNCTION number_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean;
    FUNCTION function_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean;
    FUNCTION macro_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean;

    -- sequence functions
    FUNCTION seq(M IN OUT NOCOPY mal_table,
                 type_id integer,
                 items mal_vals,
                 meta integer DEFAULT 1) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mal_table) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mal_table,
                  a integer) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mal_table,
                  a integer, b integer) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mal_table,
                  a integer, b integer, c integer) RETURN integer;

    FUNCTION first(M IN OUT NOCOPY mal_table,
                   seq integer) RETURN integer;
    FUNCTION slice(M IN OUT NOCOPY mal_table,
                   seq integer,
                   idx integer,
                   last integer DEFAULT NULL) RETURN integer;
    FUNCTION slice(M IN OUT NOCOPY mal_table,
                   items mal_vals,
                   idx integer) RETURN integer;
    FUNCTION islice(items mal_vals,
                    idx integer) RETURN mal_vals;
    FUNCTION nth(M IN OUT NOCOPY mal_table,
                 seq integer, idx integer) RETURN integer;

    FUNCTION count(M IN OUT NOCOPY mal_table,
                   seq integer) RETURN integer;

    FUNCTION atom_new(M IN OUT NOCOPY mal_table,
                      val integer) RETURN integer;
    FUNCTION atom_reset(M IN OUT NOCOPY mal_table,
                        atm integer,
                        val integer) RETURN integer;

    -- hash-map functions
    FUNCTION assoc_BANG(M IN OUT NOCOPY mal_table,
                        H IN OUT NOCOPY map_entry_table,
                        midx integer,
                        kvs mal_vals) RETURN integer;
    FUNCTION dissoc_BANG(M IN OUT NOCOPY mal_table,
                         H IN OUT NOCOPY map_entry_table,
                         midx integer,
                         ks mal_vals) RETURN integer;
    FUNCTION hash_map(M IN OUT NOCOPY mal_table,
                      H IN OUT NOCOPY map_entry_table,
                      kvs mal_vals,
                      meta integer DEFAULT 1) RETURN integer;

    -- function functions
    FUNCTION func(M IN OUT NOCOPY mal_table, name varchar) RETURN integer;
    FUNCTION malfunc(M IN OUT NOCOPY mal_table,
                     ast       integer,
                     params    integer,
                     env       integer,
                     is_macro  integer DEFAULT 0,
                     meta      integer DEFAULT 1) RETURN integer;
END types;
/
show errors;


CREATE OR REPLACE PACKAGE BODY types IS

-- ---------------------------------------------------------
-- general functions

FUNCTION mem_new RETURN mal_table IS
BEGIN
    -- initialize mal type memory pool
    -- 1 -> nil
    -- 2 -> false
    -- 3 -> true
    RETURN mal_table(mal_T(0), mal_T(1), mal_T(2));
END;

FUNCTION tf(val boolean) RETURN integer IS
BEGIN
    IF val THEN
        RETURN 3; -- true
    ELSE
        RETURN 2; -- false
    END IF;
END;

FUNCTION equal_Q(M IN OUT NOCOPY mal_table,
                 H IN OUT NOCOPY map_entry_table,
                 a integer, b integer) RETURN boolean IS
    atyp   integer;
    btyp   integer;
    aseq   mal_vals;
    bseq   mal_vals;
    amidx  integer;
    bmidx  integer;
    i      integer;
    k      varchar2(256);
BEGIN
    atyp := M(a).type_id;
    btyp := M(b).type_id;
    IF NOT (atyp = btyp OR (atyp IN (8,9) AND btyp IN (8,9))) THEN
        RETURN FALSE;
    END IF;

    CASE
    WHEN atyp IN (0,1,2) THEN
        RETURN TRUE;
    WHEN atyp = 3 THEN
        RETURN TREAT(M(a) AS mal_int_T).val_int =
               TREAT(M(b) AS mal_int_T).val_int;
    WHEN atyp IN (5,6,7) THEN
        IF TREAT(M(a) AS mal_str_T).val_str IS NULL AND
           TREAT(M(b) AS mal_str_T).val_str IS NULL THEN
            RETURN TRUE;
        ELSE
            RETURN TREAT(M(a) AS mal_str_T).val_str =
                   TREAT(M(b) AS mal_str_T).val_str;
        END IF;
    WHEN atyp IN (8,9) THEN
        aseq := TREAT(M(a) AS mal_seq_T).val_seq;
        bseq := TREAT(M(b) AS mal_seq_T).val_seq;
        IF aseq.COUNT <> bseq.COUNT THEN
            RETURN FALSE;
        END IF;
        FOR i IN 1..aseq.COUNT LOOP
            IF NOT equal_Q(M, H, aseq(i), bseq(i)) THEN
                RETURN FALSE;
            END IF;
        END LOOP;
        RETURN TRUE;
    WHEN atyp = 10 THEN
        amidx := TREAT(M(a) AS mal_map_T).map_idx;
        bmidx := TREAT(M(b) AS mal_map_T).map_idx;
        IF H(amidx).COUNT() <> H(bmidx).COUNT() THEN
            RETURN FALSE;
        END IF;

        k := H(amidx).FIRST();
        WHILE k IS NOT NULL LOOP
            IF H(amidx)(k) IS NULL OR H(bmidx)(k) IS NULL THEN
                RETURN FALSE;
            END IF;
            IF NOT equal_Q(M, H, H(amidx)(k), H(bmidx)(k)) THEN
                RETURN FALSE;
            END IF;
            k := H(amidx).NEXT(k);
        END LOOP;
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END CASE;
END;

FUNCTION clone(M IN OUT NOCOPY mal_table,
               H IN OUT NOCOPY map_entry_table,
               obj integer,
               meta integer DEFAULT 1) RETURN integer IS
    type_id  integer;
    new_hm    integer;
    old_midx  integer;
    new_midx  integer;
    k         varchar2(256);
    malfn     mal_func_T;
BEGIN
    type_id := M(obj).type_id;
    CASE
    WHEN type_id IN (8,9) THEN  -- list/vector
        RETURN seq(M, type_id,
                      TREAT(M(obj) AS mal_seq_T).val_seq,
                      meta);
    WHEN type_id = 10 THEN      -- hash-map
        new_hm := types.hash_map(M, H, mal_vals(), meta);
        old_midx := TREAT(M(obj) AS mal_map_T).map_idx;
        new_midx := TREAT(M(new_hm) AS mal_map_T).map_idx;

        k := H(old_midx).FIRST();
        WHILE k IS NOT NULL LOOP
            H(new_midx)(k) := H(old_midx)(k);
            k := H(old_midx).NEXT(k);
        END LOOP;

        RETURN new_hm;
    WHEN type_id = 12 THEN      -- mal function
        malfn := TREAT(M(obj) AS mal_func_T);
        RETURN types.malfunc(M,
            malfn.ast,
            malfn.params,
            malfn.env,
            malfn.is_macro,
            meta);
    ELSE
        raise_application_error(-20008,
            'clone not supported for type ' || type_id, TRUE);
    END CASE;
END;


-- ---------------------------------------------------------
-- scalar functions


FUNCTION int(M IN OUT NOCOPY mal_table, num integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_int_T(3, num);
    RETURN M.COUNT();
END;

FUNCTION string(M IN OUT NOCOPY mal_table, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    IF LENGTH(name) <= 4000 THEN
        M(M.COUNT()) := mal_str_T(5, name);
    ELSE
        M(M.COUNT()) := mal_long_str_T(6, NULL, name);
    END IF;
    RETURN M.COUNT();
END;

FUNCTION string_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean IS
    str  CLOB;
BEGIN
    IF M(val).type_id IN (5,6) THEN
        IF M(val).type_id = 5 THEN
            str := TREAT(M(val) AS mal_str_T).val_str;
        ELSE
            str := TREAT(M(val) AS mal_long_str_T).val_long_str;
        END IF;
        IF str IS NULL OR
           str = EMPTY_CLOB() OR
           SUBSTR(str, 1, 1) <> chr(127) THEN
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END IF;
    ELSE
        RETURN FALSE;
    END IF;
END;

FUNCTION symbol(M IN OUT NOCOPY mal_table, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_str_T(7, name);
    RETURN M.COUNT();
END;

FUNCTION keyword(M IN OUT NOCOPY mal_table, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_str_T(5, chr(127) || name);
    RETURN M.COUNT();
END;

FUNCTION keyword_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean IS
    str  CLOB;
BEGIN
    IF M(val).type_id = 5 THEN
        str := TREAT(M(val) AS mal_str_T).val_str;
        IF LENGTH(str) > 0 AND SUBSTR(str, 1, 1) = chr(127) THEN
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END IF;
    ELSE
        RETURN FALSE;
    END IF;
END;

FUNCTION number_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean IS
    str  CLOB;
BEGIN
    IF M(val).type_id IN (3,4) THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;

FUNCTION function_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean IS
    str  CLOB;
BEGIN
    IF M(val).type_id = 11 THEN
        RETURN TRUE;
    ELSIF M(val).type_id = 12 THEN
        RETURN TREAT(M(val) AS mal_func_T).is_macro = 0;
    ELSE
        RETURN FALSE;
    END IF;
END;

FUNCTION macro_Q(M IN OUT NOCOPY mal_table, val integer) RETURN boolean IS
    str  CLOB;
BEGIN
    IF M(val).type_id = 12 THEN
        RETURN TREAT(M(val) AS mal_func_T).is_macro > 0;
    ELSE
        RETURN FALSE;
    END IF;
END;


-- ---------------------------------------------------------
-- sequence functions

FUNCTION seq(M IN OUT NOCOPY mal_table,
             type_id integer,
             items mal_vals,
             meta integer DEFAULT 1) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(type_id, items, meta);
    RETURN M.COUNT();
END;

-- list:
-- return a mal list
FUNCTION list(M IN OUT NOCOPY mal_table) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(8, mal_vals(), 1);
    RETURN M.COUNT();
END;

FUNCTION list(M IN OUT NOCOPY mal_table,
              a integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(8, mal_vals(a), 1);
    RETURN M.COUNT();
END;

FUNCTION list(M IN OUT NOCOPY mal_table,
              a integer, b integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(8, mal_vals(a, b), 1);
    RETURN M.COUNT();
END;

FUNCTION list(M IN OUT NOCOPY mal_table,
              a integer, b integer, c integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(8, mal_vals(a, b, c), 1);
    RETURN M.COUNT();
END;

FUNCTION first(M IN OUT NOCOPY mal_table,
               seq integer) RETURN integer IS
BEGIN
    RETURN TREAT(M(seq) AS mal_seq_T).val_seq(1);
END;

FUNCTION slice(M IN OUT NOCOPY mal_table,
               seq integer,
               idx integer,
               last integer DEFAULT NULL) RETURN integer IS
    old_items  mal_vals;
    new_items  mal_vals;
    i          integer;
    final_idx  integer;
BEGIN
    old_items := TREAT(M(seq) AS mal_seq_T).val_seq;
    new_items := mal_vals();
    IF last IS NULL THEN
        final_idx := old_items.COUNT();
    ELSE
        final_idx := last + 1;
    END IF;
    IF final_idx > idx THEN
        new_items.EXTEND(final_idx - idx);
        FOR i IN idx+1..final_idx LOOP
            new_items(i-idx) := old_items(i);
        END LOOP;
    END IF;
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(8, new_items, 1);
    RETURN M.COUNT();
END;

FUNCTION slice(M IN OUT NOCOPY mal_table,
               items mal_vals,
               idx integer) RETURN integer IS
    new_items  mal_vals;
BEGIN
    new_items := islice(items, idx);
    M.EXTEND();
    M(M.COUNT()) := mal_seq_T(8, new_items, 1);
    RETURN M.COUNT();
END;

FUNCTION islice(items mal_vals,
                idx integer) RETURN mal_vals IS
    new_items  mal_vals;
    i          integer;
BEGIN
    new_items := mal_vals();
    IF items.COUNT > idx THEN
        new_items.EXTEND(items.COUNT - idx);
        FOR i IN idx+1..items.COUNT LOOP
            new_items(i-idx) := items(i);
        END LOOP;
    END IF;
    RETURN new_items;
END;


FUNCTION nth(M IN OUT NOCOPY mal_table,
             seq integer, idx integer) RETURN integer IS
BEGIN
    RETURN TREAT(M(seq) AS mal_seq_T).val_seq(idx+1);
END;

FUNCTION count(M IN OUT NOCOPY mal_table,
               seq integer) RETURN integer IS
BEGIN
    RETURN TREAT(M(seq) AS mal_seq_T).val_seq.COUNT;
END;

-- ---------------------------------------------------------
-- hash-map functions

FUNCTION assoc_BANG(M IN OUT NOCOPY mal_table,
                    H IN OUT NOCOPY map_entry_table,
                    midx integer,
                    kvs mal_vals) RETURN integer IS
    i     integer;
BEGIN
    IF MOD(kvs.COUNT(), 2) = 1 THEN
        raise_application_error(-20007,
            'odd number of arguments to assoc', TRUE);
    END IF;

    i := 1;
    WHILE i <= kvs.COUNT() LOOP
        H(midx)(TREAT(M(kvs(i)) AS mal_str_T).val_str) := kvs(i+1);
        i := i + 2;
    END LOOP;
    RETURN midx;
END;

FUNCTION dissoc_BANG(M IN OUT NOCOPY mal_table,
                     H IN OUT NOCOPY map_entry_table,
                     midx integer,
                     ks mal_vals) RETURN integer IS
    i     integer;
BEGIN
    FOR i IN 1..ks.COUNT() LOOP
        H(midx).DELETE(TREAT(M(ks(i)) AS mal_str_T).val_str);
    END LOOP;
    RETURN midx;
END;

FUNCTION hash_map(M IN OUT NOCOPY mal_table,
                  H IN OUT NOCOPY map_entry_table,
                  kvs mal_vals,
                  meta integer DEFAULT 1) RETURN integer IS
    midx  integer;
BEGIN
    H.EXTEND();
    midx := H.COUNT();
    midx := assoc_BANG(M, H, midx, kvs);

    M.EXTEND();
    M(M.COUNT()) := mal_map_T(10, midx, meta);
    RETURN M.COUNT();
END;


-- ---------------------------------------------------------
-- function functions

FUNCTION func(M IN OUT NOCOPY mal_table, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_str_T(11, name);
    RETURN M.COUNT();
END;

FUNCTION malfunc(M IN OUT NOCOPY mal_table,
                 ast       integer,
                 params    integer,
                 env       integer,
                 is_macro  integer DEFAULT 0,
                 meta      integer DEFAULT 1) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_func_T(12, ast, params, env, is_macro, meta);
    RETURN M.COUNT();
END;


-- ---------------------------------------------------------
-- atom functions

FUNCTION atom_new(M IN OUT NOCOPY mal_table,
                  val integer) RETURN integer IS
    aidx  integer;
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_atom_T(13, val);
    RETURN M.COUNT();
END;

FUNCTION atom_reset(M IN OUT NOCOPY mal_table,
                    atm integer,
                    val integer) RETURN integer IS
BEGIN
    M(atm) := mal_atom_T(13, val);
    RETURN val;
END;



END types;
/
show errors;
