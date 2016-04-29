-- ---------------------------------------------------------
-- persistent values

PROMPT "types.sql start";

BEGIN
  EXECUTE IMMEDIATE 'DROP TYPE mal_type FORCE';
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
-- 6:  keyword (not used, uses prefixed string)
-- 7:  symbol
-- 8:  list
-- 9:  vector
-- 10: hashmap
-- 11: function
-- 12: malfunc
-- 13: atom

-- nil (0), false (1), true (2)
CREATE OR REPLACE TYPE mal_type FORCE AS OBJECT (
    type_id  integer
) NOT FINAL;
/

-- integer (3)
CREATE OR REPLACE TYPE mal_int_type FORCE UNDER mal_type (
    val_int  integer
) FINAL;
/

-- string/keyword (5), symbol (7)
CREATE OR REPLACE TYPE mal_str_type FORCE UNDER mal_type (
    val_str  varchar2(4000)
) FINAL;
/

-- list (8), vector (9)
CREATE OR REPLACE TYPE mal_seq_items_type FORCE AS TABLE OF integer;
/

CREATE OR REPLACE TYPE mal_seq_type FORCE UNDER mal_type (
    val_seq  mal_seq_items_type
) FINAL;
/

-- malfunc (12)
CREATE OR REPLACE TYPE malfunc_type FORCE UNDER mal_type (
    ast       integer,
    params    integer,
    env       integer,
    is_macro  integer
) FINAL;
/

-- atom (13)
CREATE OR REPLACE TYPE mal_atom_type FORCE UNDER mal_type (
    val  integer  -- index into mem_type
);
/

-- nested table for storing mal_types
CREATE OR REPLACE TYPE mem_type FORCE IS TABLE OF mal_type;
/



-- ---------------------------------------------------------

CREATE OR REPLACE PACKAGE types IS
    -- general functions
    FUNCTION mem_new RETURN mem_type;

    FUNCTION wraptf(val boolean) RETURN integer;
    FUNCTION equal_Q(M IN OUT NOCOPY mem_type,
                     a integer, b integer) RETURN boolean;

    -- scalar functions
    FUNCTION int(M IN OUT NOCOPY mem_type, num integer) RETURN integer;
    FUNCTION symbol(M IN OUT NOCOPY mem_type, name varchar) RETURN integer;
    FUNCTION string(M IN OUT NOCOPY mem_type, name varchar) RETURN integer;

    -- sequence functions
    FUNCTION seq (M IN OUT NOCOPY mem_type,
                  type_id integer, items mal_seq_items_type) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mem_type) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mem_type,
                  a integer) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mem_type,
                  a integer, b integer) RETURN integer;
    FUNCTION list(M IN OUT NOCOPY mem_type,
                  a integer, b integer, c integer) RETURN integer;
-- --     FUNCTION list(M IN OUT NOCOPY mem_type,
-- --                   items mal_seq_items_type) RETURN integer;

     FUNCTION first(M IN OUT NOCOPY mem_type,
                    seq integer) RETURN integer;
     FUNCTION slice(M IN OUT NOCOPY mem_type,
                    seq integer,
                    idx integer,
                    last integer DEFAULT NULL) RETURN integer;
    FUNCTION slice(M IN OUT NOCOPY mem_type,
                   items mal_seq_items_type, idx integer) RETURN integer;
    FUNCTION nth(M IN OUT NOCOPY mem_type,
                 seq integer, idx integer) RETURN integer;

    FUNCTION count(M IN OUT NOCOPY mem_type,
                   seq integer) RETURN integer;

    FUNCTION atom_new(M IN OUT NOCOPY mem_type,
                      val integer) RETURN integer;

    -- function functions
    FUNCTION func(M IN OUT NOCOPY mem_type, name varchar) RETURN integer;
    FUNCTION malfunc(M IN OUT NOCOPY mem_type,
                     ast       integer,
                     params    integer,
                     env       integer,
                     is_macro  integer DEFAULT 0
                     ) RETURN integer;
END types;
/

CREATE OR REPLACE PACKAGE BODY types IS

-- ---------------------------------------------------------
-- general functions

FUNCTION mem_new RETURN mem_type IS
BEGIN
    -- initialize mal type memory pool
    -- 1 -> nil
    -- 2 -> false
    -- 3 -> true
    RETURN mem_type(mal_type(0), mal_type(1), mal_type(2));
END;

FUNCTION wraptf(val boolean) RETURN integer IS
BEGIN
    IF val THEN
        RETURN 3; -- true
    ELSE
        RETURN 2; -- false
    END IF;
END;

FUNCTION equal_Q(M IN OUT NOCOPY mem_type,
                 a integer, b integer) RETURN boolean IS
    atyp  integer;
    btyp  integer;
    aseq  mal_seq_items_type;
    bseq  mal_seq_items_type;
    i     integer;
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
        RETURN TREAT(M(a) AS mal_int_type).val_int =
               TREAT(M(b) AS mal_int_type).val_int;
    WHEN atyp IN (5,7) THEN
        IF TREAT(M(a) AS mal_str_type).val_str IS NULL AND
           TREAT(M(b) AS mal_str_type).val_str IS NULL THEN
            RETURN TRUE;
        ELSE
            RETURN TREAT(M(a) AS mal_str_type).val_str =
                   TREAT(M(b) AS mal_str_type).val_str;
        END IF;
    WHEN atyp IN (8,9) THEN
        aseq := TREAT(M(a) AS mal_seq_type).val_seq;
        bseq := TREAT(M(b) AS mal_seq_type).val_seq;
        IF aseq.COUNT <> bseq.COUNT THEN
            RETURN FALSE;
        END IF;
        FOR i IN 1..aseq.COUNT LOOP
            IF NOT equal_Q(M, aseq(i), bseq(i)) THEN
                RETURN FALSE;
            END IF;
        END LOOP;
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END CASE;
END;

-- -- pun both NULL and false to false
-- CREATE OR REPLACE FUNCTION _tf(val boolean) RETURNS boolean AS $$
-- BEGIN
--     IF val IS NULL OR val = false THEN
--         RETURN false;
--     END IF;
--     RETURN true;
-- END; $$ LANGUAGE plpgsql IMMUTABLE;
-- 
-- -- pun both NULL and 0 to false
-- CREATE OR REPLACE FUNCTION _tf(val integer) RETURNS boolean AS $$
-- BEGIN
--     IF val IS NULL OR val = 0 THEN
--         RETURN false;
--     END IF;
--     RETURN true;
-- END; $$ LANGUAGE plpgsql IMMUTABLE;
-- 
-- -- return the type of the given value_id
-- CREATE OR REPLACE FUNCTION _type(obj integer) RETURNS integer AS $$
-- BEGIN
--     RETURN (SELECT type_id FROM value WHERE value_id = obj);
-- END; $$ LANGUAGE plpgsql;
-- 
-- 
-- CREATE OR REPLACE FUNCTION _equal_Q(a integer, b integer) RETURNS boolean AS $$
-- DECLARE
--     atype  integer;
--     btype  integer;
--     anum   bigint;
--     bnum   bigint;
--     avid   integer;
--     bvid   integer;
--     aseq   integer[];
--     bseq   integer[];
--     ahash  hstore;
--     bhash  hstore;
--     kv     RECORD;
--     i      integer;
-- BEGIN
--     atype := _type(a);
--     btype := _type(b);
--     IF NOT ((atype = btype) OR (_sequential_Q(a) AND _sequential_Q(b))) THEN
--         RETURN false;
--     END IF;
--     CASE
--     WHEN atype = 3 THEN -- integer
--         SELECT val_int FROM value INTO anum WHERE value_id = a;
--         SELECT val_int FROM value INTO bnum WHERE value_id = b;
--         RETURN anum = bnum;
--     WHEN atype = 5 OR atype = 7 THEN -- string/symbol
--         RETURN _valueToString(a) = _valueToString(b);
--     WHEN atype IN (8, 9) THEN -- list/vector
--         IF _count(a) <> _count(b) THEN
--             RETURN false;
--         END IF;
--         SELECT val_seq INTO aseq FROM value WHERE value_id = a;
--         SELECT val_seq INTO bseq FROM value WHERE value_id = b;
--         FOR i IN 1 .. _count(a)
--         LOOP
--             IF NOT _equal_Q(aseq[i], bseq[i]) THEN
--                 return false;
--             END IF;
--         END LOOP;
--         RETURN true;
--     WHEN atype = 10 THEN -- hash-map
--         SELECT val_hash INTO ahash FROM value WHERE value_id = a;
--         SELECT val_hash INTO bhash FROM value WHERE value_id = b;
--         IF array_length(akeys(ahash), 1) <> array_length(akeys(bhash), 1) THEN
--             RETURN false;
--         END IF;
--         FOR kv IN SELECT * FROM each(ahash) LOOP
--             avid := CAST((ahash -> kv.key) AS integer);
--             bvid := CAST((bhash -> kv.key) AS integer);
--             IF bvid IS NULL OR NOT _equal_Q(avid, bvid) THEN
--                 return false;
--             END IF;
--         END LOOP;
--         RETURN true;
--     ELSE
--         RETURN a = b;
--     END CASE;
-- END; $$ LANGUAGE plpgsql;
-- 
-- 
-- -- _clone:
-- -- take a value_id of a collection
-- -- returns a new value_id of a cloned collection
-- CREATE OR REPLACE FUNCTION _clone(id integer) RETURNS integer AS $$
-- DECLARE
--     result       integer;
-- BEGIN
--     INSERT INTO value (type_id,val_int,val_string,val_seq,val_hash,
--                        ast_id,params_id,env_id,meta_id)
--         (SELECT type_id,val_int,val_string,val_seq,val_hash,
--                 ast_id,params_id,env_id,meta_id
--               FROM value
--               WHERE value_id = id)
--         RETURNING value_id INTO result;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- scalar functions


FUNCTION int(M IN OUT NOCOPY mem_type, num integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_int_type(3, num);
    RETURN M.COUNT();
END;

FUNCTION symbol(M IN OUT NOCOPY mem_type, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_str_type(7, name);
    RETURN M.COUNT();
END;

FUNCTION string(M IN OUT NOCOPY mem_type, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_str_type(5, name);
    RETURN M.COUNT();
END;



-- -- _nil_Q:
-- -- takes a value_id
-- -- returns the whether value_id is nil
-- CREATE OR REPLACE FUNCTION _nil_Q(id integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN id = 0;
-- END; $$ LANGUAGE plpgsql IMMUTABLE;
-- 
-- -- _true_Q:
-- -- takes a value_id
-- -- returns the whether value_id is true
-- CREATE OR REPLACE FUNCTION _true_Q(id integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN id = 2;
-- END; $$ LANGUAGE plpgsql IMMUTABLE;
-- 
-- -- _false_Q:
-- -- takes a value_id
-- -- returns the whether value_id is false
-- CREATE OR REPLACE FUNCTION _false_Q(id integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN id = 1;
-- END; $$ LANGUAGE plpgsql IMMUTABLE;
-- 
-- -- _string_Q:
-- -- takes a value_id
-- -- returns the whether value_id is string type
-- CREATE OR REPLACE FUNCTION _string_Q(id integer) RETURNS boolean AS $$
-- BEGIN
--     IF (SELECT 1 FROM value WHERE type_id = 5 AND value_id = id) THEN
--         RETURN NOT _keyword_Q(id);
--     END IF;
--     RETURN false;
-- END; $$ LANGUAGE plpgsql;
-- 
-- 
-- -- _valueToString:
-- -- takes a value_id for a string
-- -- returns the varchar value of the string
-- CREATE OR REPLACE FUNCTION _valueToString(sid integer) RETURNS varchar AS $$
-- BEGIN
--     RETURN (SELECT val_string FROM value WHERE value_id = sid);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _stringish:
-- -- takes a varchar string
-- -- returns the value_id of a stringish type (string, symbol, keyword)
-- CREATE OR REPLACE FUNCTION _stringish(str varchar, type integer) RETURNS integer AS $$
-- DECLARE
--     result  integer;
-- BEGIN
--     -- TODO: share string data between string types
--     -- lookup if it exists
--     SELECT value_id FROM value INTO result
--         WHERE val_string = str AND type_id = type;
--     IF result IS NULL THEN
--         -- Create string entry
--         INSERT INTO value (type_id, val_string)
--             VALUES (type, str)
--             RETURNING value_id INTO result;
--     END IF;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _stringv:
-- -- takes a varchar string
-- -- returns the value_id of a string (new or existing)
-- CREATE OR REPLACE FUNCTION _stringv(str varchar) RETURNS integer AS $$
-- BEGIN
--     RETURN _stringish(str, 5);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _keywordv:
-- -- takes a varchar string
-- -- returns the value_id of a keyword (new or existing)
-- CREATE OR REPLACE FUNCTION _keywordv(name varchar) RETURNS integer AS $$
-- BEGIN
--     RETURN _stringish(chr(CAST(x'7f' AS integer)) || name, 5);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _keyword_Q:
-- -- takes a value_id
-- -- returns the whether value_id is keyword type
-- CREATE OR REPLACE FUNCTION _keyword_Q(id integer) RETURNS boolean AS $$
-- DECLARE
--     str  varchar;
-- BEGIN
--     IF (SELECT 1 FROM value WHERE type_id = 5 AND value_id = id) THEN
--         str := _valueToString(id);
--         IF char_length(str) > 0 AND
--            chr(CAST(x'7f' AS integer)) = substring(str FROM 1 FOR 1) THEN
--             RETURN true;
--         END IF;
--     END IF;
--     RETURN false;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _symbolv:
-- -- takes a varchar string
-- -- returns the value_id of a symbol (new or existing)
-- CREATE OR REPLACE FUNCTION _symbolv(name varchar) RETURNS integer AS $$
-- BEGIN
--     RETURN _stringish(name, 7);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _symbol_Q:
-- -- takes a value_id
-- -- returns the whether value_id is symbol type
-- CREATE OR REPLACE FUNCTION _symbol_Q(id integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN _tf((SELECT 1 FROM value WHERE type_id = 7 AND value_id = id));
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _numToValue:
-- -- takes an bigint number
-- -- returns the value_id for the number
-- CREATE OR REPLACE FUNCTION _numToValue(num bigint) RETURNS integer AS $$
-- DECLARE
--     result  integer;
-- BEGIN
--     SELECT value_id FROM value INTO result
--         WHERE val_int = num AND type_id = 3;
--     IF result IS NULL THEN
--         -- Create an integer entry
--         INSERT INTO value (type_id, val_int)
--             VALUES (3, num)
--             RETURNING value_id INTO result;
--     END IF;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- ---------------------------------------------------------
-- -- sequence functions
-- 
-- -- _sequential_Q:
-- -- return true if obj value_id is a list or vector
-- CREATE OR REPLACE FUNCTION _sequential_Q(obj integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN _tf((SELECT 1 FROM value
--                 WHERE value_id = obj AND (type_id = 8 OR type_id = 9)));
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _collection:
-- -- takes a array of value_id integers
-- -- returns the value_id of a new list (8), vector (9) or hash-map (10)
-- CREATE OR REPLACE FUNCTION _collection(items integer[], type integer) RETURNS integer AS $$
-- DECLARE
--     vid  integer;
-- BEGIN
--     IF type IN (8, 9) THEN
--         INSERT INTO value (type_id, val_seq)
--             VALUES (type, items)
--             RETURNING value_id INTO vid;
--     ELSIF type = 10 THEN
--         IF (array_length(items, 1) % 2) = 1 THEN
--             RAISE EXCEPTION 'hash-map: odd number of arguments';
--         END IF;
--         INSERT INTO value (type_id, val_hash)
--             VALUES (type, hstore(CAST(items AS varchar[])))
--             RETURNING value_id INTO vid;
--     END IF;
--     RETURN vid;
-- END; $$ LANGUAGE plpgsql;
-- 

-- ---------------------------------------------------------
-- sequence functions

FUNCTION seq(M IN OUT NOCOPY mem_type,
             type_id integer, items mal_seq_items_type) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_type(type_id, items);
    RETURN M.COUNT();
END;

-- list:
-- return a mal list
FUNCTION list(M IN OUT NOCOPY mem_type) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_type(8, mal_seq_items_type());
    RETURN M.COUNT();
END;

FUNCTION list(M IN OUT NOCOPY mem_type,
              a integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_type(8, mal_seq_items_type(a));
    RETURN M.COUNT();
END;

FUNCTION list(M IN OUT NOCOPY mem_type,
              a integer, b integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_type(8, mal_seq_items_type(a, b));
    RETURN M.COUNT();
END;

FUNCTION list(M IN OUT NOCOPY mem_type,
              a integer, b integer, c integer) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_seq_type(8, mal_seq_items_type(a, b, c));
    RETURN M.COUNT();
END;

FUNCTION first(M IN OUT NOCOPY mem_type,
               seq integer) RETURN integer IS
BEGIN
    RETURN TREAT(M(seq) AS mal_seq_type).val_seq(1);
END;

FUNCTION slice(M IN OUT NOCOPY mem_type,
               seq integer,
               idx integer,
               last integer DEFAULT NULL) RETURN integer IS
    old_items  mal_seq_items_type;
    new_items  mal_seq_items_type;
    i          integer;
    final_idx  integer;
BEGIN
    old_items := TREAT(M(seq) AS mal_seq_type).val_seq;
    new_items := mal_seq_items_type();
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
    M(M.COUNT()) := mal_seq_type(8, new_items);
    RETURN M.COUNT();
END;

FUNCTION slice(M IN OUT NOCOPY mem_type,
               items mal_seq_items_type, idx integer) RETURN integer IS
    new_items  mal_seq_items_type;
    i          integer;
BEGIN
    new_items := mal_seq_items_type();
    IF items.COUNT > idx THEN
        new_items.EXTEND(items.COUNT - idx);
        FOR i IN idx+1..items.COUNT LOOP
            new_items(i-idx) := items(i);
        END LOOP;
    END IF;
    M.EXTEND();
    M(M.COUNT()) := mal_seq_type(8, new_items);
    RETURN M.COUNT();
END;

FUNCTION nth(M IN OUT NOCOPY mem_type,
             seq integer, idx integer) RETURN integer IS
BEGIN
    RETURN TREAT(M(seq) AS mal_seq_type).val_seq(idx+1);
END;

FUNCTION count(M IN OUT NOCOPY mem_type,
               seq integer) RETURN integer IS
BEGIN
    RETURN TREAT(M(seq) AS mal_seq_type).val_seq.COUNT;
END;

-- -- _vector:
-- -- takes a array of value_id integers
-- -- returns the value_id of a new list
-- CREATE OR REPLACE FUNCTION _vector(items integer[]) RETURNS integer AS $$
-- BEGIN
--     RETURN _collection(items, 9);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _list_Q:
-- -- return true if obj value_id is a list
-- CREATE OR REPLACE FUNCTION _list_Q(obj integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN _tf((SELECT 1 FROM value WHERE value_id = obj and type_id = 8));
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _vector_Q:
-- -- return true if obj value_id is a list
-- CREATE OR REPLACE FUNCTION _vector_Q(obj integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN _tf((SELECT 1 FROM value WHERE value_id = obj and type_id = 9));
-- END; $$ LANGUAGE plpgsql;
-- 
-- 
-- -- _valueToArray:
-- -- takes an value_id referring to a list or vector
-- -- returns an array of the value_ids from the list/vector
-- CREATE OR REPLACE FUNCTION _valueToArray(seq integer) RETURNS integer[] AS $$
-- DECLARE
--     result  integer[];
-- BEGIN
--     result := (SELECT val_seq FROM value WHERE value_id = seq);
--     IF result IS NULL THEN
--         result := ARRAY[]::integer[];
--     END IF;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- From: https://wiki.postgresql.org/wiki/Array_reverse
-- CREATE OR REPLACE FUNCTION array_reverse(a integer[]) RETURNS integer[] AS $$
-- SELECT ARRAY(
--     SELECT a[i]
--     FROM generate_subscripts(a,1) AS s(i)
--     ORDER BY i DESC
-- );
-- $$ LANGUAGE 'sql' STRICT IMMUTABLE;
-- 
-- 
-- -- _nth:
-- -- takes value_id and an index
-- -- returns the value_id of nth element in list/vector
-- CREATE OR REPLACE FUNCTION _nth(seq_id integer, indx integer) RETURNS integer AS $$
-- DECLARE
--     result  integer;
-- BEGIN
--     RETURN (SELECT val_seq[indx+1] FROM value WHERE value_id = seq_id);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _first:
-- -- takes value_id
-- -- returns the value_id of first element in list/vector
-- CREATE OR REPLACE FUNCTION _first(seq_id integer) RETURNS integer AS $$
-- BEGIN
--     RETURN _nth(seq_id, 0);
-- END; $$ LANGUAGE plpgsql;
-- 
-- 
-- -- _restArray:
-- -- takes value_id
-- -- returns the array of value_ids
-- CREATE OR REPLACE FUNCTION _restArray(seq_id integer) RETURNS integer[] AS $$
-- DECLARE
--     result  integer[];
-- BEGIN
--     result := (SELECT val_seq FROM value WHERE value_id = seq_id);
--     RETURN result[2:array_length(result, 1)];
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _slice:
-- -- takes value_id, a first index and an last index
-- -- returns the value_id of new list from first (inclusive) to last (exclusive)
-- CREATE OR REPLACE FUNCTION _slice(seq_id integer, first integer, last integer)
-- RETURNS integer AS $$
-- DECLARE
--     seq            integer[];
--     vid            integer;
--     i              integer;
--     result         integer;
-- BEGIN
--     SELECT val_seq INTO seq FROM value WHERE value_id = seq_id;
--     INSERT INTO value (type_id, val_seq)
--         VALUES (8, seq[first+1:last])
--         RETURNING value_id INTO result;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _rest:
-- -- takes value_id
-- -- returns the value_id of new list
-- CREATE OR REPLACE FUNCTION _rest(seq_id integer) RETURNS integer AS $$
-- BEGIN
--     RETURN _slice(seq_id, 1, _count(seq_id));
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _count:
-- -- takes value_id
-- -- returns a count (not value_id)
-- CREATE OR REPLACE FUNCTION _count(seq_id integer) RETURNS integer AS $$
-- DECLARE
--     result  integer[];
-- BEGIN
--     result := (SELECT val_seq FROM value
--                          WHERE value_id = seq_id);
--     RETURN COALESCE(array_length(result, 1), 0);
-- END; $$ LANGUAGE plpgsql;
-- 
-- 
-- -- ---------------------------------------------------------
-- -- hash-map functions
-- 
-- -- _hash_map:
-- -- return value_id of a new hash-map
-- CREATE OR REPLACE FUNCTION _hash_map(items integer[]) RETURNS integer AS $$
-- BEGIN
--     RETURN _collection(items, 10);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _hash_map_Q:
-- -- return true if obj value_id is a list
-- CREATE OR REPLACE FUNCTION _hash_map_Q(obj integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN _tf((SELECT 1 FROM value WHERE value_id = obj and type_id = 10));
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _assoc_BANG:
-- -- return value_id of the hash-map with new elements appended
-- CREATE OR REPLACE FUNCTION _assoc_BANG(hm integer, items integer[]) RETURNS integer AS $$
-- DECLARE
--     hash  hstore;
-- BEGIN
--     IF (array_length(items, 1) % 2) = 1 THEN
--         RAISE EXCEPTION 'hash-map: odd number of arguments';
--     END IF;
--     SELECT val_hash INTO hash FROM value WHERE value_id = hm;
--     IF hash IS NULL THEN
--         UPDATE value SET val_hash = hstore(CAST(items AS varchar[]))
--             WHERE value_id = hm;
--     ELSE
--         UPDATE value SET val_hash = hash || hstore(CAST(items AS varchar[]))
--             WHERE value_id = hm;
--     END IF;
--     RETURN hm;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _dissoc_BANG:
-- -- return value_id of the hash-map with elements removed
-- CREATE OR REPLACE FUNCTION _dissoc_BANG(hm integer, items integer[]) RETURNS integer AS $$
-- DECLARE
--     hash  hstore;
-- BEGIN
--     SELECT val_hash INTO hash FROM value WHERE value_id = hm;
--     UPDATE value SET val_hash = hash - CAST(items AS varchar[])
--             WHERE value_id = hm;
--     RETURN hm;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _get:
-- -- return value_id of the hash-map entry matching key
-- CREATE OR REPLACE FUNCTION _get(hm integer, key varchar) RETURNS integer AS $$
-- DECLARE
--     hash  hstore;
-- BEGIN
--     SELECT val_hash INTO hash FROM value WHERE value_id = hm;
--     RETURN hash -> CAST(_stringv(key) AS varchar);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _contains_Q:
-- -- return true if hash-map contains entry matching key
-- CREATE OR REPLACE FUNCTION _contains_Q(hm integer, key varchar) RETURNS boolean AS $$
-- DECLARE
--     hash  hstore;
-- BEGIN
--     SELECT val_hash INTO hash FROM value WHERE value_id = hm;
--     RETURN _tf(hash ? CAST(_stringv(key) AS varchar));
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _keys:
-- -- return array of key value_ids from hash-map
-- CREATE OR REPLACE FUNCTION _keys(hm integer) RETURNS integer[] AS $$
-- DECLARE
--     hash  hstore;
-- BEGIN
--     SELECT val_hash INTO hash FROM value WHERE value_id = hm;
--     RETURN CAST(akeys(hash) AS integer[]);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _vals:
-- -- return array of value value_ids from hash-map
-- CREATE OR REPLACE FUNCTION _vals(hm integer) RETURNS integer[] AS $$
-- DECLARE
--     hash  hstore;
-- BEGIN
--     SELECT val_hash INTO hash FROM value WHERE value_id = hm;
--     RETURN CAST(avals(hash) AS integer[]);
-- END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- function functions

FUNCTION func(M IN OUT NOCOPY mem_type, name varchar) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_str_type(11, name);
    RETURN M.COUNT();
END;

FUNCTION malfunc(M IN OUT NOCOPY mem_type,
                 ast       integer,
                 params    integer,
                 env       integer,
                 is_macro  integer DEFAULT 0
                 ) RETURN integer IS
BEGIN
    M.EXTEND();
    M(M.COUNT()) := malfunc_type(12, ast, params, env, is_macro);
    RETURN M.COUNT();
END;

-- -- _function:
-- -- takes a function name
-- -- returns the value_id of a new 
-- CREATE OR REPLACE FUNCTION _function(fname varchar)
-- RETURNS varchar AS $$
-- DECLARE
--     result  integer;
-- BEGIN
--     INSERT INTO value (type_id, val_string)
--         VALUES (11, fname)
--         RETURNING value_id INTO result;
--     RETURN CAST(result AS varchar);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _malfunc:
-- -- takes a ast value_id, params value_id and env_id
-- -- returns the value_id of a new function
-- CREATE OR REPLACE FUNCTION _malfunc(ast integer, params integer, env integer)
-- RETURNS integer AS $$
-- DECLARE
--     cid     integer = NULL;
--     result  integer;
-- BEGIN
--     -- Create function entry
--     INSERT INTO value (type_id, ast_id, params_id, env_id)
--         VALUES (12, ast, params, env)
--         RETURNING value_id into result;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _macro:
-- CREATE OR REPLACE FUNCTION _macro(func integer) RETURNS integer AS $$
-- DECLARE
--     newfunc  integer;
--     cid      integer;
-- BEGIN
--     newfunc := _clone(func);
--     UPDATE value SET macro = true WHERE value_id = newfunc;
--     RETURN newfunc;
-- END; $$ LANGUAGE plpgsql;
-- 
-- CREATE OR REPLACE FUNCTION _apply(func integer, args integer[]) RETURNS integer AS $$
-- DECLARE
--     type     integer;
--     fcid     integer;
--     fname    varchar;
--     fast     integer;
--     fparams  integer;
--     fenv     integer;
--     result   integer;
-- BEGIN
--     SELECT type_id, val_string, ast_id, params_id, env_id
--         INTO type, fname, fast, fparams, fenv
--         FROM value WHERE value_id = func;
--     IF type = 11 THEN
--         EXECUTE format('SELECT %s($1);', fname)
--             INTO result USING args;
--         RETURN result;
--     ELSIF type = 12 THEN
--         -- NOTE: forward reference to current step EVAL function
--         RETURN EVAL(fast, env_new_bindings(fenv, fparams, args));
--     ELSE
--         RAISE EXCEPTION 'Invalid function call';
--     END IF;
-- END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------
-- atom functions

FUNCTION atom_new(M IN OUT NOCOPY mem_type,
                  val integer) RETURN integer IS
    aidx  integer;
BEGIN
    M.EXTEND();
    M(M.COUNT()) := mal_atom_type(13, val);
    RETURN M.COUNT();
END;


-- -- _atom:
-- -- takes an ast value_id
-- -- returns a new atom value_id
-- CREATE OR REPLACE FUNCTION _atom(val integer) RETURNS integer AS $$
-- DECLARE
--     cid     integer = NULL;
--     result  integer;
-- BEGIN
--     -- Create atom
--     INSERT INTO value (type_id, val_seq)
--         VALUES (13, ARRAY[val])
--         RETURNING value_id INTO result;
--     RETURN result;
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _atom_Q:
-- -- takes a value_id
-- -- returns the whether value_id is an atom
-- CREATE OR REPLACE FUNCTION _atom_Q(id integer) RETURNS boolean AS $$
-- BEGIN
--     RETURN EXISTS(SELECT 1 FROM value WHERE type_id = 13 AND value_id = id);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _deref:
-- -- takes an atom value_id
-- -- returns a atom value value_id
-- CREATE OR REPLACE FUNCTION _deref(atm integer) RETURNS integer AS $$
-- DECLARE
--     result  integer;
-- BEGIN
--     RETURN (SELECT val_seq[1] FROM value WHERE value_id = atm);
-- END; $$ LANGUAGE plpgsql;
-- 
-- -- _reset_BANG:
-- -- takes an atom value_id and new value value_id
-- -- returns a new value value_id
-- CREATE OR REPLACE FUNCTION _reset_BANG(atm integer, newval integer) RETURNS integer AS $$
-- BEGIN
--     UPDATE value SET val_seq = ARRAY[newval] WHERE value_id = atm;
--     RETURN newval;
-- END; $$ LANGUAGE plpgsql;

END types;
/
show errors;

PROMPT "types.sql finished";
