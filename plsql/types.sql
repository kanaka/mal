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
CREATE OR REPLACE TYPE mal_seq_items_type FORCE AS TABLE OF mal_type;
/

CREATE OR REPLACE TYPE mal_seq_type FORCE UNDER mal_type (
    val_seq  mal_seq_items_type
) FINAL;
/

-- malfunc (12)
CREATE OR REPLACE TYPE malfunc_type FORCE UNDER mal_type (
    ast     mal_type,
    params  mal_type,
    env     integer
) FINAL;
/



-- ---------------------------------------------------------

CREATE OR REPLACE PACKAGE types_pkg IS
    -- general functions
    FUNCTION wraptf(val boolean) RETURN mal_type;
    FUNCTION equal_Q(a mal_type, b mal_type) RETURN boolean;

    -- scalar functions
    FUNCTION symbol(name varchar) RETURN mal_type;

    -- sequence functions
    FUNCTION list RETURN mal_type;
    FUNCTION list(a mal_type) RETURN mal_type;
    FUNCTION list(a mal_type, b mal_type) RETURN mal_type;
    FUNCTION list(a mal_type, b mal_type, c mal_type) RETURN mal_type;
    FUNCTION list(items mal_seq_items_type) RETURN mal_type;

    FUNCTION first(seq mal_type) RETURN mal_type;
    FUNCTION slice(seq mal_type, idx integer) RETURN mal_type;
    FUNCTION slice(items mal_seq_items_type, idx integer) RETURN mal_type;
    FUNCTION nth(seq mal_type, idx integer) RETURN mal_type;

    FUNCTION count(seq mal_type) RETURN integer;
END types_pkg;
/

CREATE OR REPLACE PACKAGE BODY types_pkg IS

-- ---------------------------------------------------------
-- general functions


FUNCTION wraptf(val boolean) RETURN mal_type IS
BEGIN
    IF val THEN
        RETURN mal_type(2);
    ELSE
        RETURN mal_type(1);
    END IF;
END;

FUNCTION equal_Q(a mal_type, b mal_type) RETURN boolean IS
    aseq  mal_seq_items_type;
    bseq  mal_seq_items_type;
    i     integer;
BEGIN
    if NOT (a.type_id = b.type_id OR
            (a.type_id IN (8,9) AND b.type_id IN (8,9))) THEN
        RETURN FALSE;
    END IF;

    CASE
    WHEN a.type_id IN (0,1,2) THEN
        RETURN TRUE;
    WHEN a.type_id = 3 THEN
        RETURN TREAT(a AS mal_int_type).val_int =
               TREAT(b AS mal_int_type).val_int;
    WHEN a.type_id IN (5,7) THEN
        IF TREAT(a AS mal_str_type).val_str IS NULL AND
           TREAT(b AS mal_str_type).val_str IS NULL THEN
            RETURN TRUE;
        ELSE
            RETURN TREAT(a AS mal_str_type).val_str =
                TREAT(b AS mal_str_type).val_str;
        END IF;
    WHEN a.type_id IN (8,9) THEN
        aseq := TREAT(a AS mal_seq_type).val_seq;
        bseq := TREAT(b AS mal_seq_type).val_seq;
        IF aseq.COUNT <> bseq.COUNT THEN
            RETURN FALSE;
        END IF;
        FOR i IN 1..aseq.COUNT LOOP
            IF NOT equal_Q(aseq(i), bseq(i)) THEN
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


FUNCTION symbol(name varchar) RETURN mal_type IS
BEGIN
    RETURN mal_str_type(7, name);
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

-- list:
-- return a mal list
FUNCTION list RETURN mal_type IS
BEGIN
    RETURN mal_seq_type(8, mal_seq_items_type());
END;

FUNCTION list(a mal_type) RETURN mal_type IS
BEGIN
    RETURN mal_seq_type(8, mal_seq_items_type(a));
END;

FUNCTION list(a mal_type, b mal_type) RETURN mal_type IS
BEGIN
    RETURN mal_seq_type(8, mal_seq_items_type(a, b));
END;

FUNCTION list(a mal_type, b mal_type, c mal_type) RETURN mal_type IS
BEGIN
    RETURN mal_seq_type(8, mal_seq_items_type(a, b, c));
END;

FUNCTION list(items mal_seq_items_type) RETURN mal_type IS
BEGIN
    RETURN mal_seq_type(8, items);
END;

FUNCTION first(seq mal_type) RETURN mal_type IS
BEGIN
    RETURN TREAT(seq AS mal_seq_type).val_seq(1);
END;

FUNCTION slice(seq mal_type, idx integer) RETURN mal_type IS
    old_items  mal_seq_items_type;
    new_items  mal_seq_items_type;
    i          integer;
BEGIN
    old_items := TREAT(seq AS mal_seq_type).val_seq;
    new_items := mal_seq_items_type();
    new_items.EXTEND(old_items.COUNT - idx);
    FOR i IN idx+1..old_items.COUNT LOOP
        new_items(i-idx) := old_items(i);
    END LOOP;
    RETURN mal_seq_type(8, new_items);
END;

FUNCTION slice(items mal_seq_items_type, idx integer) RETURN mal_type IS
    new_items  mal_seq_items_type;
    i          integer;
BEGIN
    new_items := mal_seq_items_type();
    new_items.EXTEND(items.COUNT - idx);
    FOR i IN idx+1..items.COUNT LOOP
        new_items(i-idx) := items(i);
    END LOOP;
    RETURN mal_seq_type(8, new_items);
END;

FUNCTION nth(seq mal_type, idx integer) RETURN mal_type IS
BEGIN
    RETURN TREAT(seq AS mal_seq_type).val_seq(idx+1);
END;

FUNCTION count(seq mal_type) RETURN integer IS
BEGIN
    RETURN TREAT(seq AS mal_seq_type).val_seq.COUNT;
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
-- 
-- 
-- -- ---------------------------------------------------------
-- -- function functions
-- 
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
-- 
-- -- ---------------------------------------------------------
-- -- atom functions
-- 
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

END types_pkg;
/
show errors;

PROMPT "types.sql finished";
