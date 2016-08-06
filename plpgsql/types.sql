-- ---------------------------------------------------------
-- persistent values

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

CREATE SCHEMA types

    CREATE SEQUENCE value_id_seq START WITH 3 -- skip nil, false, true

    CREATE TABLE value (
        value_id        integer NOT NULL DEFAULT nextval('value_id_seq'),
        type_id         integer NOT NULL,
        val_int         bigint,    -- set for integers
        val_string      varchar,   -- set for strings, keywords, symbols,
                                -- and native functions (function name)
        val_seq         integer[], -- set for lists and vectors
        val_hash        hstore,    -- set for hash-maps
        ast_id          integer,   -- set for malfunc
        params_id       integer,   -- set for malfunc
        env_id          integer,   -- set for malfunc
        macro           boolean,   -- set for malfunc
        meta_id         integer    -- can be set for any collection
    );

ALTER TABLE types.value ADD CONSTRAINT pk_value_id
    PRIMARY KEY (value_id);
-- drop sequence when table dropped
ALTER SEQUENCE types.value_id_seq OWNED BY types.value.value_id;
ALTER TABLE types.value ADD CONSTRAINT fk_meta_id
    FOREIGN KEY (meta_id) REFERENCES types.value(value_id);
ALTER TABLE types.value ADD CONSTRAINT fk_params_id
    FOREIGN KEY (params_id) REFERENCES types.value(value_id);

CREATE INDEX ON types.value (value_id, type_id);

INSERT INTO types.value (value_id, type_id) VALUES (0, 0); -- nil
INSERT INTO types.value (value_id, type_id) VALUES (1, 1); -- false
INSERT INTO types.value (value_id, type_id) VALUES (2, 2); -- true


-- ---------------------------------------------------------
-- general functions

CREATE FUNCTION types._wraptf(val boolean) RETURNS integer AS $$
BEGIN
    IF val THEN
        RETURN 2;
    ELSE
        RETURN 1;
    END IF;
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- pun both NULL and false to false
CREATE FUNCTION types._tf(val boolean) RETURNS boolean AS $$
BEGIN
    IF val IS NULL OR val = false THEN
        RETURN false;
    END IF;
    RETURN true;
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- pun both NULL and 0 to false
CREATE FUNCTION types._tf(val integer) RETURNS boolean AS $$
BEGIN
    IF val IS NULL OR val = 0 THEN
        RETURN false;
    END IF;
    RETURN true;
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- return the type of the given value_id
CREATE FUNCTION types._type(obj integer) RETURNS integer AS $$
BEGIN
    RETURN (SELECT type_id FROM types.value WHERE value_id = obj);
END; $$ LANGUAGE plpgsql;


CREATE FUNCTION types._equal_Q(a integer, b integer) RETURNS boolean AS $$
DECLARE
    atype  integer;
    btype  integer;
    anum   bigint;
    bnum   bigint;
    avid   integer;
    bvid   integer;
    aseq   integer[];
    bseq   integer[];
    ahash  hstore;
    bhash  hstore;
    kv     RECORD;
    i      integer;
BEGIN
    atype := types._type(a);
    btype := types._type(b);
    IF NOT ((atype = btype) OR
            (types._sequential_Q(a) AND types._sequential_Q(b))) THEN
        RETURN false;
    END IF;
    CASE
    WHEN atype = 3 THEN -- integer
        SELECT val_int FROM types.value INTO anum WHERE value_id = a;
        SELECT val_int FROM types.value INTO bnum WHERE value_id = b;
        RETURN anum = bnum;
    WHEN atype = 5 OR atype = 7 THEN -- string/symbol
        RETURN types._valueToString(a) = types._valueToString(b);
    WHEN atype IN (8, 9) THEN -- list/vector
        IF types._count(a) <> types._count(b) THEN
            RETURN false;
        END IF;
        SELECT val_seq INTO aseq FROM types.value WHERE value_id = a;
        SELECT val_seq INTO bseq FROM types.value WHERE value_id = b;
        FOR i IN 1 .. types._count(a)
        LOOP
            IF NOT types._equal_Q(aseq[i], bseq[i]) THEN
                return false;
            END IF;
        END LOOP;
        RETURN true;
    WHEN atype = 10 THEN -- hash-map
        SELECT val_hash INTO ahash FROM types.value WHERE value_id = a;
        SELECT val_hash INTO bhash FROM types.value WHERE value_id = b;
        IF array_length(akeys(ahash), 1) <> array_length(akeys(bhash), 1) THEN
            RETURN false;
        END IF;
        FOR kv IN SELECT * FROM each(ahash) LOOP
            avid := CAST((ahash -> kv.key) AS integer);
            bvid := CAST((bhash -> kv.key) AS integer);
            IF bvid IS NULL OR NOT types._equal_Q(avid, bvid) THEN
                return false;
            END IF;
        END LOOP;
        RETURN true;
    ELSE
        RETURN a = b;
    END CASE;
END; $$ LANGUAGE plpgsql;


-- _clone:
-- take a value_id of a collection
-- returns a new value_id of a cloned collection
CREATE FUNCTION types._clone(id integer) RETURNS integer AS $$
DECLARE
    result       integer;
BEGIN
    INSERT INTO types.value (type_id,val_int,val_string,val_seq,val_hash,
                       ast_id,params_id,env_id,meta_id)
        (SELECT type_id,val_int,val_string,val_seq,val_hash,
                ast_id,params_id,env_id,meta_id
              FROM types.value
              WHERE value_id = id)
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- scalar functions


-- _nil_Q:
-- takes a value_id
-- returns the whether value_id is nil
CREATE FUNCTION types._nil_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN id = 0;
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- _true_Q:
-- takes a value_id
-- returns the whether value_id is true
CREATE FUNCTION types._true_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN id = 2;
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- _false_Q:
-- takes a value_id
-- returns the whether value_id is false
CREATE FUNCTION types._false_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN id = 1;
END; $$ LANGUAGE plpgsql IMMUTABLE;

-- _string_Q:
-- takes a value_id
-- returns the whether value_id is string type
CREATE FUNCTION types._string_Q(id integer) RETURNS boolean AS $$
BEGIN
    IF (SELECT 1 FROM types.value WHERE type_id = 5 AND value_id = id) THEN
        RETURN NOT types._keyword_Q(id);
    END IF;
    RETURN false;
END; $$ LANGUAGE plpgsql;


-- _valueToString:
-- takes a value_id for a string
-- returns the varchar value of the string
CREATE FUNCTION types._valueToString(sid integer) RETURNS varchar AS $$
BEGIN
    RETURN (SELECT val_string FROM types.value WHERE value_id = sid);
END; $$ LANGUAGE plpgsql;

-- _stringish:
-- takes a varchar string
-- returns the value_id of a stringish type (string, symbol, keyword)
CREATE FUNCTION types._stringish(str varchar, type integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    -- TODO: share string data between string types
    -- lookup if it exists
    SELECT value_id FROM types.value INTO result
        WHERE val_string = str AND type_id = type;
    IF result IS NULL THEN
        -- Create string entry
        INSERT INTO types.value (type_id, val_string)
            VALUES (type, str)
            RETURNING value_id INTO result;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _stringv:
-- takes a varchar string
-- returns the value_id of a string (new or existing)
CREATE FUNCTION types._stringv(str varchar) RETURNS integer AS $$
BEGIN
    RETURN types._stringish(str, 5);
END; $$ LANGUAGE plpgsql;

-- _keywordv:
-- takes a varchar string
-- returns the value_id of a keyword (new or existing)
CREATE FUNCTION types._keywordv(name varchar) RETURNS integer AS $$
BEGIN
    RETURN types._stringish(chr(CAST(x'7f' AS integer)) || name, 5);
END; $$ LANGUAGE plpgsql;

-- _keyword_Q:
-- takes a value_id
-- returns the whether value_id is keyword type
CREATE FUNCTION types._keyword_Q(id integer) RETURNS boolean AS $$
DECLARE
    str  varchar;
BEGIN
    IF (SELECT 1 FROM types.value WHERE type_id = 5 AND value_id = id) THEN
        str := types._valueToString(id);
        IF char_length(str) > 0 AND
           chr(CAST(x'7f' AS integer)) = substring(str FROM 1 FOR 1) THEN
            RETURN true;
        END IF;
    END IF;
    RETURN false;
END; $$ LANGUAGE plpgsql;

-- _symbolv:
-- takes a varchar string
-- returns the value_id of a symbol (new or existing)
CREATE FUNCTION types._symbolv(name varchar) RETURNS integer AS $$
BEGIN
    RETURN types._stringish(name, 7);
END; $$ LANGUAGE plpgsql;

-- _symbol_Q:
-- takes a value_id
-- returns the whether value_id is symbol type
CREATE FUNCTION types._symbol_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN types._tf((SELECT 1 FROM types.value
            WHERE type_id = 7 AND value_id = id));
END; $$ LANGUAGE plpgsql;

-- _numToValue:
-- takes an bigint number
-- returns the value_id for the number
CREATE FUNCTION types._numToValue(num bigint) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    SELECT value_id FROM types.value INTO result
        WHERE val_int = num AND type_id = 3;
    IF result IS NULL THEN
        -- Create an integer entry
        INSERT INTO types.value (type_id, val_int)
            VALUES (3, num)
            RETURNING value_id INTO result;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------
-- sequence functions

-- _sequential_Q:
-- return true if obj value_id is a list or vector
CREATE FUNCTION types._sequential_Q(obj integer) RETURNS boolean AS $$
BEGIN
    RETURN types._tf((SELECT 1 FROM types.value
                WHERE value_id = obj AND (type_id = 8 OR type_id = 9)));
END; $$ LANGUAGE plpgsql;

-- _collection:
-- takes a array of value_id integers
-- returns the value_id of a new list (8), vector (9) or hash-map (10)
CREATE FUNCTION types._collection(items integer[], type integer) RETURNS integer AS $$
DECLARE
    vid  integer;
BEGIN
    IF type IN (8, 9) THEN
        INSERT INTO types.value (type_id, val_seq)
            VALUES (type, items)
            RETURNING value_id INTO vid;
    ELSIF type = 10 THEN
        IF (array_length(items, 1) % 2) = 1 THEN
            RAISE EXCEPTION 'hash-map: odd number of arguments';
        END IF;
        INSERT INTO types.value (type_id, val_hash)
            VALUES (type, hstore(CAST(items AS varchar[])))
            RETURNING value_id INTO vid;
    END IF;
    RETURN vid;
END; $$ LANGUAGE plpgsql;


-- _list:
-- takes a array of value_id integers
-- returns the value_id of a new list
CREATE FUNCTION types._list(items integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._collection(items, 8);
END; $$ LANGUAGE plpgsql;

-- _vector:
-- takes a array of value_id integers
-- returns the value_id of a new list
CREATE FUNCTION types._vector(items integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._collection(items, 9);
END; $$ LANGUAGE plpgsql;

-- _list_Q:
-- return true if obj value_id is a list
CREATE FUNCTION types._list_Q(obj integer) RETURNS boolean AS $$
BEGIN
    RETURN types._tf((SELECT 1 FROM types.value
            WHERE value_id = obj and type_id = 8));
END; $$ LANGUAGE plpgsql;

-- _vector_Q:
-- return true if obj value_id is a list
CREATE FUNCTION types._vector_Q(obj integer) RETURNS boolean AS $$
BEGIN
    RETURN types._tf((SELECT 1 FROM types.value
            WHERE value_id = obj and type_id = 9));
END; $$ LANGUAGE plpgsql;


-- _valueToArray:
-- takes an value_id referring to a list or vector
-- returns an array of the value_ids from the list/vector
CREATE FUNCTION types._valueToArray(seq integer) RETURNS integer[] AS $$
DECLARE
    result  integer[];
BEGIN
    result := (SELECT val_seq FROM types.value WHERE value_id = seq);
    IF result IS NULL THEN
        result := ARRAY[]::integer[];
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- From: https://wiki.postgresql.org/wiki/Array_reverse
CREATE FUNCTION types.array_reverse(a integer[]) RETURNS integer[] AS $$
SELECT ARRAY(
    SELECT a[i]
    FROM generate_subscripts(a,1) AS s(i)
    ORDER BY i DESC
);
$$ LANGUAGE 'sql' STRICT IMMUTABLE;


-- _nth:
-- takes value_id and an index
-- returns the value_id of nth element in list/vector
CREATE FUNCTION types._nth(seq_id integer, indx integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    RETURN (SELECT val_seq[indx+1] FROM types.value WHERE value_id = seq_id);
END; $$ LANGUAGE plpgsql;

-- _first:
-- takes value_id
-- returns the value_id of first element in list/vector
CREATE FUNCTION types._first(seq_id integer) RETURNS integer AS $$
BEGIN
    RETURN types._nth(seq_id, 0);
END; $$ LANGUAGE plpgsql;


-- _restArray:
-- takes value_id
-- returns the array of value_ids
CREATE FUNCTION types._restArray(seq_id integer) RETURNS integer[] AS $$
DECLARE
    result  integer[];
BEGIN
    result := (SELECT val_seq FROM types.value WHERE value_id = seq_id);
    RETURN result[2:array_length(result, 1)];
END; $$ LANGUAGE plpgsql;

-- _slice:
-- takes value_id, a first index and an last index
-- returns the value_id of new list from first (inclusive) to last (exclusive)
CREATE FUNCTION types._slice(seq_id integer, first integer, last integer)
RETURNS integer AS $$
DECLARE
    seq            integer[];
    vid            integer;
    i              integer;
    result         integer;
BEGIN
    SELECT val_seq INTO seq FROM types.value WHERE value_id = seq_id;
    INSERT INTO types.value (type_id, val_seq)
        VALUES (8, seq[first+1:last])
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _rest:
-- takes value_id
-- returns the value_id of new list
CREATE FUNCTION types._rest(seq_id integer) RETURNS integer AS $$
BEGIN
    RETURN types._slice(seq_id, 1, types._count(seq_id));
END; $$ LANGUAGE plpgsql;

-- _count:
-- takes value_id
-- returns a count (not value_id)
CREATE FUNCTION types._count(seq_id integer) RETURNS integer AS $$
DECLARE
    result  integer[];
BEGIN
    result := (SELECT val_seq FROM types.value
                         WHERE value_id = seq_id);
    RETURN COALESCE(array_length(result, 1), 0);
END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- hash-map functions

-- _hash_map:
-- return value_id of a new hash-map
CREATE FUNCTION types._hash_map(items integer[]) RETURNS integer AS $$
BEGIN
    RETURN types._collection(items, 10);
END; $$ LANGUAGE plpgsql;

-- _hash_map_Q:
-- return true if obj value_id is a list
CREATE FUNCTION types._hash_map_Q(obj integer) RETURNS boolean AS $$
BEGIN
    RETURN types._tf((SELECT 1 FROM types.value
            WHERE value_id = obj and type_id = 10));
END; $$ LANGUAGE plpgsql;

-- _assoc_BANG:
-- return value_id of the hash-map with new elements appended
CREATE FUNCTION types._assoc_BANG(hm integer, items integer[]) RETURNS integer AS $$
DECLARE
    hash  hstore;
BEGIN
    IF (array_length(items, 1) % 2) = 1 THEN
        RAISE EXCEPTION 'hash-map: odd number of arguments';
    END IF;
    SELECT val_hash INTO hash FROM types.value WHERE value_id = hm;
    IF hash IS NULL THEN
        UPDATE types.value SET val_hash = hstore(CAST(items AS varchar[]))
            WHERE value_id = hm;
    ELSE
        UPDATE types.value
            SET val_hash = hash || hstore(CAST(items AS varchar[]))
            WHERE value_id = hm;
    END IF;
    RETURN hm;
END; $$ LANGUAGE plpgsql;

-- _dissoc_BANG:
-- return value_id of the hash-map with elements removed
CREATE FUNCTION types._dissoc_BANG(hm integer, items integer[]) RETURNS integer AS $$
DECLARE
    hash  hstore;
BEGIN
    SELECT val_hash INTO hash FROM types.value WHERE value_id = hm;
    UPDATE types.value SET val_hash = hash - CAST(items AS varchar[])
            WHERE value_id = hm;
    RETURN hm;
END; $$ LANGUAGE plpgsql;

-- _get:
-- return value_id of the hash-map entry matching key
CREATE FUNCTION types._get(hm integer, key varchar) RETURNS integer AS $$
DECLARE
    hash  hstore;
BEGIN
    SELECT val_hash INTO hash FROM types.value WHERE value_id = hm;
    RETURN hash -> CAST(types._stringv(key) AS varchar);
END; $$ LANGUAGE plpgsql;

-- _contains_Q:
-- return true if hash-map contains entry matching key
CREATE FUNCTION types._contains_Q(hm integer, key varchar) RETURNS boolean AS $$
DECLARE
    hash  hstore;
BEGIN
    SELECT val_hash INTO hash FROM types.value WHERE value_id = hm;
    RETURN types._tf(hash ? CAST(types._stringv(key) AS varchar));
END; $$ LANGUAGE plpgsql;

-- _keys:
-- return array of key value_ids from hash-map
CREATE FUNCTION types._keys(hm integer) RETURNS integer[] AS $$
DECLARE
    hash  hstore;
BEGIN
    SELECT val_hash INTO hash FROM types.value WHERE value_id = hm;
    RETURN CAST(akeys(hash) AS integer[]);
END; $$ LANGUAGE plpgsql;

-- _vals:
-- return array of value value_ids from hash-map
CREATE FUNCTION types._vals(hm integer) RETURNS integer[] AS $$
DECLARE
    hash  hstore;
BEGIN
    SELECT val_hash INTO hash FROM types.value WHERE value_id = hm;
    RETURN CAST(avals(hash) AS integer[]);
END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- function functions

-- _function:
-- takes a function name
-- returns the value_id of a new 
CREATE FUNCTION types._function(fname varchar)
RETURNS varchar AS $$
DECLARE
    result  integer;
BEGIN
    INSERT INTO types.value (type_id, val_string)
        VALUES (11, fname)
        RETURNING value_id INTO result;
    RETURN CAST(result AS varchar);
END; $$ LANGUAGE plpgsql;

-- _malfunc:
-- takes a ast value_id, params value_id and env_id
-- returns the value_id of a new function
CREATE FUNCTION types._malfunc(ast integer, params integer, env integer)
RETURNS integer AS $$
DECLARE
    cid     integer = NULL;
    result  integer;
BEGIN
    -- Create function entry
    INSERT INTO types.value (type_id, ast_id, params_id, env_id)
        VALUES (12, ast, params, env)
        RETURNING value_id into result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _macro:
CREATE FUNCTION types._macro(func integer) RETURNS integer AS $$
DECLARE
    newfunc  integer;
    cid      integer;
BEGIN
    newfunc := types._clone(func);
    UPDATE types.value SET macro = true WHERE value_id = newfunc;
    RETURN newfunc;
END; $$ LANGUAGE plpgsql;

CREATE FUNCTION types._apply(func integer, args integer[]) RETURNS integer AS $$
DECLARE
    type     integer;
    fcid     integer;
    fname    varchar;
    fast     integer;
    fparams  integer;
    fenv     integer;
    result   integer;
BEGIN
    SELECT type_id, val_string, ast_id, params_id, env_id
        INTO type, fname, fast, fparams, fenv
        FROM types.value WHERE value_id = func;
    IF type = 11 THEN
        EXECUTE format('SELECT %s($1);', fname)
            INTO result USING args;
        RETURN result;
    ELSIF type = 12 THEN
        -- NOTE: forward reference to current step EVAL function
        RETURN mal.EVAL(fast, envs.new(fenv, fparams, args));
    ELSE
        RAISE EXCEPTION 'Invalid function call';
    END IF;
END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------
-- atom functions

-- _atom:
-- takes an ast value_id
-- returns a new atom value_id
CREATE FUNCTION types._atom(val integer) RETURNS integer AS $$
DECLARE
    cid     integer = NULL;
    result  integer;
BEGIN
    -- Create atom
    INSERT INTO types.value (type_id, val_seq)
        VALUES (13, ARRAY[val])
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _atom_Q:
-- takes a value_id
-- returns the whether value_id is an atom
CREATE FUNCTION types._atom_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN EXISTS(SELECT 1 FROM types.value
        WHERE type_id = 13 AND value_id = id);
END; $$ LANGUAGE plpgsql;

-- _deref:
-- takes an atom value_id
-- returns a atom value value_id
CREATE FUNCTION types._deref(atm integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    RETURN (SELECT val_seq[1] FROM types.value WHERE value_id = atm);
END; $$ LANGUAGE plpgsql;

-- _reset_BANG:
-- takes an atom value_id and new value value_id
-- returns a new value value_id
CREATE FUNCTION types._reset_BANG(atm integer, newval integer) RETURNS integer AS $$
BEGIN
    UPDATE types.value SET val_seq = ARRAY[newval] WHERE value_id = atm;
    RETURN newval;
END; $$ LANGUAGE plpgsql;
