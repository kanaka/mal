-- ---------------------------------------------------------
-- list of types

CREATE TABLE type (
    type_id  integer NOT NULL,
    name     char(10)
);
ALTER TABLE type ADD CONSTRAINT pk_types_id
    PRIMARY KEY (type_id);

INSERT INTO type VALUES (0, 'nil');
INSERT INTO type VALUES (1, 'false');
INSERT INTO type VALUES (2, 'true');
INSERT INTO type VALUES (3, 'integer');
INSERT INTO type VALUES (4, 'float');
INSERT INTO type VALUES (5, 'string');
INSERT INTO type VALUES (6, 'keyword');
INSERT INTO type VALUES (7, 'symbol');
INSERT INTO type VALUES (8, 'list');
INSERT INTO type VALUES (9, 'vector');
INSERT INTO type VALUES (10, 'hashmap');
INSERT INTO type VALUES (11, 'function');
INSERT INTO type VALUES (12, 'malfunc');
INSERT INTO type VALUES (13, 'atom');


-- string values ("interned")

CREATE SEQUENCE string_id_seq;
CREATE TABLE string (
    string_id  integer NOT NULL DEFAULT nextval('string_id_seq'),
    value      varchar(4096)
);
ALTER TABLE string ADD CONSTRAINT pk_string_id
    PRIMARY KEY (string_id);
-- drop sequence when table dropped
ALTER SEQUENCE string_id_seq OWNED BY string.string_id;


-- collections/groupings

CREATE TABLE collection (
    collection_id   integer NOT NULL,  -- same for items of a collection
    idx             integer,           -- set for list and vector items
    key_string_id   integer,           -- set for hashmap items
    value_id        integer,           -- set for all items (ast for functions)
    params_id       integer,           -- set for functions
    env_id          integer,           -- set for functions
    macro           boolean            -- set for macro functions
);
-- ALTER TABLE collection ADD CONSTRAINT pk_collection
--     PRIMARY KEY (collection_id, idx, key_string_id);
ALTER TABLE collection ADD CONSTRAINT fk_key_string_id
    FOREIGN KEY (key_string_id) REFERENCES string(string_id);
-- value_id, params_id foreign keys are after value table


-- persistent values

CREATE SEQUENCE value_id_seq START WITH 3; -- skip nil, false, true
CREATE TABLE value (
    value_id        integer NOT NULL DEFAULT nextval('value_id_seq'),
    type_id         integer NOT NULL,
    val_int         integer,  -- set for integers
    val_string_id   integer,  -- set for strings, keywords, and symbols
    collection_id   integer,  -- set for lists, vectors and hashmaps
                              -- (NULL for empty collection)
    function_name   varchar   -- set for native function types
);
ALTER TABLE value ADD CONSTRAINT pk_value_id
    PRIMARY KEY (value_id);
-- drop sequence when table dropped
ALTER SEQUENCE value_id_seq OWNED BY value.value_id;
ALTER TABLE value ADD CONSTRAINT fk_type_id
    FOREIGN KEY (type_id) REFERENCES type(type_id);
ALTER TABLE value ADD CONSTRAINT fk_val_string_id
    FOREIGN KEY (val_string_id) REFERENCES string(string_id);
-- ALTER TABLE value ADD CONSTRAINT fk_collection_id
--    FOREIGN KEY (collection_id) REFERENCES collection(collection_id, idx, key_string_id);
-- References from collection back to value
ALTER TABLE collection ADD CONSTRAINT fk_value_id
    FOREIGN KEY (value_id) REFERENCES value(value_id);
ALTER TABLE collection ADD CONSTRAINT fk_params_id
    FOREIGN KEY (params_id) REFERENCES value(value_id);

INSERT INTO value (value_id, type_id) VALUES (0, 0); -- nil
INSERT INTO value (value_id, type_id) VALUES (1, 1); -- false
INSERT INTO value (value_id, type_id) VALUES (2, 2); -- true


-- ---------------------------------------------------------
-- general functions

CREATE OR REPLACE FUNCTION
    _wraptf(val boolean) RETURNS integer AS $$
BEGIN
    IF val THEN
        RETURN 2;
    ELSE
        RETURN 1;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
    _equal_Q(a integer, b integer) RETURNS boolean AS $$
DECLARE
    atype  integer;
    btype  integer;
    aint   integer;
    bint   integer;
    astr   varchar;
    bstr   varchar;
    acid   integer;
    bcid   integer;
    i      integer;
BEGIN
    SELECT type_id INTO atype FROM value WHERE value_id = a;
    SELECT type_id INTO btype FROM value WHERE value_id = b;
    IF NOT ((atype = btype) OR (_sequential_Q(a) AND _sequential_Q(b))) THEN
        RETURN false;
    END IF;
    CASE
    WHEN atype = 3 THEN -- integer
        SELECT val_int FROM value INTO aint
            WHERE value_id = a;
        SELECT val_int FROM value INTO bint
            WHERE value_id = b;
        RETURN aint = bint;
    WHEN atype = 5 OR atype = 7 THEN -- string/symbol
        RETURN _vstring(a) = _vstring(b);
    WHEN atype = 8 OR atype = 9 THEN -- list/vector
        IF _count(a) <> _count(b) THEN
            RETURN false;
        END IF;
        SELECT collection_id FROM value INTO acid
            WHERE value_id = a;
        SELECT collection_id FROM value INTO bcid
            WHERE value_id = b;
        FOR i IN 0 .. _count(a)-1
        LOOP
            SELECT value_id INTO aint FROM collection
                WHERE collection_id = acid AND idx = i;
            SELECT value_id INTO bint FROM collection
                WHERE collection_id = bcid AND idx = i;
            IF NOT _equal_Q(aint, bint) THEN
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
CREATE OR REPLACE FUNCTION
    _clone(id integer) RETURNS integer AS $$
DECLARE
    src_coll_id  integer;
    dst_coll_id  integer;
    result       integer;
BEGIN
    SELECT collection_id FROM collection INTO src_coll_id
        WHERE collection_id = (SELECT collection_id FROM value
                               WHERE value_id = id);
    dst_coll_id := COALESCE((SELECT Max(collection_id) FROM collection)+1,0);

    -- copy collection and change collection_id
    INSERT INTO collection
        (collection_id,idx,key_string_id,value_id,params_id,env_id,macro)
        (SELECT dst_coll_id,idx,key_string_id,value_id,params_id,env_id,macro
            FROM collection
            WHERE collection_id = src_coll_id);

    -- copy value and change collection_id to new value
    INSERT INTO value (type_id,collection_id)
        (SELECT type_id,dst_coll_id
            FROM value
            WHERE value_id = id)
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- scalar functions


-- _nil_Q:
-- takes a value_id
-- returns the whether value_id is nil
CREATE OR REPLACE FUNCTION
    _nil_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN id = 0;
END; $$ LANGUAGE plpgsql;

-- _false_Q:
-- takes a value_id
-- returns the whether value_id is false
CREATE OR REPLACE FUNCTION
    _false_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN id = 1;
END; $$ LANGUAGE plpgsql;

-- _true_Q:
-- takes a value_id
-- returns the whether value_id is true
CREATE OR REPLACE FUNCTION
    _true_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN id = 2;
END; $$ LANGUAGE plpgsql;

-- _vstring:
-- takes a value_id for a string
-- returns the varchar value of the string
CREATE OR REPLACE FUNCTION
    _vstring(sid integer) RETURNS varchar AS $$
BEGIN
    RETURN (SELECT value FROM string
            WHERE string_id = (SELECT val_string_id
                           FROM value WHERE value_id = sid));
END; $$ LANGUAGE plpgsql;

-- _stringish:
-- takes a varchar string
-- returns the value_id of a stringish type (string, symbol, keyword)
CREATE OR REPLACE FUNCTION
    _stringish(str varchar, type integer) RETURNS integer AS $$
DECLARE
    str_id  integer;
    result  integer;
BEGIN
    -- TODO: share string data between string types
    -- lookup if it exists
    SELECT value_id FROM value INTO result
        INNER JOIN string ON value.val_string_id=string.string_id
        WHERE string.value = str AND value.type_id = type;
    IF result IS NULL THEN
        -- Create string value for string
        INSERT INTO string (value)
            VALUES (str)
            RETURNING string_id INTO str_id;
        -- Create actual string entry
        INSERT INTO value (type_id, val_string_id)
            VALUES (type, str_id)
            RETURNING value_id INTO result;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _stringv:
-- takes a varchar string
-- returns the value_id of a string (new or existing)
CREATE OR REPLACE FUNCTION
    _stringv(str varchar) RETURNS integer AS $$
BEGIN
    RETURN _stringish(str, 5);
END; $$ LANGUAGE plpgsql;

-- _symbolv:
-- takes a varchar string
-- returns the value_id of a symbol (new or existing)
CREATE OR REPLACE FUNCTION
    _symbolv(name varchar) RETURNS integer AS $$
BEGIN
    RETURN _stringish(name, 7);
END; $$ LANGUAGE plpgsql;

-- _symbol_Q:
-- takes a value_id
-- returns the whether value_id is symbol type
CREATE OR REPLACE FUNCTION
    _symbol_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN EXISTS(SELECT 1 FROM value WHERE type_id = 7 AND value_id = id);
END; $$ LANGUAGE plpgsql;

-- _numToValue:
-- takes an integer number
-- returns the value_id for the number
CREATE OR REPLACE FUNCTION
    _numToValue(num integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    INSERT INTO value (type_id, val_int)
        VALUES (3, num)
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------
-- sequence functions

-- _sequential_Q:
-- return true if obj value_id is a list or vector
CREATE OR REPLACE FUNCTION _sequential_Q(obj integer) RETURNS boolean AS $$
BEGIN
    IF (SELECT 1 FROM value
        WHERE value_id = obj AND (type_id = 8 OR type_id = 9)) THEN
        RETURN true;
    ELSE
        RETURN false;
    END IF;
END; $$ LANGUAGE plpgsql;

-- _seq:
-- takes a array of value_id integers
-- returns the value_id of a new list or vector
CREATE OR REPLACE FUNCTION
    _seq(items integer[], type integer) RETURNS integer AS $$
DECLARE
    cid      integer = NULL;
    list_id  integer;
BEGIN
    IF array_length(items, 1) > 0 THEN
        cid := COALESCE((SELECT Max(collection_id) FROM collection)+1,0);
        FOR idx IN array_lower(items, 1) .. array_upper(items, 1)
        LOOP
            -- Create entries
            INSERT INTO collection (collection_id, idx, value_id)
                VALUES (cid, idx-1, items[idx]);
        END LOOP;
    END IF;
    -- Create value entry pointing to collection (or NULL)
    INSERT INTO value (type_id, collection_id)
        VALUES (type, cid)
        RETURNING value_id INTO list_id;
    RETURN list_id;
END; $$ LANGUAGE plpgsql;

-- _list:
-- takes a array of value_id integers
-- returns the value_id of a new list
CREATE OR REPLACE FUNCTION
    _list(items integer[]) RETURNS integer AS $$
BEGIN
    RETURN _seq(items, 8);
END; $$ LANGUAGE plpgsql;

-- _vector:
-- takes a array of value_id integers
-- returns the value_id of a new list
CREATE OR REPLACE FUNCTION
    _vector(items integer[]) RETURNS integer AS $$
BEGIN
    RETURN _seq(items, 9);
END; $$ LANGUAGE plpgsql;

-- _list_Q:
-- return true if obj value_id is a list
CREATE OR REPLACE FUNCTION _list_Q(obj integer) RETURNS boolean AS $$
BEGIN
    IF (SELECT 1 FROM value WHERE value_id = obj and type_id = 8) THEN
        RETURN true;
    ELSE
        RETURN false;
    END IF;
END; $$ LANGUAGE plpgsql;

-- _vector_Q:
-- return true if obj value_id is a list
CREATE OR REPLACE FUNCTION _vector_Q(obj integer) RETURNS boolean AS $$
BEGIN
    IF (SELECT 1 FROM value WHERE value_id = obj and type_id = 9) THEN
        RETURN true;
    ELSE
        RETURN false;
    END IF;
END; $$ LANGUAGE plpgsql;


-- _valueToArray:
-- takes an value_id referring to a list or vector
-- returns an array of the value_ids from the list/vector
CREATE OR REPLACE FUNCTION
    _valueToArray(seq integer) RETURNS integer[] AS $$
BEGIN
    RETURN ARRAY(SELECT value_id FROM collection
                 WHERE collection_id = (SELECT collection_id FROM value
                                        WHERE value_id = seq));
END; $$ LANGUAGE plpgsql;


-- _nth:
-- takes value_id and an index
-- returns the value_id of nth element in list/vector
CREATE OR REPLACE FUNCTION
    _nth(seq_id integer, indx integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    SELECT value_id INTO result FROM collection
        WHERE collection_id = (SELECT collection_id FROM value
                               WHERE value_id = seq_id)
        AND idx = indx;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _first:
-- takes value_id
-- returns the value_id of first element in list/vector
CREATE OR REPLACE FUNCTION
    _first(seq_id integer) RETURNS integer AS $$
BEGIN
    RETURN _nth(seq_id, 0);
END; $$ LANGUAGE plpgsql;


-- _restArray:
-- takes value_id
-- returns the array of value_ids
CREATE OR REPLACE FUNCTION
    _restArray(seq_id integer) RETURNS integer[] AS $$
BEGIN
    RETURN ARRAY(SELECT value_id FROM collection
                 WHERE collection_id = (SELECT collection_id FROM value
                                        WHERE value_id = seq_id)
                 AND idx > 0);
END; $$ LANGUAGE plpgsql;

-- _slice:
-- takes value_id, a first index and an last index
-- returns the value_id of new list from first (inclusive) to last (exclusive)
CREATE OR REPLACE FUNCTION
    _slice(seq_id integer, first integer, last integer) RETURNS integer AS $$
DECLARE
    dst_coll_id    integer = NULL;
    vid            integer;
    i              integer;
    result         integer;
BEGIN
    FOR vid, i IN (SELECT value_id, idx FROM collection
                   WHERE collection_id = (SELECT collection_id FROM value
                                          WHERE value_id = seq_id)
                   AND idx >= first AND idx < last
                   ORDER BY idx)
    LOOP
        IF dst_coll_id IS NULL THEN
            dst_coll_id := COALESCE((SELECT Max(collection_id) FROM collection)+1,0);
        END IF;
        INSERT INTO collection (collection_id, idx, value_id)
            VALUES (dst_coll_id, i-1, vid);
    END LOOP;
    INSERT INTO value (type_id, collection_id)
        VALUES (8, dst_coll_id)
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _rest:
-- takes value_id
-- returns the value_id of new list
CREATE OR REPLACE FUNCTION
    _rest(seq_id integer) RETURNS integer AS $$
DECLARE
BEGIN
    RETURN _slice(seq_id, 1, _count(seq_id));
END; $$ LANGUAGE plpgsql;

-- _count:
-- takes value_id
-- returns a count (not value_id)
CREATE OR REPLACE FUNCTION
    _count(seq_id integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    SELECT count(*) INTO result FROM collection
        WHERE collection_id = (SELECT collection_id FROM value
                               WHERE value_id = seq_id);
    RETURN result;
END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- function functions

-- _function:
-- takes a ast value_id, params value_id and env_id
-- returns the value_id of a new function
CREATE OR REPLACE FUNCTION
    _function(ast integer, params integer, env integer) RETURNS integer AS $$
DECLARE
    cid     integer = NULL;
    result  integer;
BEGIN
    cid := COALESCE((SELECT Max(collection_id) FROM collection)+1,0);
    -- Create function entry
    INSERT INTO collection (collection_id, value_id, params_id, env_id)
        VALUES (cid, ast, params, env);
    INSERT INTO value (type_id, collection_id)
        VALUES (12, cid)
        RETURNING value_id into result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _macro:
CREATE OR REPLACE FUNCTION
    _macro(func integer) RETURNS integer AS $$
DECLARE
    newfunc  integer;
    cid      integer;
BEGIN
    newfunc := _clone(func);
    SELECT collection_id FROM value INTO cid WHERE value_id = newfunc;
    UPDATE collection SET macro = true
        WHERE collection_id = cid;
    RETURN newfunc;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
    _apply(func integer, args integer[]) RETURNS integer AS $$
DECLARE
    type     integer;
    fcid     integer;
    fname    varchar;
    fast     integer;
    fparams  integer;
    fenv     integer;
    result   integer;
BEGIN
    SELECT type_id, collection_id, function_name
        INTO type, fcid, fname
        FROM value WHERE value_id = func;
    IF type = 11 THEN
        EXECUTE format('SELECT %s($1);', fname)
            INTO result USING args;
        RETURN result;
    ELSIF type = 12 THEN
        SELECT value_id, params_id, env_id
            INTO fast, fparams, fenv
            FROM collection
            WHERE collection_id = fcid;
        -- NOTE: forward reference to current step EVAL function
        RETURN EVAL(fast, env_new_bindings(fenv, fparams, args));
    ELSE
        RAISE EXCEPTION 'Invalid function call';
    END IF;
END; $$ LANGUAGE plpgsql;

-- ---------------------------------------------------------
-- atom functions

-- _atom:
-- takes an ast value_id
-- returns a new atom value_id
CREATE OR REPLACE FUNCTION
    _atom(val integer) RETURNS integer AS $$
DECLARE
    cid     integer = NULL;
    result  integer;
BEGIN
    cid := COALESCE((SELECT Max(collection_id) FROM collection)+1,0);
    -- Create function entry
    INSERT INTO collection (collection_id, value_id) VALUES (cid, val);
    INSERT INTO value (type_id, collection_id) VALUES (13, cid)
        RETURNING value_id into result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _atom_Q:
-- takes a value_id
-- returns the whether value_id is an atom
CREATE OR REPLACE FUNCTION
    _atom_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN EXISTS(SELECT 1 FROM value WHERE type_id = 13 AND value_id = id);
END; $$ LANGUAGE plpgsql;

-- _deref:
-- takes an atom value_id
-- returns a atom value value_id
CREATE OR REPLACE FUNCTION
    _deref(atm integer) RETURNS integer AS $$
DECLARE
    result  integer;
BEGIN
    SELECT value_id INTO result FROM collection
        WHERE collection_id = (SELECT collection_id FROM value
                               WHERE value_id = atm);
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _reset_BANG:
-- takes an atom value_id and new value value_id
-- returns a new value value_id
CREATE OR REPLACE FUNCTION
    _reset_BANG(atm integer, newval integer) RETURNS integer AS $$
BEGIN
    UPDATE collection SET value_id = newval
        WHERE collection_id = (SELECT collection_id FROM value
                               WHERE value_id = atm);
    RETURN newval;
END; $$ LANGUAGE plpgsql;
