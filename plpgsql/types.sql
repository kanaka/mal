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
    value_id        integer            -- set for all items
);
-- ALTER TABLE collection ADD CONSTRAINT pk_collection
--     PRIMARY KEY (collection_id, idx, key_string_id);
ALTER TABLE collection ADD CONSTRAINT fk_key_string_id
    FOREIGN KEY (key_string_id) REFERENCES string(string_id);
-- value_id foreign key is after value table


-- persistent values

CREATE SEQUENCE value_id_seq START WITH 3; -- skip nil, false, true
CREATE TABLE value (
    value_id        integer NOT NULL DEFAULT nextval('value_id_seq'),
    type_id         integer NOT NULL,
    val_int         integer,  -- set for integers
    val_string_id   integer,  -- set for strings, keywords, and symbols
    collection_id   integer,  -- set for lists, vectors and hashmaps
                              -- (NULL for empty collection)
    function_name   varchar   -- set for function types
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
-- Reference from collection back to value
ALTER TABLE collection ADD CONSTRAINT fk_value_id
    FOREIGN KEY (value_id) REFERENCES value(value_id);

INSERT INTO value (value_id, type_id) VALUES (0, 0); -- nil
INSERT INTO value (value_id, type_id) VALUES (1, 1); -- false
INSERT INTO value (value_id, type_id) VALUES (2, 2); -- true


-- ---------------------------------------------------------
-- scalar functions


-- _symbol:
-- takes a varchar string
-- returns the value_id of a new symbol
CREATE OR REPLACE FUNCTION
    _symbol(name varchar) RETURNS integer AS $$
DECLARE
    str_id  integer;
    result  integer;
BEGIN
    -- Create string value for name
    INSERT INTO string (value)
        VALUES (name)
        RETURNING string_id INTO str_id;
    -- Create symbol entry
    INSERT INTO value (type_id, val_string_id) VALUES (7, str_id)
        RETURNING value_id INTO result;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- _symbol_Q:
-- takes a value_id
-- returns the whether value_id is symbol type
CREATE OR REPLACE FUNCTION
    _symbol_Q(id integer) RETURNS boolean AS $$
BEGIN
    RETURN EXISTS(SELECT 1 FROM value WHERE type_id = 7 AND value_id = id);
END; $$ LANGUAGE plpgsql;


-- ---------------------------------------------------------
-- sequence functions

-- _list:
-- takes a array of value_id integers
-- returns the value_id of a new list
CREATE OR REPLACE FUNCTION
    _list(items integer[]) RETURNS integer AS $$
DECLARE
    collection_id  integer = NULL;
    list_id        integer;
BEGIN
    IF array_length(items, 1) > 0 THEN
        collection_id := COALESCE((SELECT Max(value_id) FROM value)+1,0);
        FOR idx IN array_lower(items, 1) .. array_upper(items, 1)
        LOOP
            -- Create entries
            INSERT INTO collection (collection_id, idx, value_id)
                VALUES (collection_id, idx, items[idx]);
        END LOOP;
    END IF;
    -- Create value entry pointing to collection (or NULL)
    INSERT INTO value (type_id, collection_id)
        VALUES (8, collection_id)
        RETURNING value_id INTO list_id;
    RETURN list_id;
END; $$ LANGUAGE plpgsql;

-- _list2:
-- takes two value_id integers
-- returns the value_id of a new list
CREATE OR REPLACE FUNCTION
    _list2(a integer, b integer) RETURNS integer AS $$
DECLARE
    collection_id  integer = NULL;
    list_id        integer;
BEGIN
    collection_id := COALESCE((SELECT Max(value_id) FROM value)+1,0);
    -- Create entries
    INSERT INTO collection (collection_id, idx, value_id)
        VALUES (collection_id, 0, a);
    INSERT INTO collection (collection_id, idx, value_id)
        VALUES (collection_id, 1, b);

    -- Create value entry pointing to collection
    INSERT INTO value (type_id, collection_id)
        VALUES (8, collection_id)
        RETURNING value_id INTO list_id;
    RETURN list_id;
END; $$ LANGUAGE plpgsql;

-- _list3:
-- takes three value_id integers
-- returns the value_id of a new list
CREATE OR REPLACE FUNCTION
    _list2(a integer, b integer, c integer) RETURNS integer AS $$
DECLARE
    collection_id  integer = NULL;
    list_id        integer;
BEGIN
    collection_id := COALESCE((SELECT Max(value_id) FROM value)+1,0);
    -- Create entries
    INSERT INTO collection (collection_id, idx, value_id)
        VALUES (collection_id, 0, a);
    INSERT INTO collection (collection_id, idx, value_id)
        VALUES (collection_id, 1, b);
    INSERT INTO collection (collection_id, idx, value_id)
        VALUES (collection_id, 2, c);

    -- Create value entry pointing to collection
    INSERT INTO value (type_id, collection_id)
        VALUES (8, collection_id)
        RETURNING value_id INTO list_id;
    RETURN list_id;
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
DECLARE
    result  integer;
BEGIN
    RETURN ARRAY(SELECT value_id FROM collection
                 WHERE collection_id = (SELECT collection_id FROM value
                                        WHERE value_id = seq_id)
                 AND idx > 0);
END; $$ LANGUAGE plpgsql;
