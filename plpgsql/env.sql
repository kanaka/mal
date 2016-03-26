-- env table
CREATE SEQUENCE env_id_seq;
CREATE TABLE env (
    env_id    integer NOT NULL DEFAULT nextval('env_id_seq'),
    outer_id  integer
);
ALTER TABLE env ADD CONSTRAINT pk_env_id
    PRIMARY KEY (env_id);
-- drop sequence when table dropped
ALTER SEQUENCE env_id_seq OWNED BY env.env_id;
ALTER TABLE env ADD CONSTRAINT fk_env_outer_id
    FOREIGN KEY (outer_id) REFERENCES env(env_id);


CREATE TABLE env_data (
    env_id    integer NOT NULL,
    key       varchar NOT NULL,
    value_id  integer NOT NULL
);
ALTER TABLE env_data ADD CONSTRAINT fk_env_data_env_id
    FOREIGN KEY (env_id) REFERENCES env(env_id);

-- -----------------------

-- env_new
CREATE FUNCTION env_new(outer_env integer)
    RETURNS integer AS $$
DECLARE
    e  integer;
BEGIN
    INSERT INTO env (outer_id) VALUES (outer_env)
        RETURNING env_id INTO e;
    --RAISE NOTICE 'env_new: e: %, outer_env: %', e, outer_env;
    RETURN e;
END; $$ LANGUAGE plpgsql;

-- env_new_bindings
CREATE FUNCTION env_new_bindings(outer_env integer,
                                            binds integer,
                                            exprs integer[])
    RETURNS integer AS $$
DECLARE
    e     integer;
    cid   integer;
    i     integer;
    bind  integer;
    bsym  varchar;
    expr  integer;
BEGIN
    e := env_new(outer_env);
    SELECT collection_id INTO cid FROM value
        WHERE value_id = binds;
    FOR bind, i IN (SELECT value_id, idx FROM collection
                    WHERE collection_id = cid
                    ORDER BY idx)
    LOOP
        expr := exprs[i+1];
        bsym := _valueToString(bind);
        --RAISE NOTICE 'i: %, bind: %, expr: %', i, bind, expr;
        IF bsym = '&' THEN
            bind := (SELECT value_id FROM collection
                     WHERE collection_id = cid
                     AND idx = i+1);
            PERFORM env_set(e, bind,
                            _collection(exprs[i+1:array_length(exprs, 1)], 8));
            RETURN e;
        END IF;
        PERFORM env_vset(e, bsym, expr);
    END LOOP;
    RETURN e;
END; $$ LANGUAGE plpgsql;


-- env_vset
-- like env_set but takes a varchar key instead of value_id
CREATE FUNCTION env_vset(env integer, name varchar, val integer)
    RETURNS integer AS $$
BEGIN
    -- upsert
    IF (SELECT 1 FROM env_data WHERE env_id=env AND env_data.key=name) THEN
        UPDATE env_data SET value_id = val
            WHERE env_id=env AND env_data.key=name;
    ELSE
        INSERT INTO env_data (env_id, key, value_id)
            VALUES (env, name, val);
    END IF;
    RETURN val;
END; $$ LANGUAGE plpgsql;


-- env_set
CREATE FUNCTION env_set(env integer, key integer, val integer)
    RETURNS integer AS $$
DECLARE
    symkey  varchar;
BEGIN
    symkey := _valueToString(key);
    RETURN env_vset(env, symkey, val);
END; $$ LANGUAGE plpgsql;

-- env_find
CREATE FUNCTION env_find(env integer, symkey varchar)
    RETURNS integer AS $$
DECLARE
    outer_id  integer;
    val       integer;
BEGIN
    SELECT e.outer_id INTO outer_id FROM env e WHERE e.env_id = env;
    SELECT value_id INTO val FROM env_data
        WHERE env_id = env AND env_data.key = symkey;
    --RAISE NOTICE 'env_find symkey: %, env: %, outer_id: %, val: %', symkey, env, outer_id, val;
    IF val IS NOT NULL THEN
        RETURN env;
    ELSIF outer_id IS NOT NULL THEN
        --RAISE NOTICE 'symkey: %, not found in: %, trying: %', symkey, env, outer_id;
        RETURN env_find(outer_id, symkey);
    ELSE
        RETURN NULL;
    END IF;
END; $$ LANGUAGE plpgsql;


-- env_vget
CREATE FUNCTION env_vget(env integer, symkey varchar)
    RETURNS integer AS $$
DECLARE
    result  integer;
    e       integer;
BEGIN
    e := env_find(env, symkey);
    --RAISE NOTICE 'env_find env: %, symkey: % -> e: %', env, symkey, e;
    IF e IS NULL THEN
        RAISE EXCEPTION '''%'' not found', symkey;
    ELSE
        SELECT env_data.value_id
            INTO result
            FROM env_data
            WHERE env_data.env_id = e
            AND env_data.key = symkey;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- env_get
CREATE FUNCTION env_get(env integer, key integer)
    RETURNS integer AS $$
DECLARE
    symkey  varchar;
    result  integer;
    e       integer;
BEGIN
    RETURN env_vget(env, _valueToString(key));
END; $$ LANGUAGE plpgsql;


-- env_print
-- For debugging
CREATE FUNCTION env_print(env integer)
    RETURNS void AS $$
DECLARE
    k  varchar;
    v  integer;
BEGIN
    RAISE NOTICE 'env: %, outer: %', env,
        (SELECT e.outer_id FROM env e WHERE e.env_id = env);
    FOR k,v in (SELECT key, value_id FROM env_data WHERE env_id = env)
    LOOP
        RAISE NOTICE 'key: %, value: %', k, pr_str(v);
    END LOOP;
END; $$ LANGUAGE plpgsql;

