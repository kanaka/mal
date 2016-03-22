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

CREATE OR REPLACE FUNCTION env_new(outer_env integer)
    RETURNS integer AS $$
DECLARE
    e  integer;
BEGIN
    INSERT INTO env (outer_id) VALUES (outer_env)
        RETURNING env_id INTO e;
    --RAISE NOTICE 'env_new: e: %, outer_env: %', e, outer_env;
    RETURN e;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION env_set(env integer, key integer, val integer)
    RETURNS integer AS $$
DECLARE
    symkey  varchar;
BEGIN
    symkey := (SELECT value FROM string
               WHERE string_id = (SELECT val_string_id FROM value
                                  WHERE value_id = key));
    -- upsert
    IF (SELECT 1 FROM env_data WHERE env_id=env AND env_data.key=symkey) THEN
        UPDATE env_data SET value_id = val
            WHERE env_id=env AND env_data.key=symkey;
    ELSE
        INSERT INTO env_data (env_id, key, value_id)
            VALUES (env, symkey, val);
    END IF;
    RETURN val;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION env_find(env integer, key integer)
    RETURNS integer AS $$
DECLARE
    symkey    varchar;
    outer_id  integer;
    val       integer;
BEGIN
    symkey := (SELECT value FROM string
               WHERE string_id = (SELECT val_string_id FROM value
                                  WHERE value_id = key));
    SELECT e.outer_id INTO outer_id FROM env e WHERE e.env_id = env;
    SELECT value_id INTO val FROM env_data
        WHERE env_id = env AND env_data.key = symkey;
    --RAISE NOTICE 'env_find symkey: %, env: %, outer_id: %, val: %', symkey, env, outer_id, val;
    IF val IS NOT NULL THEN
        RETURN env;
    ELSIF outer_id IS NOT NULL THEN
        --RAISE NOTICE 'symkey: %, not found in: %, trying: %', symkey, env, outer_id;
        RETURN env_find(outer_id, key);
    ELSE
        RETURN NULL;
    END IF;
END; $$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION env_get(env integer, key integer)
    RETURNS integer AS $$
DECLARE
    symkey  varchar;
    result  integer;
    e       integer;
BEGIN
    symkey := (SELECT value FROM string
               WHERE string_id = (SELECT val_string_id FROM value
                                  WHERE value_id = key));
    e := env_find(env, key);
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

CREATE OR REPLACE FUNCTION env_print(env integer)
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

