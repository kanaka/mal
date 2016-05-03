-- ---------------------------------------------------------
-- envs.sql

CREATE SCHEMA envs
    -- env table
    CREATE SEQUENCE env_id_seq
    CREATE TABLE env (
        env_id    integer NOT NULL DEFAULT nextval('envs.env_id_seq'),
        outer_id  integer,
        data      hstore
    );

ALTER TABLE envs.env ADD CONSTRAINT pk_env_id
    PRIMARY KEY (env_id);
-- drop sequence when table dropped
ALTER SEQUENCE envs.env_id_seq OWNED BY envs.env.env_id;
ALTER TABLE envs.env ADD CONSTRAINT fk_env_outer_id
    FOREIGN KEY (outer_id) REFERENCES envs.env(env_id);

-- -----------------------

-- envs.new
CREATE FUNCTION envs.new(outer_env integer) RETURNS integer AS $$
DECLARE
    e  integer;
BEGIN
    INSERT INTO envs.env (outer_id) VALUES (outer_env)
        RETURNING env_id INTO e;
    --RAISE NOTICE 'env_new: e: %, outer_env: %', e, outer_env;
    RETURN e;
END; $$ LANGUAGE plpgsql;

-- envs.new with bindings
CREATE FUNCTION envs.new(outer_env integer,
                         binds integer,
                         exprs integer[])
    RETURNS integer AS $$
DECLARE
    bseq  integer[];
    env   integer;
    i     integer;
    bind  integer;
    bsym  varchar;
    expr  integer;
BEGIN
    env := envs.new(outer_env);
    bseq := types._valueToArray(binds);
    FOR i IN 1 .. COALESCE(array_length(bseq, 1), 0) LOOP
        bind := bseq[i];
        bsym := types._valueToString(bind);
        expr := exprs[i];
        --RAISE NOTICE 'i: %, bind: %, expr: %', i, bind, expr;
        IF bsym = '&' THEN
            bind := bseq[i+1];
            PERFORM envs.set(env, bind,
                             types._list(exprs[i:array_length(exprs, 1)]));
            RETURN env;
        END IF;
        PERFORM envs.vset(env, bsym, expr);
    END LOOP;
    RETURN env;
END; $$ LANGUAGE plpgsql;


-- envs.vset
-- like envs.set but takes a varchar key instead of value_id
CREATE FUNCTION envs.vset(env integer, name varchar, val integer)
    RETURNS integer AS $$
DECLARE
    e  integer = env;
    d  hstore;
BEGIN
    SELECT data INTO d FROM envs.env WHERE env_id=e;
    IF d IS NULL THEN
        d := hstore(name, CAST(val AS varchar));
    ELSE
        d := d || hstore(name, CAST(val AS varchar));
    END IF;
    UPDATE envs.env SET data = d WHERE env_id=e;
    RETURN val;
END; $$ LANGUAGE plpgsql;


-- envs.set
CREATE FUNCTION envs.set(env integer, key integer, val integer)
    RETURNS integer AS $$
DECLARE
    symkey  varchar;
BEGIN
    symkey := types._valueToString(key);
    RETURN envs.vset(env, symkey, val);
END; $$ LANGUAGE plpgsql;

-- envs.find
CREATE FUNCTION envs.find(env integer, symkey varchar) RETURNS integer AS $$
DECLARE
    outer_id  integer;
    d         hstore;
    val       integer;
BEGIN
    SELECT e.data, e.outer_id INTO d, outer_id FROM envs.env e
        WHERE e.env_id = env;
    IF d ? symkey THEN
        RETURN env;
    ELSIF outer_id IS NOT NULL THEN
        RETURN envs.find(outer_id, symkey);
    ELSE
        RETURN NULL;
    END IF;
END; $$ LANGUAGE plpgsql;


-- envs.vget
CREATE FUNCTION envs.vget(env integer, symkey varchar) RETURNS integer AS $$
DECLARE
    result  integer;
    e       integer;
BEGIN
    e := envs.find(env, symkey);
    --RAISE NOTICE 'envs.find env: %, symkey: % -> e: %', env, symkey, e;
    IF e IS NULL THEN
        RAISE EXCEPTION '''%'' not found', symkey;
    ELSE
        SELECT data -> symkey INTO result FROM envs.env WHERE env_id = e;
    END IF;
    RETURN result;
END; $$ LANGUAGE plpgsql;

-- envs.get
CREATE FUNCTION envs.get(env integer, key integer) RETURNS integer AS $$
BEGIN
    RETURN envs.vget(env, types._valueToString(key));
END; $$ LANGUAGE plpgsql;
