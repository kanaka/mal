-- dblink is necessary to be able to sub-transactions (autonomous
-- transactions) to the stream table. This is necessary to be able to
-- modify the stream table from the perspective of outside callers
-- because actual code can be long-lived and it's direct updates will
-- not be seen until the process completes.

CREATE SCHEMA io

    CREATE TABLE stream (
        stream_id  integer,
        open       boolean,
        data       varchar,
        rl_prompt  varchar  -- prompt for readline input
    );

-- stdin
INSERT INTO io.stream (stream_id, open, data, rl_prompt)
    VALUES (0, false, '', '');
-- stdout
INSERT INTO io.stream (stream_id, open, data, rl_prompt)
    VALUES (1, false, '', '');

-- ---------------------------------------------------------

CREATE FUNCTION io.open(sid integer) RETURNS void AS $$
DECLARE
    query  varchar;
BEGIN
    --RAISE NOTICE 'io.open start';
    query := format('UPDATE io.stream
        SET data = '''', rl_prompt = '''', open = true
        WHERE stream_id = %L', sid);
    PERFORM dblink('dbname=mal', query);
    --RAISE NOTICE 'io.open done';
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION io.close(sid integer) RETURNS void AS $$
DECLARE
    query  varchar;
BEGIN
    --RAISE NOTICE 'io.close start';
    query := format('UPDATE io.stream
        SET rl_prompt = '''', open = false
        WHERE stream_id = %L', sid);
    PERFORM dblink('dbname=mal', query);
    --RAISE NOTICE 'io.close done';
END; $$ LANGUAGE 'plpgsql' STRICT;


-- called from read via dblink
CREATE FUNCTION io.__read(sid integer) RETURNS varchar AS $$
DECLARE
    input   varchar;
    isopen  boolean;
BEGIN
    LOCK io.stream;
    SELECT data, open INTO input, isopen FROM io.stream
        WHERE stream_id = sid;
    IF input <> '' THEN
        UPDATE io.stream SET data = '' WHERE stream_id = sid;
        RETURN input;
    END IF;
    IF isopen = false THEN
        RETURN NULL;
    END IF;
    RETURN input;
END; $$ LANGUAGE 'plpgsql' STRICT;

-- read:
-- read from stream stream_id in stream table. Waits until there is
-- either data to return or the stream closes (NULL data). Returns
-- NULL when stream is closed.
CREATE FUNCTION io.read(sid integer DEFAULT 0) RETURNS varchar AS $$
DECLARE
    query  varchar;
    input  varchar;
    sleep  real = 0.05;
BEGIN
    -- poll / wait for input
    query := format('SELECT io.__read(%L);', sid);

    WHILE true
    LOOP
        -- atomic get and set to empty
        SELECT cur_data INTO input FROM dblink('dbname=mal', query)
            AS t1(cur_data varchar);
        IF input <> '' OR input IS NULL THEN
            RETURN input;
        END IF;
        PERFORM pg_sleep(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END; $$ LANGUAGE 'plpgsql' STRICT;

-- read_or_error:
-- similar to read, but throws exception when stream is closed
CREATE FUNCTION io.read_or_error(sid integer DEFAULT 0) RETURNS varchar AS $$
DECLARE
    input  varchar;
BEGIN
    input := io.read(sid);
    IF input IS NULL THEN
        raise EXCEPTION 'Stream ''%'' is closed', sid;
    ELSE
        RETURN input;
    END IF;
END; $$ LANGUAGE 'plpgsql' STRICT;


-- readline:
-- set prompt and wait for readline style input on the stream
CREATE FUNCTION io.readline(prompt varchar, sid integer DEFAULT 0)
    RETURNS varchar AS $$
DECLARE
    query  varchar;
BEGIN
    -- set prompt / request readline style input
    IF sid = 0 THEN
        PERFORM io.wait_flushed(1);
    ELSIF sid = 1 THEN
        PERFORM io.wait_flushed(0);
    END IF;
    query := format('LOCK io.stream; UPDATE io.stream SET rl_prompt = %L',
        prompt);
    PERFORM dblink('dbname=mal', query);

    RETURN io.read(sid);
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION io.write(data varchar, sid integer DEFAULT 1)
RETURNS void AS $$
DECLARE
    query   varchar;
BEGIN
    query := format('LOCK io.stream;
        UPDATE io.stream SET data = data || %L WHERE stream_id = %L',
        data, sid);
    --RAISE NOTICE 'write query: %', query;
    PERFORM dblink('dbname=mal', query);
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION io.writeline(data varchar, sid integer DEFAULT 1)
RETURNS void AS $$
BEGIN
    PERFORM io.write(data || E'\n', sid);
END; $$ LANGUAGE 'plpgsql' STRICT;

-- ---------------------------------------------------------

-- called from wait_rl_prompt via dblink
CREATE FUNCTION io.__wait_rl_prompt(sid integer) RETURNS varchar AS $$
DECLARE
    isopen  boolean;
    prompt  varchar;
    datas   integer;
BEGIN
    LOCK io.stream;
    SELECT open, rl_prompt INTO isopen, prompt FROM io.stream
        WHERE stream_id = sid;
    SELECT count(stream_id) INTO datas FROM io.stream WHERE data <> '';

    IF isopen = false THEN
        return NULL;
        --raise EXCEPTION 'Stream ''%'' is closed', sid;
    END IF;

    IF datas = 0 AND prompt <> '' THEN
        UPDATE io.stream SET rl_prompt = '' WHERE stream_id = sid;
        -- There is pending data on some stream
        RETURN prompt;
    END IF;
    RETURN ''; -- '' -> no input
END; $$ LANGUAGE 'plpgsql' STRICT;

-- wait_rl_prompt:
-- wait for rl_prompt to be set on the given stream and return the
-- rl_prompt value. Errors if stream is already closed.
CREATE FUNCTION io.wait_rl_prompt(sid integer DEFAULT 0) RETURNS varchar AS $$
DECLARE
    query    varchar;
    prompt   varchar;
    sleep    real = 0.05;
BEGIN
    query := format('SELECT io.__wait_rl_prompt(%L);', sid);
    WHILE true
    LOOP
        SELECT rl_prompt INTO prompt FROM dblink('dbname=mal', query)
            AS t1(rl_prompt varchar);
        IF prompt IS NULL THEN
            raise EXCEPTION 'Stream ''%'' is closed', sid;
        END IF;
        IF prompt <> '' THEN
            sleep := 0.05; -- reset sleep timer
            RETURN prompt;
        END IF;
        PERFORM pg_sleep(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION io.wait_flushed(sid integer DEFAULT 1) RETURNS void AS $$
DECLARE
    query    varchar;
    pending  integer;
    sleep    real = 0.05;
BEGIN
    query := format('SELECT count(stream_id) FROM io.stream
        WHERE stream_id = %L AND data <> ''''', sid);
    WHILE true
    LOOP
        SELECT p INTO pending FROM dblink('dbname=mal', query)
            AS t1(p integer);
        IF pending = 0 THEN RETURN; END IF;
        PERFORM pg_sleep(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END; $$ LANGUAGE 'plpgsql' STRICT;

