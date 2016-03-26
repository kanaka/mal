CREATE TABLE stream (
    stream_id  integer,
    data       varchar,
    rl_prompt  varchar  -- prompt for readline input
);

INSERT INTO stream (stream_id, data, rl_prompt) VALUES (0, '', ''); -- stdin
INSERT INTO stream (stream_id, data, rl_prompt) VALUES (1, '', ''); -- stdout

-- dblink is necessary to be able to sub-transactions (autonomous
-- transactions) to the stream table. This is necessary to be able to
-- modify the stream table from the perspective of outside callers
-- because actual code can be long-lived and it's direct updates will
-- not be seen until the process completes.
CREATE EXTENSION dblink;

-- ---------------------------------------------------------

CREATE FUNCTION stream_open(sid integer) RETURNS void AS $$
DECLARE
    query  varchar;
BEGIN
    --RAISE NOTICE 'stream_open start';
    query := format('UPDATE stream SET data = '''', rl_prompt = '''' WHERE stream_id = %L', sid);
    PERFORM dblink('dbname=mal', query);
    --RAISE NOTICE 'stream_open done';
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION stream_close(sid integer) RETURNS void AS $$
DECLARE
    query  varchar;
BEGIN
    --RAISE NOTICE 'stream_close start';
    query := format('UPDATE stream SET data = NULL WHERE stream_id = %L', sid);
    PERFORM dblink('dbname=mal', query);
    --RAISE NOTICE 'stream_close done';
END; $$ LANGUAGE 'plpgsql' STRICT;


-- called via dblink
CREATE FUNCTION __read(sid integer) RETURNS varchar AS $$
DECLARE
    input  varchar;
BEGIN
    LOCK stream;
    SELECT data INTO input FROM stream WHERE stream_id = sid;
    IF input IS NOT NULL AND input <> '' THEN
        UPDATE stream SET data = '' WHERE stream_id = sid;
    END IF;
    RETURN input; -- '' -> no input, NULL -> stream closed
END; $$ LANGUAGE 'plpgsql' STRICT;

-- read:
-- read from stream stream_id in stream table. Waits until there is
-- either data to return or the stream closes (NULL data). Returns
-- NULL when stream is closed.
CREATE FUNCTION read(stream_id integer DEFAULT 0)
RETURNS varchar AS $$
DECLARE
    query  varchar;
    input  varchar;
    sleep  real = 0.05;
BEGIN
    -- poll / wait for input
    query := format('SELECT __read(%L);', stream_id);
    WHILE true
    LOOP
        -- atomic get and set to empty
        SELECT cur_data INTO input FROM dblink('dbname=mal', query)
            AS t1(cur_data varchar);
        --RAISE NOTICE 'read input: [%] %', input, stream_id;
        IF input <> '' OR input IS NULL THEN
            sleep := 0.05; -- reset sleep timer
            --RAISE NOTICE 'read input: [%] %', input, stream_id;
            --RETURN rtrim(input, E'\n');
            RETURN input;
        END IF;
        --RAISE NOTICE 'sleeping 2 seconds';
        --PERFORM pg_sleep(2);
        PERFORM pg_sleep(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END; $$ LANGUAGE 'plpgsql' STRICT;

-- read_or_error:
-- similar to read, but throws exception when stream is closed
CREATE FUNCTION read_or_error(stream_id integer DEFAULT 0)
RETURNS varchar AS $$
DECLARE
    input  varchar;
BEGIN
    input := read(stream_id);
    IF input IS NULL THEN
        raise EXCEPTION 'Stream ''%'' is closed', stream_id;
    ELSE
        RETURN input;
    END IF;
END; $$ LANGUAGE 'plpgsql' STRICT;


-- readline:
-- set prompt and wait for readline style input on the stream
CREATE FUNCTION readline(prompt varchar, stream_id integer DEFAULT 0)
RETURNS varchar AS $$
DECLARE
    query  varchar;
BEGIN
    -- set prompt / request readline style input
    query := format('LOCK stream; UPDATE stream SET rl_prompt = %L', prompt);
    PERFORM dblink('dbname=mal', query);

    RETURN read(stream_id);
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION write(data varchar, stream_id integer DEFAULT 1)
RETURNS void AS $$
DECLARE
    query   varchar;
BEGIN
    query := format('LOCK stream; UPDATE stream SET data = data || %L WHERE stream_id = %L',
        data, stream_id);
    --RAISE NOTICE 'write query: %', query;
    PERFORM dblink('dbname=mal', query);
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION writeline(data varchar, stream_id integer DEFAULT 1)
RETURNS void AS $$
BEGIN
    PERFORM write(data || E'\n', stream_id);
END; $$ LANGUAGE 'plpgsql' STRICT;

-- wait_rl_prompt:
-- wait for rl_prompt to be set on the given stream and return the
-- rl_prompt value
CREATE FUNCTION wait_rl_prompt(stream_id integer DEFAULT 0)
RETURNS varchar AS $$
DECLARE
    dquery   varchar;
    pending  integer;
    query    varchar;
    prompt   varchar;
    sleep    real = 0.05;
BEGIN
    -- make sure no data is pending on any stream
    dquery := format('SELECT count(data) FROM stream WHERE data <> ''''');
    -- wait for readline style input to be requested
    query := format('LOCK stream; UPDATE stream x SET rl_prompt = '''' FROM (SELECT rl_prompt FROM stream WHERE stream_id = %L AND rl_prompt <> '''') y WHERE x.stream_id = %L AND x.rl_prompt <> '''' RETURNING y.rl_prompt AS rl_prompt', stream_id, stream_id);
    WHILE true
    LOOP
        SELECT p INTO pending FROM dblink('dbname=mal', dquery)
            AS t1(p integer);
        IF pending = 0 THEN
            -- atomic get and set to empty
            SELECT rl_prompt INTO prompt FROM dblink('dbname=mal', query)
                AS t1(rl_prompt varchar);
            IF prompt <> '' THEN
                sleep := 0.05; -- reset sleep timer
                RETURN prompt;
            END IF;
        END IF;
        PERFORM pg_sleep(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END; $$ LANGUAGE 'plpgsql' STRICT;

CREATE FUNCTION wait_flushed(stream_id integer DEFAULT 1)
RETURNS void AS $$
DECLARE
    query    varchar;
    pending  integer;
    sleep    real = 0.05;
BEGIN
    query := format('SELECT count(data) FROM stream WHERE stream_id = %L AND data IS NOT NULL AND data <> ''''', stream_id);
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

