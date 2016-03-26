CREATE EXTENSION dblink;

CREATE TABLE stream (
    stream_id  integer,
    data       varchar,
    rl_prompt  varchar  -- prompt for readline input
);

INSERT INTO stream (stream_id, data, rl_prompt) VALUES (0, '', ''); -- stdin
INSERT INTO stream (stream_id, data, rl_prompt) VALUES (1, '', ''); -- stdout

CREATE FUNCTION read(stream_id integer DEFAULT 0)
RETURNS varchar AS $$
DECLARE
    query  varchar;
    input  varchar;
    sleep  real = 0.05;
BEGIN
    -- poll / wait for input
    query := format('LOCK stream; UPDATE stream x SET data = '''' FROM (SELECT data FROM stream WHERE stream_id = %L AND data <> '''') y WHERE x.stream_id = %L AND x.data <> '''' RETURNING y.data AS cur_data;', stream_id, stream_id);
    WHILE true
    LOOP
        -- atomic get and set to empty
        SELECT cur_data INTO input FROM dblink('dbname=mal', query)
            AS t1(cur_data varchar);
        --RAISE NOTICE 'read input: [%] %', input, stream_id;
        IF input <> '' THEN
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
    --query := format('SELECT rl_prompt FROM stream WHERE stream_id = %L AND rl_prompt <> ''''', stream_id);
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

