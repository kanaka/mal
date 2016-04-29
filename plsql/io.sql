BEGIN
  EXECUTE IMMEDIATE 'DROP TABLE stream';
EXCEPTION
  WHEN OTHERS THEN IF SQLCODE != -942 THEN RAISE; END IF;
END;
/

CREATE TABLE stream (
    stream_id  integer,
    open       number(1,0),     -- stream open (1) or closed (0)
    data       varchar2(4000),  -- queued stream data
    rl_prompt  varchar2(4000)   -- prompt for readline input
);

-- stdin
INSERT INTO stream (stream_id, open, data, rl_prompt)
    VALUES (0, 0, '', '');
-- stdout
INSERT INTO stream (stream_id, open, data, rl_prompt)
    VALUES (1, 0, '', '');

-- ---------------------------------------------------------

BEGIN
  EXECUTE IMMEDIATE 'DROP TABLE file_io';
EXCEPTION
  WHEN OTHERS THEN IF SQLCODE != -942 THEN RAISE; END IF;
END;
/

CREATE TABLE file_io (
    path       varchar2(1024),  -- file to read/write
    data       varchar2(4000),  -- file data
    error      varchar2(1024),  -- any errors during read
    in_or_out  varchar2(4)      -- input ('in') or output ('out')
);

-- ---------------------------------------------------------

CREATE OR REPLACE PROCEDURE stream_open(sid integer) AS
BEGIN
    -- DBMS_OUTPUT.PUT_LINE('stream_open(' || sid || ') start');
    UPDATE stream SET data = '', rl_prompt = '', open = 1
        WHERE stream_id = sid;
    COMMIT;
    -- DBMS_OUTPUT.PUT_LINE('stream_open(' || sid || ') done');
END;
/

CREATE OR REPLACE PROCEDURE stream_close(sid integer) AS
BEGIN
    -- DBMS_OUTPUT.PUT_LINE('stream_close(' || sid || ') start');
    UPDATE stream SET rl_prompt = '', open = 0
        WHERE stream_id = sid;
    COMMIT;
    -- DBMS_OUTPUT.PUT_LINE('stream_close(' || sid || ') done');
END;
/

-- stream_read:
-- read from stream stream_id in stream table. Waits until there is
-- either data to return or the stream closes (NULL data). Returns
-- NULL when stream is closed.
CREATE OR REPLACE FUNCTION stream_read(sid integer DEFAULT 0)
RETURN varchar IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    input   varchar(4000);
    isopen  integer;
    sleep   real;
BEGIN
    sleep := 0.05;
    -- poll / wait for input
    WHILE true
    LOOP
        -- atomic get and set to empty
        -- LOCK TABLE stream IN EXCLUSIVE MODE;
        SELECT data, open INTO input, isopen FROM stream
            WHERE stream_id = sid;
        IF isopen = 1 AND input IS NOT NULL THEN
            UPDATE stream SET data = '' WHERE stream_id = sid;
            COMMIT;
            RETURN trim(TRAILING chr(10) FROM input);
        END IF;
        -- '' -> no input, NULL -> stream closed
        --RAISE NOTICE 'read input: [%] %', input, stream_id;
        IF isopen = 0 THEN
            raise_application_error(-20001,
                'stream_read: stream ''' || sid || ''' is closed', TRUE);
        END IF;
        SYS.DBMS_LOCK.SLEEP(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END;
/

-- stream_readline:
-- set prompt and wait for readline style input on the stream
CREATE OR REPLACE FUNCTION stream_readline(prompt varchar, sid integer DEFAULT 0)
RETURN varchar IS
    PRAGMA AUTONOMOUS_TRANSACTION;
BEGIN
    -- set prompt / request readline style input
    -- LOCK TABLE stream IN EXCLUSIVE MODE;
    UPDATE stream SET rl_prompt = prompt WHERE stream_id = sid;
    COMMIT;

    RETURN stream_read(sid);
END;
/

CREATE OR REPLACE PROCEDURE stream_write(input varchar, sid integer DEFAULT 1) AS
    PRAGMA AUTONOMOUS_TRANSACTION;
BEGIN
    -- LOCK TABLE stream IN EXCLUSIVE MODE;
    UPDATE stream SET data = data || input WHERE stream_id = sid;
    COMMIT;
END;
/

CREATE OR REPLACE PROCEDURE stream_writeline(data varchar, sid integer DEFAULT 1) AS
    PRAGMA AUTONOMOUS_TRANSACTION;
BEGIN
    stream_write(data || chr(10), sid);
END;
/

-- ---------------------------------------------------------

-- wait_rl_prompt:
-- wait for rl_prompt to be set on the given stream and return the
-- rl_prompt value. Errors if stream is already closed.
CREATE OR REPLACE FUNCTION stream_wait_rl_prompt(sid integer DEFAULT 0)
RETURN varchar IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    isopen   integer;
    prompt   varchar(4000);
    sleep    real;
    datas    integer;
BEGIN
    sleep := 0.05;
    WHILE true
    LOOP
        LOCK TABLE stream IN EXCLUSIVE MODE;
        SELECT open, rl_prompt INTO isopen, prompt
            FROM stream WHERE stream_id = sid;
        SELECT count(data) INTO datas FROM stream WHERE data IS NOT NULL;

        IF isopen = 0 THEN
            raise_application_error(-20001,
                'stream_wait_rl_prompt: stream ''' || sid || ''' is closed', TRUE);
        END IF;

        -- wait until all channels have flushed
        IF datas = 0 AND prompt IS NOT NULL THEN
            UPDATE stream SET rl_prompt = '' WHERE stream_id = sid;
            COMMIT;
            -- Prompt is returned single-quoted because sqlplus trims
            -- trailing whitespace in select output.
            RETURN '''' || prompt || '''';
        END IF;
        COMMIT;

        DBMS_LOCK.SLEEP(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END;
/

CREATE OR REPLACE PROCEDURE stream_wait_flushed(sid integer DEFAULT 1) AS
    PRAGMA AUTONOMOUS_TRANSACTION;
    pending  integer;
    sleep    real;
BEGIN
    sleep := 0.05;
    WHILE true
    LOOP
        SELECT count(data) INTO pending FROM stream
            WHERE stream_id = sid AND data IS NOT NULL AND data <> '';
        IF pending = 0 THEN RETURN; END IF;
        DBMS_LOCK.SLEEP(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END;
/

-- ---------------------------------------------------------

CREATE OR REPLACE FUNCTION file_open_and_read(path varchar)
RETURN varchar IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    sleep    real;
    content    varchar2(4000);
    error_msg  varchar2(1024);
BEGIN
    sleep := 0.05;
    -- TODO: use unique ID instead of path
    INSERT INTO file_io (path, data, error, in_or_out)
        VALUES (path, NULL, NULL, 'in');
    WHILE true
    LOOP
        LOCK TABLE file_io IN EXCLUSIVE MODE;
        SELECT data, error INTO content, error_msg
            FROM file_io WHERE path = path AND ROWNUM = 1;

        IF error_msg IS NOT NULL THEN
            raise_application_error(-20010,
                'open_and_read error: ''' || error_msg || '''', TRUE);
        END IF;

        IF content IS NOT NULL THEN
            DELETE FROM file_io WHERE path = path;
            COMMIT;
            RETURN content;
        END IF;
        COMMIT;

        -- keep waiting
        DBMS_LOCK.SLEEP(sleep);
        IF sleep < 0.5 THEN
            sleep := sleep * 1.1; -- backoff
        END IF;
    END LOOP;
END;
/

CREATE OR REPLACE PROCEDURE file_read_response(path varchar, data varchar) AS
    PRAGMA AUTONOMOUS_TRANSACTION;
BEGIN
    UPDATE file_io SET data = data WHERE path = path;
END;
/

