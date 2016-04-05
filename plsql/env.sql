-- ---------------------------------------------------------
-- env.sql

PROMPT "env.sql start";

CREATE OR REPLACE TYPE env_item FORCE AS OBJECT (
    key  varchar2(100),
    val  mal_type
) FINAL;
/

CREATE OR REPLACE TYPE env_data FORCE IS TABLE OF env_item;
/

CREATE OR REPLACE TYPE env_type FORCE AS OBJECT (
    idx        integer,
    outer_idx  integer,
    data       env_data
);
/

CREATE OR REPLACE TYPE env_mem_type FORCE IS TABLE OF env_type;
/

CREATE OR REPLACE PACKAGE env_pkg IS
    FUNCTION env_new(mem IN OUT NOCOPY env_mem_type,
                     outer_idx integer DEFAULT NULL) RETURN integer;
    FUNCTION env_set(mem IN OUT NOCOPY env_mem_type,
                     eidx integer,
                     key mal_type, val mal_type) RETURN mal_type;
    FUNCTION env_find(mem env_mem_type,
                      eidx integer,
                      key mal_type) RETURN integer;
    FUNCTION env_get(mem env_mem_type,
                     eidx integer,
                     key mal_type) RETURN mal_type;
END env_pkg;
/

CREATE OR REPLACE PACKAGE BODY env_pkg IS
    FUNCTION env_new(mem IN OUT NOCOPY env_mem_type,
                     outer_idx integer DEFAULT NULL) RETURN integer IS
        eidx  integer;
    BEGIN
        mem.EXTEND();
        eidx := mem.COUNT;
        mem(eidx) := env_type(eidx, outer_idx, env_data());
        RETURN eidx;
    END;

    FUNCTION env_set(mem IN OUT NOCOPY env_mem_type,
                     eidx integer,
                     key mal_type, val mal_type) RETURN mal_type IS
        k    varchar2(100);
        i    integer;
        cnt  integer;
        ed   env_data;
    BEGIN
        k := TREAT(key AS mal_str_type).val_str;
        SELECT count(*) INTO cnt FROM TABLE(mem(eidx).data) t
            WHERE key = k;
        IF cnt > 0 THEN
            -- TODO: a more efficient way to do this
            ed := mem(eidx).data;
            FOR i IN ed.FIRST..ed.LAST LOOP
                IF ed(i).key = k THEN
                    mem(eidx).data(i).val := val;
                    EXIT;
                END IF;
            END LOOP;
        ELSE
            mem(eidx).data.EXTEND();
            mem(eidx).data(mem(eidx).data.COUNT) := env_item(k, val);
        END IF;
        RETURN val;
    END;

    FUNCTION env_find(mem env_mem_type,
                      eidx integer,
                      key mal_type) RETURN integer IS
        e    env_type;
        k    varchar2(100);
        cnt  integer;
    BEGIN
        e := mem(eidx);
        k := TREAT(key AS mal_str_type).val_str;
        SELECT COUNT(*) INTO cnt FROM TABLE(e.data) t WHERE key = k;
        IF cnt > 0 THEN
            RETURN e.idx;
        ELSIF e.outer_idx IS NOT NULL THEN
            e := mem(e.outer_idx);
            RETURN env_find(mem, e.idx, key);
        ELSE
            RETURN NULL;
        END IF;
    END;

    FUNCTION env_get(mem env_mem_type,
                     eidx integer,
                     key mal_type) RETURN mal_type IS
        idx   integer;
        e     env_type;
        k     varchar2(100);
        v     mal_type;
    BEGIN
        idx := env_find(mem, eidx, key);
        k := TREAT(key AS mal_str_type).val_str;
        IF idx IS NOT NULL THEN
            e := mem(idx);
            SELECT t.val INTO v FROM TABLE(e.data) t
                WHERE key = k;
            RETURN v;
        ELSE
            raise_application_error(-20005,
                '''' || k || ''' not found', TRUE);
        END IF;
    END;
END env_pkg;
/
show errors;


PROMPT "env.sql finished";
