-- ---------------------------------------------------------
-- env.sql

PROMPT "env.sql start";

CREATE OR REPLACE TYPE env_item FORCE AS OBJECT (
    key  varchar2(100),
    val  integer
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
    FUNCTION env_new(M IN OUT NOCOPY mem_type,
                     eM IN OUT NOCOPY env_mem_type,
                     outer_idx integer DEFAULT NULL)
                RETURN integer;
    FUNCTION env_new(M IN OUT NOCOPY mem_type,
                     eM IN OUT NOCOPY env_mem_type,
                     outer_idx integer,
                     binds integer,
                     exprs mal_seq_items_type)
                RETURN integer;
    FUNCTION env_set(M IN OUT NOCOPY mem_type,
                     eM IN OUT NOCOPY env_mem_type,
                     eidx integer,
                     key integer,
                     val integer) RETURN integer;
    FUNCTION env_find(M IN OUT NOCOPY mem_type,
                      eM env_mem_type,
                      eidx integer,
                      key integer) RETURN integer;
    FUNCTION env_get(M IN OUT NOCOPY mem_type,
                     eM env_mem_type,
                     eidx integer,
                     key integer) RETURN integer;
END env_pkg;
/
show errors;

CREATE OR REPLACE PACKAGE BODY env_pkg IS
    FUNCTION env_new(M IN OUT NOCOPY mem_type,
                     eM IN OUT NOCOPY env_mem_type,
                     outer_idx integer DEFAULT NULL)
                RETURN integer IS
        eidx  integer;
    BEGIN
        eM.EXTEND();
        eidx := eM.COUNT();
        eM(eidx) := env_type(eidx, outer_idx, env_data());
        RETURN eidx;
    END;

    FUNCTION env_new(M IN OUT NOCOPY mem_type,
                     eM IN OUT NOCOPY env_mem_type,
                     outer_idx integer,
                     binds integer,
                     exprs mal_seq_items_type)
                RETURN integer IS
        eidx  integer;
        ed    env_data;
        i     integer;
        bs    mal_seq_items_type;
    BEGIN
        eM.EXTEND();
        eidx := eM.COUNT();
        ed := env_data();
        IF binds IS NOT NULL THEN
            bs := TREAT(M(binds) AS mal_seq_type).val_seq;
            FOR i IN 1..bs.COUNT LOOP
                ed.EXTEND();
                IF TREAT(M(bs(i)) AS mal_str_type).val_str = '&' THEN
                    ed(ed.COUNT) := env_item(
                        TREAT(M(bs(i+1)) AS mal_str_type).val_str,
                        types.slice(M, exprs, i-1));
                    EXIT;
                ELSE
                    ed(ed.COUNT) := env_item(
                        TREAT(M(bs(i)) AS mal_str_type).val_str,
                        exprs(i));
                END IF;
            END LOOP;
        END IF;
        eM(eidx) := env_type(eidx, outer_idx, ed);
        RETURN eidx;
    END;

    FUNCTION env_set(M IN OUT NOCOPY mem_type,
                     eM IN OUT NOCOPY env_mem_type,
                     eidx integer,
                     key integer,
                     val integer) RETURN integer IS
        k    varchar2(100);
        i    integer;
        cnt  integer;
        ed   env_data;
    BEGIN
        k := TREAT(M(key) AS mal_str_type).val_str;
        SELECT count(*) INTO cnt FROM TABLE(eM(eidx).data) t
            WHERE key = k;
        IF cnt > 0 THEN
            -- TODO: a more efficient way to do this
            ed := eM(eidx).data;
            FOR i IN ed.FIRST..ed.LAST LOOP
                IF ed(i).key = k THEN
                    eM(eidx).data(i).val := val;
                    EXIT;
                END IF;
            END LOOP;
        ELSE
            eM(eidx).data.EXTEND();
            eM(eidx).data(eM(eidx).data.COUNT) := env_item(k, val);
        END IF;
        RETURN val;
    END;

    FUNCTION env_find(M IN OUT NOCOPY mem_type,
                      eM env_mem_type,
                      eidx integer,
                      key integer) RETURN integer IS
        e    env_type;
        k    varchar2(100);
        cnt  integer;
    BEGIN
        e := eM(eidx);
        k := TREAT(M(key) AS mal_str_type).val_str;
        SELECT COUNT(*) INTO cnt FROM TABLE(e.data) t WHERE key = k;
        IF cnt > 0 THEN
            RETURN e.idx;
        ELSIF e.outer_idx IS NOT NULL THEN
            e := eM(e.outer_idx);
            RETURN env_find(M, eM, e.idx, key);
        ELSE
            RETURN NULL;
        END IF;
    END;

    FUNCTION env_get(M IN OUT NOCOPY mem_type,
                     eM env_mem_type,
                     eidx integer,
                     key integer) RETURN integer IS
        idx   integer;
        e     env_type;
        k     varchar2(100);
        v     integer;
    BEGIN
        idx := env_find(M, eM, eidx, key);
        k := TREAT(M(key) AS mal_str_type).val_str;
        IF idx IS NOT NULL THEN
            e := eM(idx);
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
