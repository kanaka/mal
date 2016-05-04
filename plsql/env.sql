-- ---------------------------------------------------------
-- env.sql

CREATE OR REPLACE TYPE env_item FORCE AS OBJECT (
    key  varchar2(256),
    val  integer
) FINAL;
/

CREATE OR REPLACE TYPE env_data FORCE IS TABLE OF env_item;
/

CREATE OR REPLACE TYPE env_T FORCE AS OBJECT (
    idx        integer,
    outer_idx  integer,
    data       env_data
);
/

CREATE OR REPLACE TYPE env_mem_T FORCE IS TABLE OF env_T;
/

CREATE OR REPLACE PACKAGE env_pkg IS
    TYPE env_entry IS TABLE OF integer INDEX BY varchar2(256);
    TYPE env_entry_table IS TABLE OF env_entry;

    FUNCTION env_new(M IN OUT NOCOPY types.mal_table,
                     eeT IN OUT NOCOPY env_entry_table,
                     outer_idx integer DEFAULT NULL)
                RETURN integer;
    FUNCTION env_new(M IN OUT NOCOPY types.mal_table,
                     eeT IN OUT NOCOPY env_entry_table,
                     outer_idx integer,
                     binds integer,
                     exprs mal_vals)
                RETURN integer;
    FUNCTION env_set(M IN OUT NOCOPY types.mal_table,
                     eeT IN OUT NOCOPY env_entry_table,
                     eidx integer,
                     key integer,
                     val integer) RETURN integer;
    FUNCTION env_find(M IN OUT NOCOPY types.mal_table,
                      eeT env_entry_table,
                      eidx integer,
                      key integer) RETURN integer;
    FUNCTION env_get(M IN OUT NOCOPY types.mal_table,
                     eeT env_entry_table,
                     eidx integer,
                     key integer) RETURN integer;
END env_pkg;
/
show errors;


CREATE OR REPLACE PACKAGE BODY env_pkg IS

FUNCTION env_new(M IN OUT NOCOPY types.mal_table,
                 eeT IN OUT NOCOPY env_entry_table,
                 outer_idx integer DEFAULT NULL)
            RETURN integer IS
    eidx  integer;
BEGIN
    eeT.EXTEND();
    eidx := eeT.COUNT();
    eeT(eidx)('**OUTER**') := outer_idx;
    RETURN eidx;
END;

FUNCTION env_new(M IN OUT NOCOPY types.mal_table,
                 eeT IN OUT NOCOPY env_entry_table,
                 outer_idx integer,
                 binds integer,
                 exprs mal_vals)
            RETURN integer IS
    eidx  integer;
    i     integer;
    bs    mal_vals;
BEGIN
    eeT.EXTEND();
    eidx := eeT.COUNT();
    eeT(eidx)('**OUTER**') := outer_idx;
    IF binds IS NOT NULL THEN
        bs := TREAT(M(binds) AS mal_seq_T).val_seq;
        FOR i IN 1..bs.COUNT LOOP
            IF TREAT(M(bs(i)) AS mal_str_T).val_str = '&' THEN
                eeT(eidx)(TREAT(M(bs(i+1)) AS mal_str_T).val_str) :=
                    types.slice(M, exprs, i-1);
                EXIT;
            ELSE
                eeT(eidx)(TREAT(M(bs(i)) AS mal_str_T).val_str) :=
                    exprs(i);
            END IF;
        END LOOP;
    END IF;
    RETURN eidx;
END;

FUNCTION env_set(M IN OUT NOCOPY types.mal_table,
                 eeT IN OUT NOCOPY env_entry_table,
                 eidx integer,
                 key integer,
                 val integer) RETURN integer IS
    k    varchar2(256);
    i    integer;
    cnt  integer;
BEGIN
    k := TREAT(M(key) AS mal_str_T).val_str;
    eeT(eidx)(k) := val;
    RETURN val;
END;

FUNCTION env_find(M IN OUT NOCOPY types.mal_table,
                  eeT env_entry_table,
                  eidx integer,
                  key integer) RETURN integer IS
    k    varchar2(256);
    cnt  integer;
BEGIN
    k := TREAT(M(key) AS mal_str_T).val_str;
    IF eeT(eidx).EXISTS(k) THEN
        RETURN eidx;
    ELSIF eeT(eidx)('**OUTER**') IS NOT NULL THEN
        RETURN env_find(M, eeT, eeT(eidx)('**OUTER**'), key);
    ELSE
        RETURN NULL;
    END IF;
END;

FUNCTION env_get(M IN OUT NOCOPY types.mal_table,
                 eeT env_entry_table,
                 eidx integer,
                 key integer) RETURN integer IS
    found  integer;
    k      varchar2(256);
BEGIN
    found := env_find(M, eeT, eidx, key);
    k := TREAT(M(key) AS mal_str_T).val_str;
    IF found IS NOT NULL THEN
        RETURN eeT(found)(k);
    ELSE
        raise_application_error(-20005,
            '''' || k || ''' not found', TRUE);
    END IF;
END;

END env_pkg;
/
show errors;
