-- ---------------------------------------------------------
-- printer.sql

CREATE OR REPLACE FUNCTION
    pr_str(ast integer) RETURNS varchar AS $$
DECLARE
    re    varchar = E'[[:space:] ,]*(~@|[\\[\\]{}()\'`~@]|"(?:[\\\\].|[^\\\\"])*"|;.*|[^\\s \\[\\]{}()\'"`~@,;]*)';
    type  integer;
    cid   integer;
    vid   integer;
    res   varchar;
BEGIN
    -- RAISE NOTICE 'pr_str ast: %', ast;
    SELECT type_id FROM value WHERE value_id = ast INTO type;
    -- RAISE NOTICE 'pr_str type: %', type;
    CASE
    WHEN type = 0 THEN RETURN 'nil';
    WHEN type = 1 THEN RETURN 'false';
    WHEN type = 2 THEN RETURN 'true';
    WHEN type = 3 THEN
        RETURN CAST((SELECT val_int
                     FROM value WHERE value_id = ast) as varchar);
    WHEN type = 5 THEN
        RETURN '"' ||
               (SELECT value FROM string
                WHERE string_id = (SELECT val_string_id
                                   FROM value WHERE value_id = ast)) ||
               '"';
    WHEN type = 7 THEN
        RETURN (SELECT value FROM string
                WHERE string_id = (SELECT val_string_id
                                   FROM value WHERE value_id = ast));
    WHEN type = 8 THEN
        BEGIN
            cid := (SELECT collection_id FROM value WHERE value_id = ast);
            RETURN '(' ||
                   array_to_string(array(
                        SELECT pr_str(c.value_id) FROM collection c
                        WHERE c.collection_id = cid), ' ') ||
                   ')';
        END;
    WHEN type = 9 THEN
        BEGIN
            cid := (SELECT collection_id FROM value WHERE value_id = ast);
            RETURN '[' ||
                   array_to_string(array(
                        SELECT pr_str(c.value_id) FROM collection c
                        WHERE c.collection_id = cid), ' ') ||
                   ']';
        END;
    WHEN type = 11 THEN
        BEGIN
            RETURN '#<function ' ||
                   (SELECT function_name FROM value WHERE value_id = ast) ||
                   '>';
        END;
    ELSE
        RETURN 'unknown';
    END CASE;
END; $$ LANGUAGE plpgsql;
