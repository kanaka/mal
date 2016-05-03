-- PROMPT 'Start login.sql';
whenever sqlerror exit SQL.SQLCODE;
whenever oserror exit 1;

SET ECHO OFF;
SET LINESIZE 32767;
-- SET TRIMOUT ON;
-- SET WRAP OFF;
SET PAGESIZE 0;

-- Do not format whitespace in terminaml output
SET TAB OFF;

-- Allow literal & in strings
SET DEFINE OFF;

-- Print DBMS_OUTPUT.PUT_LINE debugcommands
SET SERVEROUTPUT ON SIZE 30000;

-- Do not truncate or wrap CLOB output
SET LONG 32767;
SET LONGCHUNKSIZE 32767;

-- PROMPT 'Finish login.sql';

