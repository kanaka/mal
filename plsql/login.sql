-- PROMPT 'Start login.sql';
whenever sqlerror exit SQL.SQLCODE;
whenever oserror exit 1;

SET ECHO OFF;
SET LINESIZE 32767;
-- SET TRIMOUT ON;
-- SET WRAP OFF;
SET PAGESIZE 0;

SET SERVEROUTPUT ON SIZE 30000;

-- PROMPT 'Finish login.sql';

