-- ---------------------------------------------------------
-- init.sql

-- Drop pre-existing schemas
DROP SCHEMA IF EXISTS io, types, reader, printer, envs, core, mal CASCADE;

-- Drop and recreate extensions
DROP EXTENSION IF EXISTS hstore;
CREATE EXTENSION hstore;

DROP EXTENSION IF EXISTS dblink;
CREATE EXTENSION dblink;

