-- Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>
-- 
-- All rights reserved.
-- 
-- For license and copyright information, see the file COPYRIGHT

--------------------------------------------------------------------------
--------------------------------------------------------------------------

\! clear

DROP DATABASE   IF EXISTS pcltCatalogs;
DROP TABLESPACE IF EXISTS tabsp_pcltCatalogs;

\! rm -rf /var/lib/postgres/data/pg_tblspc/pcltCatalogs

DROP ROLE IF EXISTS user_pcltCatalogs_data_reader;
DROP ROLE IF EXISTS user_pcltCatalogs_data_admin;
DROP ROLE IF EXISTS user_pcltCatalogs_owner;

---------------------------------
---------------------------------
---------------------------------

CREATE ROLE user_pcltCatalogs_owner 
   WITH SUPERUSER 
        NOCREATEDB
        LOGIN
        CREATEROLE
        UNENCRYPTED PASSWORD 'owner_password';

CREATE ROLE user_pcltCatalogs_data_admin 
   WITH LOGIN
        UNENCRYPTED PASSWORD 'data_admin_password';

CREATE ROLE user_pcltCatalogs_data_reader 
   WITH LOGIN
        UNENCRYPTED PASSWORD 'data_reader_password';

-------------------------

\! mkdir /var/lib/postgres/data/pg_tblspc/pcltCatalogs
\! mkdir /var/lib/postgres/data/pg_tblspc/pcltCatalogs/tabsp_pcltCatalogs.data

CREATE TABLESPACE tabsp_pcltCatalogs 
        OWNER user_pcltCatalogs_owner 
        LOCATION '/var/lib/postgres/data/pg_tblspc/pcltCatalogs/tabsp_pcltCatalogs.data';

-------------------------

CREATE DATABASE pcltCatalogs
  WITH OWNER = user_pcltCatalogs_owner 
                ENCODING = 'UTF8'
                TABLESPACE = tabsp_pcltCatalogs;

GRANT CONNECT ON DATABASE pcltCatalogs TO user_pcltCatalogs_data_admin, user_pcltCatalogs_data_reader;

-------------------------
-- (1) case sensetive (2) postgres lowercases real names
\c pcltcatalogs user_pcltcatalogs_owner

CREATE LANGUAGE plpgsql;

CREATE SCHEMA sch_pcltCatalogs AUTHORIZATION user_pcltCatalogs_owner;

GRANT USAGE ON SCHEMA sch_pcltCatalogs TO user_pcltCatalogs_data_admin, user_pcltCatalogs_data_reader;

