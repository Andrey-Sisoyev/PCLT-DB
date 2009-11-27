-- Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>
-- 
-- All rights reserved.
-- 
-- For license and copyright information, see the file COPYRIGHT

--------------------------------------------------------------------------
--------------------------------------------------------------------------

\c pcltcatalogs user_pcltcatalogs_owner

SET search_path TO sch_pcltcatalogs, public; -- sets only for current session

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

DROP FUNCTION IF EXISTS by_catalog_used_config (integer);
DROP FUNCTION IF EXISTS in_catalog_localized_tpls_with_their_sdls(integer); 
DROP TYPE     IF EXISTS type__catalog_ltpl_row;

\echo
\echo **** DROPPING TRIGGERS. 
\echo **** psql may throw a dosen of complains here, like "ERROR:  relation "tpls_catalogs" does not exist", - just ignore that!..
\echo
DROP TRIGGER IF EXISTS tri_tpls_catalogs_onupdate ON tpls_catalogs;
DROP TRIGGER IF EXISTS tri_tplscat_inner_configs_onupdate ON tplscat_inner_configs;
DROP TRIGGER IF EXISTS tri_tpls_collections_onupdate ON tpls_collections;
DROP TRIGGER IF EXISTS tri_tpls_onupdate ON tpls;
DROP TRIGGER IF EXISTS tri_tpls_oninsert ON tpls;
DROP TRIGGER IF EXISTS tri_tpls_localized_onupdate ON tpls_localized;
DROP TRIGGER IF EXISTS tri_tpls_inclusions_in_collections_onupdate ON tpls_inclusions_in_collections;
DROP TRIGGER IF EXISTS tri_tpls_representations_sdls_onupdate ON tpls_representations_sdls;
DROP TRIGGER IF EXISTS tri_collections_inclusions_in_catalogs_onupdate ON collections_inclusions_in_catalogs;
DROP TRIGGER IF EXISTS tri_collections_dependencies_onupdate ON collections_dependencies;

DROP TABLE IF EXISTS collections_dependencies;
DROP TABLE IF EXISTS collections_inclusions_in_catalogs;
DROP TABLE IF EXISTS tpls_representations_sdls;
DROP TABLE IF EXISTS tpls_inclusions_in_collections;
DROP TABLE IF EXISTS tpls_localized;
DROP TABLE IF EXISTS tpls;
DROP TABLE IF EXISTS tpls_catalogs;
DROP SEQUENCE IF EXISTS tpls_catalogs_ids_seq;
DROP TABLE IF EXISTS tpls_detalization_policies;
DROP SEQUENCE IF EXISTS tpls_detalization_policies_ids_seq;
DROP TABLE IF EXISTS tpls_collections;
DROP SEQUENCE IF EXISTS tpls_collections_ids_seq;
DROP TABLE IF EXISTS tplscat_inner_configs;
DROP SEQUENCE IF EXISTS tplscat_inner_configs_ids_seq;
DROP TABLE IF EXISTS lang_localizables;
DROP TABLE IF EXISTS language_iso639_3;
DROP TABLE IF EXISTS nameables;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

CREATE TABLE nameables ( -- an ancestor table
         name        varchar NOT NULL DEFAULT '<name not given>'
       , description varchar     NULL 
       , comments    varchar     NULL
) TABLESPACE tabsp_pcltcatalogs;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE nameables TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE nameables TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE TABLE language_iso639_3 ( -- an ancestor table
	lng varchar(3) PRIMARY KEY
) INHERITS (nameables) TABLESPACE tabsp_pcltcatalogs;

INSERT INTO language_iso639_3 (lng, name) VALUES ('rus', 'Russian'), ('eng', 'English');

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE language_iso639_3 TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE language_iso639_3 TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE TABLE lang_localizables ( -- an ancestor table
	lng varchar(3) NOT NULL REFERENCES language_iso639_3 (lng) ON DELETE RESTRICT ON UPDATE CASCADE
) TABLESPACE tabsp_pcltcatalogs;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE lang_localizables TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE lang_localizables TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE SEQUENCE tplscat_inner_configs_ids_seq INCREMENT BY 1
       MINVALUE 100
       START WITH 100
       NO CYCLE;

CREATE TABLE tplscat_inner_configs (
          tplscat_inner_config_id integer DEFAULT nextval('tplscat_inner_configs_ids_seq') PRIMARY KEY

        , CompositePlaceholderWrapper    varchar(10) NOT NULL DEFAULT '##|'
        , ParameterPlaceholderWrapper    varchar(10) NOT NULL DEFAULT '@@|'
        , InsuficientDetLevelPlaceholder varchar(10) NOT NULL DEFAULT '#x#'
        , MarkingErrorPlaceholderWrapper varchar(10) NOT NULL DEFAULT '/!E!\\'
        , DefaultLanguage                varchar(3)  NOT NULL DEFAULT 'eng' REFERENCES language_iso639_3 (lng) ON DELETE RESTRICT ON UPDATE CASCADE
        , StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets
                                         varchar     NOT NULL DEFAULT
' StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets {
  soStrict_IsIt = True
, soExcludingInComposites = []
, soExcludingComposites   = []
, soExcludingParameters   = []
, soExcludingCompComposites = []
, soExcludingCompParameters = []
}'
        , AllowUntemplatedMessages       boolean NOT NULL DEFAULT FALSE
        , AllowUntemplatedLocalizationsOfMessages
                                         boolean NOT NULL DEFAULT FALSE
        , ShowAdhocParamsInResultOfUntemplated
                                         boolean NOT NULL DEFAULT FALSE
        , InstaniationResultMaxSize      integer NOT NULL DEFAULT 10000000
        , AllowEmptySDL_parseItByModusMargin
                                         boolean NOT NULL DEFAULT TRUE
        , AllowUnreadableSDL_parseIdByModusMargin
                                         boolean NOT NULL DEFAULT TRUE
        , ReparsingDepthMax              integer NOT NULL DEFAULT 100
        , ReparseParameterContentMaxSize integer NOT NULL DEFAULT 50000
        , Newline                        varchar NOT NULL DEFAULT E'\n'

) INHERITS (nameables) TABLESPACE tabsp_pcltcatalogs;
INSERT INTO tplscat_inner_configs(tplscat_inner_config_id, name) VALUES (1, 'Default config');

GRANT USAGE                          ON SEQUENCE tplscat_inner_configs_ids_seq TO user_pcltCatalogs_data_admin;
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE    tplscat_inner_configs         TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE    tplscat_inner_configs         TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE SEQUENCE tpls_collections_ids_seq INCREMENT BY 1
       MINVALUE 100
       START WITH 100
       NO CYCLE;

CREATE TABLE tpls_collections (
         tpls_collection_id integer DEFAULT nextval('tpls_collections_ids_seq') PRIMARY KEY
) INHERITS (nameables) TABLESPACE tabsp_pcltcatalogs;

INSERT INTO tpls_collections(tpls_collection_id, name) VALUES (1, 'General commons');
INSERT INTO tpls_collections(tpls_collection_id, name) VALUES (2, 'General DB commons');
INSERT INTO tpls_collections(tpls_collection_id, name) VALUES (3, 'General networking commons');
INSERT INTO tpls_collections(tpls_collection_id, name) VALUES (4, 'PCLT messages');
INSERT INTO tpls_collections(tpls_collection_id, name) VALUES (5, 'PCLT-DB messages');
INSERT INTO tpls_collections(tpls_collection_id, name) VALUES (10, 'Default commons');

GRANT USAGE                          ON SEQUENCE tpls_collections_ids_seq TO user_pcltCatalogs_data_admin;
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE    tpls_collections         TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE    tpls_collections         TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE SEQUENCE tpls_detalization_policies_ids_seq INCREMENT BY 1
       MINVALUE 100
       START WITH 100
       NO CYCLE;

CREATE TABLE tpls_detalization_policies (
         tpls_detalization_policy_id integer DEFAULT nextval('tpls_detalization_policies_ids_seq') PRIMARY KEY
) INHERITS (nameables) TABLESPACE tabsp_pcltcatalogs;
INSERT INTO tpls_detalization_policies (tpls_detalization_policy_id, name, description) VALUES 
          (1, 'Minimal detailization requirements for all', 'The policy gets automatically filled by the AFTER INSERT trigger on tpls.')
        , (5,  'Custom detailization requirements for all', 'The policy gets automatically filled by the AFTER INSERT trigger on tpls.')
        , (9, 'Maximal detailization requirements for all', 'The policy gets automatically filled by the AFTER INSERT trigger on tpls.');

GRANT USAGE                          ON SEQUENCE tpls_detalization_policies_ids_seq TO user_pcltCatalogs_data_admin;
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE    tpls_detalization_policies         TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE    tpls_detalization_policies         TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE SEQUENCE tpls_catalogs_ids_seq INCREMENT BY 1
       MINVALUE 100 
       START WITH 100 
       NO CYCLE;

CREATE TABLE tpls_catalogs (
         tpls_catalog_id             integer NOT NULL DEFAULT nextval('tpls_catalogs_ids_seq') PRIMARY KEY
       , tplscat_inner_config_id     integer NOT NULL DEFAULT 1 REFERENCES tplscat_inner_configs(tplscat_inner_config_id) ON DELETE RESTRICT ON UPDATE CASCADE
       , cat_new_version_available   boolean NOT NULL DEFAULT TRUE
       , tpls_detalization_policy_id integer     NULL           REFERENCES tpls_detalization_policies(tpls_detalization_policy_id) ON DELETE RESTRICT ON UPDATE CASCADE
) INHERITS (nameables) TABLESPACE tabsp_pcltcatalogs;

INSERT INTO tpls_catalogs(tpls_catalog_id, name) VALUES (1, 'PCLT-DB initial catalog');

GRANT USAGE                              ON SEQUENCE tpls_catalogs_ids_seq TO user_pcltCatalogs_data_admin;
GRANT SELECT, INSERT, UPDATE, DELETE     ON TABLE    tpls_catalogs         TO user_pcltCatalogs_data_admin;
GRANT SELECT                             ON TABLE    tpls_catalogs         TO user_pcltCatalogs_data_reader;
GRANT UPDATE (cat_new_version_available) ON TABLE    tpls_catalogs         TO user_pcltCatalogs_data_reader; -- in next version this workaround will be removed

------------------------------------------------------------------------------

CREATE TABLE tpls (
         pclt_id varchar PRIMARY KEY
) INHERITS (nameables) TABLESPACE tabsp_pcltcatalogs;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE tpls TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE tpls TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE TABLE tpls_localized (
         pclt_id         varchar NOT NULL REFERENCES tpls (pclt_id) ON DELETE CASCADE ON UPDATE CASCADE
       , structured_text varchar NOT NULL
       , FOREIGN KEY (lng) REFERENCES language_iso639_3 (lng) ON DELETE RESTRICT ON UPDATE CASCADE
       , PRIMARY KEY (lng, pclt_id)
) INHERITS (lang_localizables) TABLESPACE tabsp_pcltcatalogs;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE tpls_localized TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE tpls_localized TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE TABLE tpls_inclusions_in_collections (
         tpls_collection_id integer NOT NULL REFERENCES tpls_collections(tpls_collection_id) ON DELETE CASCADE ON UPDATE CASCADE
       , pclt_id            varchar NOT NULL REFERENCES tpls (pclt_id) ON DELETE CASCADE ON UPDATE CASCADE
       , tpl_dflt_req_sdl   varchar NOT NULL -- '+inf' < {number} < 'one' < 'zero' 
       , PRIMARY KEY (tpls_collection_id, pclt_id)
) TABLESPACE tabsp_pcltcatalogs;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE tpls_inclusions_in_collections TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE tpls_inclusions_in_collections TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE TABLE tpls_representations_sdls (
         tpls_detalization_policy_id integer NOT NULL REFERENCES tpls_detalization_policies(tpls_detalization_policy_id) ON DELETE RESTRICT ON UPDATE CASCADE
       , pclt_id                     varchar NOT NULL REFERENCES tpls (pclt_id) ON DELETE CASCADE ON UPDATE CASCADE
       , tpl_req_sdl                 varchar     NULL -- '+inf' < {number} < 'one' < 'zero' 
       , PRIMARY KEY (pclt_id, tpls_detalization_policy_id)
) TABLESPACE tabsp_pcltcatalogs;

-- A fast way to regulate current requirement of representation reciever's required detailization level minimum.
-- On every insertion of every template (in tpls table) a trigger gets executed, 
-- which lookups from tpls_representations_sdls table first row that uses 5th policy, remembers used tpl_req_sdl, 
-- inserts a new row into tpls_representations_sdls with same policy ID and tpl_req_sdl and with a reference 
-- on a newly created template. 
-- This way, there is an account for each template in 5th policy. In a similar way 1st and 9th policy gets filled,
-- but for these tpl_req_sdl is constant - 'zero' for 1st and '+inf' for 9th. Using simple funcion below one may 
-- perform fast regulation or tpl_req_sdl used by 5th policy. 
-- Then by fast switching of policy used by catalog from some userdefined to 1st, 5th or 9th user may regulate 
-- representation requirement for all template at once.
CREATE OR REPLACE FUNCTION setSDL_forAll5 (varchar) RETURNS VOID AS $$ -- $1 is a new tpl_req_sdl for all templates in a 5th policy
        UPDATE tpls_representations_sdls SET tpl_req_sdl = $1 WHERE tpls_detalization_policy_id = 5;
$$ LANGUAGE SQL;

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE tpls_representations_sdls   TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE tpls_representations_sdls   TO user_pcltCatalogs_data_reader;
GRANT EXECUTE                        ON FUNCTION setSDL_forAll5 (varchar) TO user_pcltCatalogs_data_admin;

------------------------------------------------------------------------------

CREATE TABLE collections_inclusions_in_catalogs (
         tpls_collection_id integer NOT NULL REFERENCES tpls_collections(tpls_collection_id) ON DELETE CASCADE ON UPDATE CASCADE
       , tpls_catalog_id    integer NOT NULL REFERENCES tpls_catalogs(tpls_catalog_id)       ON DELETE CASCADE ON UPDATE CASCADE
       , PRIMARY KEY (tpls_collection_id, tpls_catalog_id)
) TABLESPACE tabsp_pcltcatalogs;

INSERT INTO collections_inclusions_in_catalogs(tpls_catalog_id, tpls_collection_id) VALUES (1 , 10);

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE collections_inclusions_in_catalogs TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE collections_inclusions_in_catalogs TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------

CREATE TABLE collections_dependencies (
         tpls_collection_rer_id integer NOT NULL REFERENCES tpls_collections(tpls_collection_id) ON DELETE CASCADE  ON UPDATE CASCADE
       , tpls_collection_red_id integer NOT NULL REFERENCES tpls_collections(tpls_collection_id) ON DELETE RESTRICT ON UPDATE CASCADE
         -- rer = requirer; red = required
       , PRIMARY KEY (tpls_collection_rer_id, tpls_collection_red_id)
) TABLESPACE tabsp_pcltcatalogs;

INSERT INTO collections_dependencies(tpls_collection_rer_id, tpls_collection_red_id) VALUES (10,  1);
INSERT INTO collections_dependencies(tpls_collection_rer_id, tpls_collection_red_id) VALUES (10,  2);
INSERT INTO collections_dependencies(tpls_collection_rer_id, tpls_collection_red_id) VALUES (10,  3);
INSERT INTO collections_dependencies(tpls_collection_rer_id, tpls_collection_red_id) VALUES (10,  4);
INSERT INTO collections_dependencies(tpls_collection_rer_id, tpls_collection_red_id) VALUES (10,  5);

GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE collections_dependencies TO user_pcltCatalogs_data_admin;
GRANT SELECT                         ON TABLE collections_dependencies TO user_pcltCatalogs_data_reader;

------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION catalogs_thatuse_tpls_collections (integer,integer) RETURNS SETOF integer AS $$ 
        SELECT tpls_catalog_id
        FROM collections_inclusions_in_catalogs
        WHERE tpls_collection_id = $1 OR tpls_collection_id = $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION catalogs_thatuse_tpls (varchar, varchar) RETURNS SETOF integer AS $$ 
        SELECT tpls_catalog_id
        FROM collections_inclusions_in_catalogs ciic
           , tpls_inclusions_in_collections tiic
        WHERE ciic.tpls_collection_id = tiic.tpls_collection_id
          AND (tiic.pclt_id = $1 OR tiic.pclt_id = $2)
$$ LANGUAGE SQL;

GRANT EXECUTE ON FUNCTION catalogs_thatuse_tpls_collections  (integer,integer)                 TO user_pcltCatalogs_data_admin;
GRANT EXECUTE ON FUNCTION catalogs_thatuse_tpls_collections  (integer,integer)                 TO user_pcltCatalogs_data_reader;
GRANT EXECUTE ON FUNCTION catalogs_thatuse_tpls              (varchar,varchar)                 TO user_pcltCatalogs_data_admin;
GRANT EXECUTE ON FUNCTION catalogs_thatuse_tpls              (varchar,varchar)                 TO user_pcltCatalogs_data_reader;

--------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION tpls_catalogs_onupdate() RETURNS trigger AS $tri_tpls_catalogs_onupdate$ -- upd
BEGIN
        NEW.cat_new_version_available := TRUE;
        RETURN NEW;
END;
$tri_tpls_catalogs_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tplscat_inner_configs_onupdate() RETURNS trigger AS $tri_tplscat_inner_configs_onupdate$ -- upd
BEGIN
        UPDATE tpls_catalogs AS tc
        SET cat_new_version_available = TRUE 
        WHERE tc.tplscat_inner_config_id = OLD.tplscat_inner_config_id
           OR tc.tplscat_inner_config_id = NEW.tplscat_inner_config_id;

        RETURN NEW;
END;
$tri_tplscat_inner_configs_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tpls_collections_onupdate() RETURNS trigger AS $tri_tpls_collections_onupdate$ -- upd, del
DECLARE
        old_col_id integer := NULL;
BEGIN
        IF TG_OP = 'UPDATE' THEN
                old_col_id := OLD.tpls_collection_id;
        END IF;
        UPDATE tpls_catalogs AS tc 
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_catalog_id IN (SELECT * FROM catalogs_thatuse_tpls_collections (old_col_id, NEW.tpls_collection_id));

        RETURN NEW;
END;
$tri_tpls_collections_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tpls_onupdate() RETURNS trigger AS $tri_tpls_onupdate$ -- upd, del
DECLARE
        old_tpl_id varchar := NULL;
BEGIN
        IF TG_OP = 'UPDATE' THEN
                old_tpl_id := OLD.pclt_id;
        END IF;
        UPDATE tpls_catalogs AS tc 
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_catalog_id IN (SELECT * FROM catalogs_thatuse_tpls (old_tpl_id, NEW.pclt_id));

        RETURN NEW;
END;
$tri_tpls_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tpls_oninsert() RETURNS trigger AS $tri_tpls_oninsert$ -- ins
DECLARE
        custom_sdl varchar;
BEGIN 
        SELECT tpl_req_sdl INTO custom_sdl FROM tpls_representations_sdls WHERE tpls_detalization_policy_id = 5 LIMIT 1;
        INSERT INTO tpls_representations_sdls (tpls_detalization_policy_id, pclt_id, tpl_req_sdl) 
                SELECT tpls_detalization_policy_id
                     , NEW.pclt_id
                     , CASE tpls_detalization_policy_id 
                           WHEN 1 THEN 'zero'
                           WHEN 5 THEN custom_sdl
                           WHEN 9 THEN '+inf'
		       END
                FROM tpls_detalization_policies
                WHERE tpls_detalization_policy_id IN (1, 5, 9);

        RETURN NEW;
END;
$tri_tpls_oninsert$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tpls_localized_onupdate() RETURNS trigger AS $tri_tpls_localized_onupdate$ -- upd, del, ins
DECLARE
        old_tpl_id varchar := NULL;
        new_tpl_id varchar := NULL;
BEGIN
        IF    TG_OP = 'INSERT' THEN
                new_tpl_id := NEW.pclt_id;
        ELSIF TG_OP = 'DELETE' THEN
                old_tpl_id := OLD.pclt_id;
        ELSIF TG_OP = 'UPDATE' THEN
                new_tpl_id := NEW.pclt_id;
                old_tpl_id := OLD.pclt_id;
        END IF;
        UPDATE tpls_catalogs AS tc 
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_catalog_id IN (SELECT * FROM catalogs_thatuse_tpls (new_tpl_id, old_tpl_id));

        RETURN NEW;
END;
$tri_tpls_localized_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tpls_inclusions_in_collections_onupdate() RETURNS trigger AS $tri_tpls_inclusions_in_collections_onupdate$ -- upd, del, ins
DECLARE
        old_col_id integer := NULL;
        new_col_id integer := NULL;
BEGIN
        IF    TG_OP = 'INSERT' THEN
                new_col_id := NEW.tpls_collection_id;
        ELSIF TG_OP = 'DELETE' THEN
                old_col_id := OLD.tpls_collection_id;
        ELSIF TG_OP = 'UPDATE' THEN
                new_col_id := NEW.tpls_collection_id;
                old_col_id := OLD.tpls_collection_id;
        END IF;
        UPDATE tpls_catalogs AS tc 
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_catalog_id IN (SELECT * FROM catalogs_thatuse_tpls_collections (old_col_id, new_col_id));

        RETURN NEW;
END;
$tri_tpls_inclusions_in_collections_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION tpls_representations_sdls_onupdate() RETURNS trigger AS $tri_tpls_representations_sdls_onupdate$ -- upd, ins, del
DECLARE
        old_dp_id integer := NULL;
        new_dp_id integer := NULL;
BEGIN
        IF    TG_OP = 'INSERT' THEN
                new_dp_id := NEW.tpls_detalization_policy_id;
        ELSIF TG_OP = 'DELETE' THEN
                old_dp_id := OLD.tpls_detalization_policy_id;
        ELSIF TG_OP = 'UPDATE' THEN
                new_dp_id := NEW.tpls_detalization_policy_id;
                old_dp_id := OLD.tpls_detalization_policy_id;
        END IF;
        UPDATE tpls_catalogs AS tc
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_detalization_policy_id = old_dp_id
           OR tc.tpls_detalization_policy_id = new_dp_id;

        RETURN NEW;
END;
$tri_tpls_representations_sdls_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION collections_inclusions_in_catalogs_onupdate() RETURNS trigger AS $tri_collections_inclusions_in_catalogs_onupdate$ -- upd, ins, del
DECLARE
        old_cat_id integer := NULL;
        new_cat_id integer := NULL;
BEGIN
        IF    TG_OP = 'INSERT' THEN
                new_cat_id := NEW.tpls_catalog_id;
        ELSIF TG_OP = 'DELETE' THEN
                old_cat_id := OLD.tpls_catalog_id;
        ELSIF TG_OP = 'UPDATE' THEN
                new_cat_id := NEW.tpls_catalog_id;
                old_cat_id := OLD.tpls_catalog_id;
        END IF;
        UPDATE tpls_catalogs AS tc
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_catalog_id = old_cat_id
           OR tc.tpls_catalog_id = new_cat_id;

        RETURN NEW;
END;
$tri_collections_inclusions_in_catalogs_onupdate$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION collections_dependencies_onupdate() RETURNS trigger AS $tri_collections_dependencies_onupdate$ -- upd, ins, del
DECLARE
        old_col_id integer := NULL;
        new_col_id integer := NULL;
BEGIN
        IF    TG_OP = 'INSERT' THEN
                new_col_id := NEW.tpls_collection_rer_id;
        ELSIF TG_OP = 'DELETE' THEN
                old_col_id := OLD.tpls_collection_rer_id;
        ELSIF TG_OP = 'UPDATE' THEN
                new_col_id := NEW.tpls_collection_rer_id;
                old_col_id := OLD.tpls_collection_rer_id;
        END IF;
        UPDATE tpls_catalogs AS tc
        SET cat_new_version_available = TRUE 
        WHERE tc.tpls_catalog_id IN (SELECT * FROM catalogs_thatuse_tpls_collections (old_col_id, new_col_id));

        RETURN NEW;
END;
$tri_collections_dependencies_onupdate$ LANGUAGE plpgsql;

\i data.sql

CREATE TRIGGER tri_tpls_catalogs_onupdate                      AFTER UPDATE ON tpls_catalogs
    FOR EACH ROW EXECUTE PROCEDURE tpls_catalogs_onupdate();
CREATE TRIGGER tri_tplscat_inner_configs_onupdate              AFTER UPDATE ON tplscat_inner_configs
    FOR EACH ROW EXECUTE PROCEDURE tplscat_inner_configs_onupdate();
CREATE TRIGGER tri_tpls_collections_onupdate                   AFTER UPDATE OR DELETE ON tpls_collections
    FOR EACH ROW EXECUTE PROCEDURE tpls_collections_onupdate();
CREATE TRIGGER tri_tpls_onupdate                               AFTER UPDATE OR DELETE ON tpls
    FOR EACH ROW EXECUTE PROCEDURE tpls_onupdate();
CREATE TRIGGER tri_tpls_oninsert                               AFTER UPDATE OR DELETE ON tpls
    FOR EACH ROW EXECUTE PROCEDURE tpls_oninsert();
CREATE TRIGGER tri_tpls_localized_onupdate                     AFTER UPDATE OR DELETE OR INSERT ON tpls_localized
    FOR EACH ROW EXECUTE PROCEDURE tpls_localized_onupdate();
CREATE TRIGGER tri_tpls_inclusions_in_collections_onupdate     AFTER UPDATE OR DELETE OR INSERT ON tpls_inclusions_in_collections
    FOR EACH ROW EXECUTE PROCEDURE tpls_inclusions_in_collections_onupdate();
CREATE TRIGGER tri_tpls_representations_sdls_onupdate          AFTER UPDATE OR DELETE OR INSERT ON tpls_representations_sdls
    FOR EACH ROW EXECUTE PROCEDURE tpls_representations_sdls_onupdate();
CREATE TRIGGER tri_collections_inclusions_in_catalogs_onupdate AFTER UPDATE OR DELETE OR INSERT ON collections_inclusions_in_catalogs
    FOR EACH ROW EXECUTE PROCEDURE collections_inclusions_in_catalogs_onupdate();
CREATE TRIGGER tri_collections_dependencies_onupdate           AFTER UPDATE OR DELETE OR INSERT ON collections_dependencies
    FOR EACH ROW EXECUTE PROCEDURE collections_dependencies_onupdate();