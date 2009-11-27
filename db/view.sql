-- Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>
-- 
-- All rights reserved.
-- 
-- For license and copyright information, see the file COPYRIGHT

--------------------------------------------------------------------------
--------------------------------------------------------------------------

\c pcltcatalogs user_pcltcatalogs_owner

SET search_path TO sch_pcltcatalogs, public;

DROP FUNCTION IF EXISTS in_catalog_localized_tpls_with_their_sdls(integer); 
DROP TYPE     IF EXISTS type__catalog_ltpl_row;

CREATE TYPE type__catalog_ltpl_row AS (
      pclt_id         varchar
    , tpl_req_sdl     varchar
    , structured_text varchar
    , lng             varchar(3)
);

CREATE OR REPLACE FUNCTION in_catalog_localized_tpls_with_their_sdls (integer) RETURNS SETOF type__catalog_ltpl_row AS $$
-- i don't know, why it doesn't allow me to name parameter as par__cat_id... well $1 = catalog_id for wich we are viewing catalog entries...
        WITH RECURSIVE included_collections (tpls_collection_rer_id, tpls_collection_red_id) AS (
                       SELECT tpls_collection_rer_id, tpls_collection_red_id 
                       FROM sch_pcltcatalogs.collections_dependencies 
                       WHERE tpls_collection_rer_id IN (
                                        SELECT tpls_collection_id 
                                        FROM sch_pcltcatalogs.collections_inclusions_in_catalogs 
                                        WHERE tpls_catalog_id = $1
                                        )
                  UNION 
                       SELECT cd.tpls_collection_rer_id, cd.tpls_collection_red_id 
                       FROM included_collections ic, sch_pcltcatalogs.collections_dependencies cd
                       WHERE ic.tpls_collection_red_id = cd.tpls_collection_rer_id 
             ), used_collections AS (
                       SELECT tpls_collection_id 
                       FROM (
                          SELECT tpls_collection_id 
                          FROM sch_pcltcatalogs.collections_inclusions_in_catalogs 
                          WHERE tpls_catalog_id = $1
                       ) AS top_collection_ids
                  UNION
                       SELECT tpls_collection_red_id AS tpls_collection_id FROM included_collections
             ), used_tpls_w_default_sdls AS ( 
                  SELECT pclt_id, tpl_dflt_req_sdl 
                  FROM sch_pcltcatalogs.tpls_inclusions_in_collections tiic, used_collections uc 
                  WHERE uc.tpls_collection_id = tiic.tpls_collection_id 
             ), used_tpls_representations_sdls AS (
                  SELECT trs.pclt_id, trs.tpl_req_sdl
                  FROM sch_pcltcatalogs.tpls_representations_sdls trs
                     , sch_pcltcatalogs.tpls_detalization_policies trp
                     , sch_pcltcatalogs.tpls_catalogs tc
                WHERE  tc.tpls_detalization_policy_id = trp.tpls_detalization_policy_id
                    AND trs.tpls_detalization_policy_id = trp.tpls_detalization_policy_id
             ), used_tpls_w_sdls AS (
                  SELECT DISTINCT utds.pclt_id, CASE utrs.tpl_req_sdl is NULL WHEN TRUE THEN utds.tpl_dflt_req_sdl ELSE utrs.tpl_req_sdl END AS tpl_req_sdl
                  FROM used_tpls_w_default_sdls utds
                              LEFT OUTER JOIN
                       used_tpls_representations_sdls utrs
                              ON utds.pclt_id = utrs.pclt_id
             ), used_localized_tpls_w_sdls AS (
                  SELECT tl.pclt_id, uts.tpl_req_sdl, tl.structured_text, tl.lng
                  FROM used_tpls_w_sdls uts
                     , sch_pcltcatalogs.tpls_localized tl 
                  WHERE tl.pclt_id = uts.pclt_id
             )
          SELECT * FROM used_localized_tpls_w_sdls;
$$ LANGUAGE SQL;

GRANT EXECUTE ON FUNCTION in_catalog_localized_tpls_with_their_sdls (integer) TO user_pcltCatalogs_data_admin;
GRANT EXECUTE ON FUNCTION in_catalog_localized_tpls_with_their_sdls (integer) TO user_pcltCatalogs_data_reader;

-------------------------------------------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION by_catalog_used_config (integer) RETURNS SETOF tplscat_inner_configs AS $$
        SELECT tic.* 
        FROM sch_pcltcatalogs.tplscat_inner_configs AS tic 
           , sch_pcltcatalogs.tpls_catalogs AS tcat 
        WHERE tic.tplscat_inner_config_id = tcat.tplscat_inner_config_id -- dont know why NATURAL JOIN didn't work
          AND tcat.tpls_catalog_id = $1;
$$ LANGUAGE SQL;

GRANT EXECUTE ON FUNCTION by_catalog_used_config (integer) TO user_pcltCatalogs_data_admin;
GRANT EXECUTE ON FUNCTION by_catalog_used_config (integer) TO user_pcltCatalogs_data_reader;
