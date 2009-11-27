-- Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>
-- 
-- All rights reserved.
-- 
-- For license and copyright information, see the file COPYRIGHT

--------------------------------------------------------------------------
--------------------------------------------------------------------------

\c pcltcatalogs user_pcltcatalogs_data_admin

SET search_path TO sch_pcltcatalogs, public; -- sets only for current session

INSERT INTO tplscat_inner_configs(tplscat_inner_config_id, name, StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets) 
VALUES ( 50
       , 'HelloWorld config'
       ,    ' StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets {' 
         || '  soStrict_IsIt = True'
         || ', soExcludingInComposites = []'
         || ', soExcludingComposites   = []'
         || ', soExcludingParameters   = []'
         || ', soExcludingCompComposites = []'
         || ', soExcludingCompParameters = [("E_HWE_AMBWRLDCH_OW", "__row_idx")]' -- about that read in the explanations to the config in version of HelloWorld that uses only PCLT (but no PCLT-DB)
         || '}'
       );
INSERT INTO tpls_catalogs(tpls_catalog_id, tplscat_inner_config_id, name, description) VALUES (50, 50, 'HelloWorld catalog', 'PCLT catalog for testing of packages PCLT and PCLT-DB by application "HelloWorld"');
INSERT INTO tpls_collections(tpls_collection_id, name, description) VALUES (50, 'HelloWorld templates', 'A collection of templates for testing of packages PCLT and PCLT-DB by application "HelloWorld". Contains (by dependencies) the collection (ID:10) "Default commons", which contains all the templates, that are used by PCLT and PCLT-DB packages + templates used by "HelloWorld" application.');
INSERT INTO collections_inclusions_in_catalogs(tpls_catalog_id, tpls_collection_id) VALUES (50, 50); -- collection dedicated to HelloWorld catalog 
INSERT INTO collections_dependencies(tpls_collection_rer_id, tpls_collection_red_id) VALUES (50, 10); -- collection with ID 10 contains all the templates that are used by PCLT and PCLT-DB packages
INSERT INTO language_iso639_3 (lng, name) VALUES ('hs_', 'Haskell'); -- used by HelloWorld example

-- These \/ are generated in "HelloWorld.PCLTCatalog.ods" file
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'TRUE', 'True'); -- these are already in catalog, we just add localizations for "hs_" language
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'FALSE', 'False');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'MAYBE_A', '@@|maybe_cnstr@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'MAYBE_JUST', 'Just @@|a@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'MAYBE_NOTHING', 'Nothing');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'LLEXCPT', 'SomeException (ErrorCall "@@|excpt_msg@@|")');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'DB_ERROR', 'SqlError { seState = "@@|seState@@|", seNativeError = @@|seNativeError@@|, seErrorMsg = "@@|seErrorMsg@@|" }');

INSERT INTO tpls (pclt_id) VALUES('HW');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'HW', 'Привет, Мир!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'HW', 'Hello world!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'HW', 'HelloWorld');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'HW','0');

INSERT INTO tpls (pclt_id) VALUES('E_HWE');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE', '##|E_HWE_PREFIX##|@@|hwe_details_pcsi@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE', '##|E_HWE_PREFIX##|@@|hwe_details_pcsi@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE', '##|E_HWE_PREFIX##|@@|hwe_details_pcsi@@|');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE','10');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_PREFIX');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_PREFIX', 'Приветствие мира не удалось!\nПричина: ');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_PREFIX', 'Hello world failure!\nReason: ');

INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_PREFIX','##|E_HWE##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_NOWRLD');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_NOWRLD', 'некого приветствовать (нет мира)!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_NOWRLD', 'no world!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE_NOWRLD', 'NoWorld_HWE');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_NOWRLD','##|E_HWE##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_AMBWRLDCH');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_AMBWRLDCH', 'неясно, какой из миров приветствовать - их несколько!\nПервый мир: [имя: ''@@|w_name_1@@|'', индекс: @@|w_idx_1@@|].\nВторой мир: [имя: ''@@|w_name_2@@|'', индекс: @@|w_idx_2@@|].\nА так же эти миры: \n   @@|other_worlds@@|.');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_AMBWRLDCH', 'ambiguous choice of worlds!\nFirst world: [name: ''@@|w_name_1@@|'', index: @@|w_idx_1@@|].\nSecond world: [name: ''@@|w_name_2@@|'', index: @@|w_idx_2@@|].\nAnd also these worlds: \n   @@|other_worlds@@|.');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE_AMBWRLDCH', 'AmbiguousChoiceOfWorlds_HWE\n   ("@@|w_name_1@@|", @@|w_idx_1@@|)\n   ("@@|w_name_2@@|", @@|w_idx_2@@|)\n   [ @@|other_worlds@@|\n   ]');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_AMBWRLDCH','##|E_HWE##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_AMBWRLDCH_OW');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_AMBWRLDCH_OW', '@@|__row_idx@@|) мир [имя: ''@@|w_name@@|'', индекс: @@|w_idx@@|]');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_AMBWRLDCH_OW', '@@|__row_idx@@|) world [name: ''@@|w_name@@|'', index: @@|w_idx@@|]');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE_AMBWRLDCH_OW', '("@@|w_name@@|", @@|w_idx@@|)');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_AMBWRLDCH_OW','##|E_HWE_AMBWRLDCH##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_AMBWRLDCH_OW_SEP');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_AMBWRLDCH_OW_SEP', '\n');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_AMBWRLDCH_OW_SEP', '\n');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE_AMBWRLDCH_OW_SEP', '\n, ');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_AMBWRLDCH_OW_SEP','##|E_HWE_AMBWRLDCH_OW##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_AMBWRLDCH_NOMORE');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_AMBWRLDCH_NOMORE', 'список пуст.');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_AMBWRLDCH_NOMORE', 'empty list.');

INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_AMBWRLDCH_NOMORE','##|E_HWE_AMBWRLDCH_OW##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_STRNGERR');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_STRNGERR', 'какая-то странная непонятная ошибка! Данные: @@|int@@| "@@|str@@|" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) { @@|sm_excpt@@| }');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_STRNGERR', 'some very strange error! Data: @@|int@@| "@@|str@@|" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) { @@|sm_excpt@@| }');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE_STRNGERR', 'SomeVeryStrangeError_HWE @@|int@@| "@@|str@@|" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) (@@|sm_excpt@@|)');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_STRNGERR','##|E_HWE##|');
INSERT INTO tpls (pclt_id) VALUES('E_HWE_EIS');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_HWE_EIS', 'ошибка в подсистеме!\nТекст исключения уровнем ниже:\n##|E_EIS##|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_HWE_EIS', 'failed due to error(s) in subsystem!\nLower level exception message:\n##|E_EIS##|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_HWE_EIS', 'FailedDueToErrorInSubsystem_HWE (##|E_EIS##|)');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_HWE_EIS','##|E_HWE##|');

INSERT INTO tpls (pclt_id) VALUES('E_EIS');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EIS', '##|E_EIS_PREFIX##|@@|eis_details_pcsi@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EIS', '##|E_EIS_PREFIX##|@@|eis_details_pcsi@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EIS', '##|E_EIS_PREFIX##|@@|eis_details_pcsi@@|');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EIS','20');
INSERT INTO tpls (pclt_id) VALUES('E_EIS_PREFIX');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EIS_PREFIX', 'Сбой в подсистеме!\nПричина: ');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EIS_PREFIX', 'Subsystem failure!\nReason: ');

INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EIS_PREFIX','##|E_EIS##|');
INSERT INTO tpls (pclt_id) VALUES('E_EIS_ET1');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EIS_ET1', 'ошибка типа #1!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EIS_ET1', 'error of type #1!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EIS_ET1', 'ErrorType1_EIS');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EIS_ET1','##|E_EIS##|');
INSERT INTO tpls (pclt_id) VALUES('E_EIS_ET2');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EIS_ET2', 'ошибка типа #2!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EIS_ET2', 'error of type #2!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EIS_ET2', 'ErrorType2_EIS');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EIS_ET2','##|E_EIS##|');
INSERT INTO tpls (pclt_id) VALUES('E_EIS_EISS');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EIS_EISS', 'сбой в подПОДсистеме! Текст исключения уровнем ниже: @@|e_eiss@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EIS_EISS', 'failed due to error(s) in subSUBsystem!\nLower level exception message:\n@@|e_eiss@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EIS_EISS', 'FailedDueToErrorInSub_sub_system_EIS (@@|e_eiss@@|)');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EIS_EISS','@@|e_eiss@@|');

INSERT INTO tpls (pclt_id) VALUES('E_EISS');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EISS', '##|E_EISS_PREFIX##|@@|eiss_details_pcsi@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EISS', '##|E_EISS_PREFIX##|@@|eiss_details_pcsi@@|');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EISS', '##|E_EISS_PREFIX##|@@|eiss_details_pcsi@@|');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EISS','20');
INSERT INTO tpls (pclt_id) VALUES('E_EISS_PREFIX');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EISS_PREFIX', 'Сбой в подПОДсистеме!\nПричина: ');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EISS_PREFIX', 'SubSUBsystem failure!\nReason: ');

INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EISS_PREFIX','##|E_EISS##|');
INSERT INTO tpls (pclt_id) VALUES('E_EISS_ET1');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EISS_ET1', 'ошибка типа #1!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EISS_ET1', 'error of type #1!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EISS_ET1', 'ErrorType1_EISS');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EISS_ET1','##|E_EISS##|');
INSERT INTO tpls (pclt_id) VALUES('E_EISS_ET2');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('rus', 'E_EISS_ET2', 'ошибка типа #2!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('eng', 'E_EISS_ET2', 'error of type #2!');
INSERT INTO tpls_localized (lng, pclt_id, structured_text) VALUES('hs_', 'E_EISS_ET2', 'ErrorType2_EISS');
INSERT INTO tpls_inclusions_in_collections (tpls_collection_id, pclt_id, tpl_dflt_req_sdl) VALUES (50,'E_EISS_ET2','##|E_EISS##|');
