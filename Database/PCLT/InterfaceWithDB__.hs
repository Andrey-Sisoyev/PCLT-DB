{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}

module Database.PCLT.InterfaceWithDB__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Database.ReadableFromDB__
import Database.PCLT.InterfaceWithDB

instance ShowAsPCSI AddressedDropFlag_CatalogNeedsToBeUpdated_Error where
        showAsPCSI (AddressedDropFlag_CatalogNeedsToBeUpdated_Error dfcntbue cat_id) =
                thePCSI "E_PCLTC_DFCNTBUE" [("pclt_dfcntbue_err_details", PCSI_PV err_pcsi), ("cat_id", PlainText_PV $ show cat_id)]
              where
                err_pcsi =
                  case dfcntbue of
                      NotModified_DFCNTBUE ->
                            empPCSI "E_PCLTC_DFCNTBUE_NOMODIFY"
                      DBError_DFCNTBUE sql_err ->
                            addToPCSI
                                [showAsPCSI sql_err]
                                (empPCSI "E_PCLTC_DFCNTBUE_DBERR")

instance HasStaticRawPCLTs AddressedDropFlag_CatalogNeedsToBeUpdated_Error where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLTC_DFCNTBUE", (M.fromList [("rus", B.pack "##|E_PCLTC_DFCNTBUE_PREFIX##|@@|pclt_dfcntbue_err_details@@|."), ("eng", B.pack "##|E_PCLTC_DFCNTBUE_PREFIX##|@@|pclt_dfcntbue_err_details@@|.")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLTC_DFCNTBUE_PREFIX", (M.fromList [("rus", B.pack "Произошла ошибка при попытке спустить флаг сигнализирующий о том, что доступна новая версия каталога [ИД: @@|cat_id@@|]: "), ("eng", B.pack "An error occured when trying to drop flag, which signalizes, that there is a new catalog [ID: @@|cat_id@@|] version is available:")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_DFCNTBUE##|" inner_cfg))
                        , ("E_PCLTC_DFCNTBUE_NOMODIFY", (M.fromList [("rus", B.pack "запрос в БД на изменение таблицы содержащей флаг выполнен без ошибок, но и СУДБ отметила, что никакие данные не подвергались изменениям"), ("eng", B.pack "modifying DB query was executed without errors, however DBMS returned, that no row was actually modified")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_DFCNTBUE##|" inner_cfg))
                        , ("E_PCLTC_DFCNTBUE_DBERR", (M.fromList [("rus", B.pack "##|DB_ERROR##|"), ("eng", B.pack "##|DB_ERROR##|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_DFCNTBUE##|" inner_cfg))
                        ]

---------------------------------

instance ShowAsPCSI CatalogUpdateFromDBErrors where
        showAsPCSI cue =
                thePCSI
                        "E_PCLTC_CUE"
                        [ ("pclt_cue_err_details", PCSIList_PV err_pcsi_list Nothing_PV)
                        , ("cat_id"              , PlainText_PV $ show  $ cueCatalogID cue)
                        , ("cat_read_wasit"      , PCSI_PV $ showAsPCSI $ cueCatalogRead cue)
                        ]
              where
                err_pcsi_arfdbe   = case cueARFDBE cue of
                                        Nothing       -> []
                                        Just arfdbe   -> [thePCSI "E_PCLTC_CUE_ARFDBE" [("arfdbe", Indented_PV 4 $ PCSI_PV $ showAsPCSI arfdbe)]]
                err_pcsi_dfcntbue = case cueDropCNTBUFlag cue of
                                        Nothing       -> []
                                        Just dfcntbue -> [thePCSI "E_PCLTC_CUE_DFCNTBUE" [("dfcntbue", Indented_PV 4 $ PCSI_PV $ showAsPCSI dfcntbue)]]
                err_pcsi_rpcsce_l = case cueCatReadErrs cue of
                                        []            -> []
                                        l             -> [thePCSI "E_PCLTC_CUE_RLATCE_L" [("cre_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI l) usualSeparatorInPCSIList)]]
                err_pcsi_cole_l = case cueCollectionErrs cue of
                                        []            -> []
                                        l             -> [thePCSI "E_PCLTC_CUE_APTTPTE_L" [("cole_list", Indented_PV 4 $ PCSIList_PV (map showAsPCSI l) usualSeparatorInPCSIList)]]
                err_pcsi_list     = err_pcsi_arfdbe ++ err_pcsi_dfcntbue ++ err_pcsi_cole_l ++ err_pcsi_rpcsce_l

instance HasStaticRawPCLTs CatalogUpdateFromDBErrors where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("E_PCLTC_CUE", (M.fromList [("rus", B.pack "##|E_PCLTC_CUE_PREFIX##| @@|pclt_cue_err_details@@|\n##|E_PCLTC_CUE_POSTFIX##|"), ("eng", B.pack "##|E_PCLTC_CUE_PREFIX##|@@|pclt_cue_err_details@@|\n##|E_PCLTC_CUE_POSTFIX##|")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                        , ("E_PCLTC_CUE_PREFIX", (M.fromList [("rus", B.pack "Произошла одна или несколько ошибок при попытке обновить оперативную версию каталога [ИД: @@|cat_id@@|] (исторник обновления: БД): "), ("eng", B.pack "One or more errors occurred when trying to update operative version of catalog [ID: @@|cat_id@@|] (update source: DB):")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE##|" inner_cfg))
                        , ("E_PCLTC_CUE_ARFDBE", (M.fromList [("rus", B.pack "\n ** Ошибка чтения из БД: @@|arfdbe@@|"), ("eng", B.pack "\n ** An error, when reading from DB: @@|arfdbe@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE##|" inner_cfg))
                        , ("E_PCLTC_CUE_DFCNTBUE", (M.fromList [("rus", B.pack "\n ** Ошибка сброса флага новой версии каталога: @@|dfcntbue@@|"), ("eng", B.pack "\n ** An error, when dropping a flag of a new catalog version: @@|dfcntbue@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE##|" inner_cfg))
                        , ("E_PCLTC_CUE_RLATCE_L", (M.fromList [("rus", B.pack "\n ** Ошибки чтения и формирования каталога: @@|cre_list@@|"), ("eng", B.pack "\n ** Catalog parsing errors: @@|cre_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE##|" inner_cfg))
                        , ("E_PCLTC_CUE_APTTPTE_L", (M.fromList [("rus", B.pack "\n ** Ошибки сборки множества шаблонов, из которых необходимо сформировать каталог: @@|cole_list@@|"), ("eng", B.pack "\n ** Errors of collecting templates for catalog formation input: @@|cole_list@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE##|" inner_cfg))
                        , ("E_PCLTC_CUE_POSTFIX", (M.fromList [("rus", B.pack "Каталог считан: @@|cat_read_wasit@@|"), ("eng", B.pack "Catalog loaded: @@|cat_read_wasit@@|")], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE##|" inner_cfg))

                        -- These \/ templates ar not for content but for representation requirement referencing that determines "ARFDBE" group templates requirements
                        , ("ARFDBE_SDLR.Text.PCLT.Config.PCLT_InnerConfig", (M.fromList [("rus", B.empty), ("eng", B.empty)], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE_ARFDBE##|" inner_cfg))
                        , ("ARFDBE_SDLR.Database.PCLT.InterfaceWithDB.RawCatalogDataReadFromDBResult", (M.fromList [("rus", B.empty), ("eng", B.empty)], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE_ARFDBE##|" inner_cfg))
                        , ("ARFDBE_SDLR.Database.PCLT.InterfaceWithDB.CatalogReadFromDBResult", (M.fromList [("rus", B.empty), ("eng", B.empty)], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE_ARFDBE##|" inner_cfg))
                        , ("ARFDBE_SDLR.Database.PCLT.InterfaceWithDB.CatalogNeedsToBeUpdated_DoesIt", (M.fromList [("rus", B.empty), ("eng", B.empty)], str2PCLT_SDL Required_SDLM "##|E_PCLTC_CUE_ARFDBE##|" inner_cfg))
                        ]

data PCLTRawCatalog__Database_PCLT_InterfaceWithDB = PCLTRawCatalog__Database_PCLT_InterfaceWithDB
instance HasStaticRawPCLTs PCLTRawCatalog__Database_PCLT_InterfaceWithDB where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                                [
                                  getStaticRawPCLTs inner_cfg (undefined :: AddressedDropFlag_CatalogNeedsToBeUpdated_Error)
                                , getStaticRawPCLTs inner_cfg (undefined :: CatalogUpdateFromDBErrors)
                                , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Database_ReadableFromDB)
                                ]

