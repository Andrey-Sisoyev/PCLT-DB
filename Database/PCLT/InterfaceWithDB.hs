{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable #-}

-- | Some instances of 'Database.ReadableFromDB.ReadableFromDB' for reading
-- PCLT catalog and related objects from DB.
--
-- DBMS is PostgreSQL v8.4. Not earlier, since PCLT-DB relies on a feature that
-- appeared in PostgresSQL only in v8.4. - the @WITH@ clause for @SELECT@ query
-- (also @WITH RECURSIVE@).
module Database.PCLT.InterfaceWithDB where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe (isNothing)
import Data.MyHelpers
import Data.Either
import Data.Typeable
import Database.HDBC
import Database.HDBC.PostgreSQL
import Database.ReadableFromDB
import Text.PCLT
import Text.PCLT.Config -- for Haddock
import Text.PCLT.MakeCatalog
import Text.PCLT.CatalogFromHSRT
import System.IO
import System.IO.Unsafe

-- * Instances of ReadableFromDB

instance ReadableFromDB PCLT_InnerConfig PCLT_CatalogID where
        readFromDB db_conn cat_id = handleSql
                  (\ exc -> return $ wrapParseResult_Nrows cat_id $ liftInList $ Left $ RecieveError_RFDBE (exc :: SqlError))
                  (do stmt     <- prepare db_conn "SELECT * FROM sch_pcltcatalogs.by_catalog_used_config(?)"
                      i        <- execute stmt [toSql cat_id]
                      raw_rows <- fetchAllRowsMap' stmt
                      mapM (parseDBrow db_conn cat_id) raw_rows
                  )
        parseDBrow db_conn cat_id _row_map = liftM (wrapParseResult_1row cat_id) $ do
                        let so_f_name = "StrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets"
                        let loc_takeFieldValue f = takeUFieldValue f (uppercaseMapKeys _row_map)
                        excpt_or_so <- try (evaluate $ read $ loc_takeFieldValue so_f_name)
                        return $ case excpt_or_so of
                            Left   e -> Left $ RowParseError_RFDBE $ SomeException $ ErrorCall ("Field '" ++ so_f_name ++ "' of Text.PCLT.Config.PCLT_InnerConfig row parse failed! Lower level exception: " ++ show (e :: SomeException))
                            Right so -> Right $
                                    defaultPCLTInnerConfig {
                                                pcsInnerConfigID                  = loc_takeFieldValue "tplscat_inner_config_id"
                                              , pcsCompositePlaceholderWrapper    = loc_takeFieldValue "CompositePlaceholderWrapper"
                                              , pcsParameterPlaceholderWrapper    = loc_takeFieldValue "ParameterPlaceholderWrapper"
                                              , pcsInsuficientDetLevelPlaceholder = loc_takeFieldValue "InsuficientDetLevelPlaceholder"
                                              , pcsMarkingErrorPlaceholderWrapper = loc_takeFieldValue "MarkingErrorPlaceholderWrapper"
                                              , pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets
                                                                                  = so
                                              , pcsAllowUntemplatedMessages       = loc_takeFieldValue "AllowUntemplatedMessages"
                                              , pcsAllowUntemplatedLocalizationsOfMessages
                                                                                  = loc_takeFieldValue "AllowUntemplatedLocalizationsOfMessages"
                                              , pcsShowAdhocParamsInResultOfUntemplated
                                                                                  = loc_takeFieldValue "ShowAdhocParamsInResultOfUntemplated"
                                              , pcsDefaultLanguage                = loc_takeFieldValue "DefaultLanguage"
                                              , pcsReparsingDepthMax              = loc_takeFieldValue "ReparsingDepthMax"
                                              , pcsReparseParameterContentMaxSize = loc_takeFieldValue "ReparseParameterContentMaxSize"
                                              , pcsInstaniationResultMaxSize      = loc_takeFieldValue "InstaniationResultMaxSize"
                                              , pcsAllowEmptySDL_parseItByModusMargin
                                                                                  = loc_takeFieldValue "AllowEmptySDL_parseItByModusMargin"
                                              , pcsAllowUnreadableSDL_parseIdByModusMargin
                                                                                  = loc_takeFieldValue "AllowUnreadableSDL_parseIdByModusMargin"
                                              , pcsNewlineLBS                     = loc_takeFieldValue "Newline"
                                    }

-- | Container for data, that is read from DB, that goes on input to
-- catalog formation routine.
newtype RawCatalogDataReadFromDBResult = RawCatalogDataReadFromDBResult (PCLT_RawCatalogData, [AddPCLT_toPCLT_Error]) deriving (Show, Typeable)
instance ReadableFromDB RawCatalogDataReadFromDBResult (PCLT_CatalogID, PCLT_InnerConfig) where
        readFromDB db_conn (cat_id, cfg) = handleSql
                      (\ exc -> return $ wrapParseResult_Nrows cat_id $ liftInList $ Left $ RecieveError_RFDBE (exc :: SqlError)) $
                      do let query = "SELECT pclt_id,tpl_req_sdl,lng,structured_text FROM sch_pcltcatalogs.in_catalog_localized_tpls_with_their_sdls(?) ORDER BY pclt_id"
                         stmt     <- prepare db_conn query -- ORDER BY pclt_id is obligate
                         i        <- execute stmt [toSql cat_id]
                         raw_rows <- fetchAllRowsMap stmt
                         let f remaining_rows (tpls_accum, errs_accum) =
                                case remaining_rows of
                                    []    -> (tpls_accum, errs_accum)
                                    (h:t) ->
                                        let tpl_id  = takeFieldValue "pclt_id" h
                                            req_sdl = str2PCLT_SDL Required_SDLM (takeFieldValue "tpl_req_sdl" h) cfg
                                            (rows_by_tpl_id, new_remaining_rows) = span (\ _row -> takeFieldValue "pclt_id" _row == tpl_id) t
                                            (of_wrong_sdls, raw_localizations) = partitionEithers $ map --traceCond (tpl_id == "HW" || tpl_id == "E_PCLT_P2TE") 5 "***" $ watchCond (tpl_id == "HW" || tpl_id == "E_PCLT_P2TE") $
                                                (\ _row -> let sdl2_raw = takeFieldValue "tpl_req_sdl" _row
                                                               sdl2 = str2PCLT_SDL Required_SDLM sdl2_raw cfg
                                                            in case req_sdl == sdl2 of
                                                                   True  -> Right ( takeFieldValue "lng" _row, takeFieldValue "structured_text" _row )
                                                                   False -> Left sdl2
                                                )
                                                (h : rows_by_tpl_id)
                                         in f new_remaining_rows
                                              ( M.insert tpl_id (M.fromList raw_localizations, req_sdl) tpls_accum
                                              , errs_accum ++ map (\ sdl2 -> DifferentSDLs_APTTPTE $ DifferentSDLs_PCLTE tpl_id (req_sdl, sdl2) ) of_wrong_sdls
                                              )
                         let (raw_data_map, errs) = f raw_rows (M.empty, [])
                         return $ liftInList $ wrapParseResult_1row (cat_id, cfg) $ Right $ RawCatalogDataReadFromDBResult (PCLT_RawCatalogData raw_data_map, errs)

        parseDBrow _ _ _ = undefined -- doesn't fit for complex cases...

-- | Container for PCLT catalog, that is read from DB.
newtype CatalogReadFromDBResult = CatalogReadFromDBResult (PCLT_Catalog, [ErrorWithPCSCatalog ReadPCSCatalogError], [AddPCLT_toPCLT_Error]) deriving (Show, Typeable)
instance ReadableFromDB CatalogReadFromDBResult PCLT_CatalogID where
        readFromDB db_conn cat_id = do
                err_or_inner_cfg <- readOneFromDB db_conn cat_id True
                case err_or_inner_cfg of
                    Left err -> return [wrapParseResult_1row cat_id $ Left $ SubReadError_RFDBE err]
                    Right inner_cfg -> do
                        err_or_raw_cat_data <- readOneFromDB db_conn (cat_id, inner_cfg) True
                        return $ liftInList $ wrapParseResult_1row cat_id $
                             case err_or_raw_cat_data of
                                 Left           err -> Left $ SubReadError_RFDBE err
                                 Right (RawCatalogDataReadFromDBResult (raw_cat_data, collection_errors)) -> Right $
                                      let (cat, parse_errs) = readPCLTCatalog inner_cfg cat_id raw_cat_data
                                       in CatalogReadFromDBResult (cat, parse_errs, collection_errors)
        parseDBrow _ _ _ = undefined -- doesn't fit for complex cases...

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

-- | Container for a flag, that is read from DB.
--
-- For each catalog in DB there is stored a special flag \"catalog data
-- or config changed\". The flag gets raised by an army of triggers,
-- that watches every table, and is thought to get dropped by a program,
-- which uses DB catalog. Drop the flag right before catalog update
-- from DB.
--
-- Unfortunately, the current implementation of this flagging system
-- constraints the number of agents that update their catalogs from DB
-- by only one (because, when agent updates it's catalog, it drops
-- the flag, and other agents don't know that flag was risen, so they
-- can't know if version of catalog in their operative memory is older
-- then in DB or is it not).
--
-- But this limitation removal is planned as a first ToDo in the next
-- version of PCLT-DB.
newtype CatalogNeedsToBeUpdated_DoesIt = CatalogNeedsToBeUpdated_DoesIt Bool deriving (Typeable,Show)
instance ReadableFromDB CatalogNeedsToBeUpdated_DoesIt PCLT_CatalogID where
        readFromDB db_conn cat_id = handleSql
                  (\ exc -> return $ wrapParseResult_Nrows cat_id $ liftInList $ Left $ RecieveError_RFDBE (exc :: SqlError))
                  (do stmt     <- prepare db_conn "SELECT cat_new_version_available FROM sch_pcltcatalogs.tpls_catalogs WHERE tpls_catalog_id = ?"
                      i        <- execute stmt [toSql cat_id]
                      raw_rows <- fetchAllRowsMap' stmt
                      mapM (parseDBrow db_conn cat_id) raw_rows
                  )
        parseDBrow db_conn cat_id row_map =
                  return $ wrapParseResult_1row cat_id $ Right $ CatalogNeedsToBeUpdated_DoesIt $ (takeFieldValue "cat_new_version_available" row_map)

-- * Drop flag \"catalog data or config changed\" in DB

-- | Errors that may occur, when performing 'dropFlag_CatalogNeedsToBeUpdated'.
data DropFlag_CatalogNeedsToBeUpdated_Error =
          NotModified_DFCNTBUE
        | DBError_DFCNTBUE SqlError
       deriving (Show, Typeable)
-- | Wrapper around 'DropFlag_CatalogNeedsToBeUpdated_Error',
-- added 'PCLT_CatalogID', for which \"drop flag\" operation failed.
data AddressedDropFlag_CatalogNeedsToBeUpdated_Error = AddressedDropFlag_CatalogNeedsToBeUpdated_Error DropFlag_CatalogNeedsToBeUpdated_Error PCLT_CatalogID deriving (Show, Typeable)

-- | Drop that flag in DB.
dropFlag_CatalogNeedsToBeUpdated :: Connection -> PCLT_CatalogID -> IO (Maybe AddressedDropFlag_CatalogNeedsToBeUpdated_Error)
dropFlag_CatalogNeedsToBeUpdated db_conn cat_id =
        handleSql
                (\ exc -> return $ Just $ AddressedDropFlag_CatalogNeedsToBeUpdated_Error (DBError_DFCNTBUE (exc :: SqlError)) cat_id )
                (do r <- run db_conn "UPDATE sch_pcltcatalogs.tpls_catalogs SET cat_new_version_available = FALSE WHERE tpls_catalog_id = ?" [toSql cat_id]
                    commit db_conn
                    return $ case r > 0 of
                                 True  -> Nothing
                                 False -> Just $ AddressedDropFlag_CatalogNeedsToBeUpdated_Error NotModified_DFCNTBUE cat_id
                )

-- * Consider catalog update

-- | The container for all types of errors that may result, when trying
-- to update a catalog from DB.
data CatalogUpdateFromDBErrors =
        CatalogUpdateFromDBErrors {
                  cueDropCNTBUFlag  :: Maybe AddressedDropFlag_CatalogNeedsToBeUpdated_Error
                , cueARFDBE         :: Maybe AddressedReadFromDBError
                , cueCatReadErrs    :: [ErrorWithPCSCatalog ReadPCSCatalogError]
                , cueCollectionErrs :: [AddPCLT_toPCLT_Error]
                , cueCatalogID      :: PCLT_CatalogID
                , cueCatalogRead    :: Bool
        }
       deriving (Show, Typeable)

-- | Checks if catalog is read without a single error.
emptyCUE :: CatalogUpdateFromDBErrors -> Bool
emptyCUE cue = foldr (\ p_f accum -> accum && p_f cue) True [isNothing . cueDropCNTBUFlag, isNothing . cueARFDBE, null . cueCatReadErrs, null . cueCollectionErrs, cueCatalogRead]

-- | An empty form to be filled.
defaultCatalogUpdateFromDBErrors :: PCLT_CatalogID -> CatalogUpdateFromDBErrors
defaultCatalogUpdateFromDBErrors cat_id = CatalogUpdateFromDBErrors {
                  cueDropCNTBUFlag  = Nothing
                , cueARFDBE         = Nothing
                , cueCatReadErrs    = []
                , cueCollectionErrs = []
                , cueCatalogID      = cat_id
                , cueCatalogRead    = False
                }

-- | There is a one way relation from
-- 'Text.PCLT.CatalogFromHSRT.CatalogFromHSRTInitErrors'
-- to 'CatalogUpdateFromDBErrors'. That's because subject operations are
-- similar it their abstractions.
cfhie2cue :: CatalogFromHSRTInitErrors -> CatalogUpdateFromDBErrors
cfhie2cue cfhie = (defaultCatalogUpdateFromDBErrors $ cfhieCatalogID cfhie) {
                        cueCatReadErrs    = cfhieCatReadErrs    cfhie
                      , cueCollectionErrs = cfhieCollectionErrs cfhie
                      , cueCatalogRead    = True
                  }

___always_update = False

-- | Check if the flag \"catalog data or config changed\" is up in DB.
-- If so, drop it and try to read from DB
--
-- (1) 'Text.PCLT.Config.PCLT_InnerConfig'
--
-- (2) 'RawCatalogDataReadFromDBResult'
--
-- (3) 'CatalogReadFromDBResult'
considerCatalogUpdate :: Connection -> PCLT_CatalogID -> IO (Maybe PCLT_Catalog, Maybe CatalogUpdateFromDBErrors)
considerCatalogUpdate db_conn cat_id = do
        err_or_newversion_thereis <- readOneFromDB db_conn cat_id True
        case err_or_newversion_thereis of
            Left arfdbe -> return (Nothing, Just $ ((defaultCatalogUpdateFromDBErrors cat_id) { cueARFDBE = Just arfdbe }))
            Right (CatalogNeedsToBeUpdated_DoesIt newversion_thereis) ->
                case newversion_thereis  || ___always_update of
                    False -> return (Nothing, Nothing)
                    True  -> do
                        mb_err <- dropFlag_CatalogNeedsToBeUpdated db_conn cat_id
                        let mb_cue = case mb_err of
                                Just dropflag_err -> Just $ ((defaultCatalogUpdateFromDBErrors cat_id) { cueDropCNTBUFlag = mb_err })
                                Nothing           -> Nothing
                        readCat mb_cue
     where
        readCat mb_cue = do
                err_or_cat <- readOneFromDB db_conn cat_id True
                return $ case err_or_cat of
                    Left arfdbe ->
                        ( Nothing
                        , liftM (\ cue -> cue { cueARFDBE = Just arfdbe } ) $ Just $ maybe (defaultCatalogUpdateFromDBErrors cat_id) id mb_cue
                        )
                    Right (CatalogReadFromDBResult (cat, cat_read_errs_list, collection_errs)) ->
                        ( Just cat
                        , case null cat_read_errs_list && null collection_errs of
                              True  -> (\ cue -> cue { cueCatalogRead = True } ) `liftM` mb_cue
                              False -> liftM (\ cue -> cue { cueCatalogRead    = True
                                                           , cueCatReadErrs    = cat_read_errs_list
                                                           , cueCollectionErrs = collection_errs
                                                           }
                                             )
                                             (Just $ maybe (defaultCatalogUpdateFromDBErrors cat_id) id mb_cue)
                        )

