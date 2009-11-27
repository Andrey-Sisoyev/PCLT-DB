{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | A module around a thread that considers updating catalog, when commanded
-- to do so.
module Database.PCLT.UpdatableCatalog where

import Control.Concurrent
import Control.Exception
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Maybe
import Data.MyHelpers
import Data.Either
import Database.HDBC
import Database.HDBC.PostgreSQL
import Database.PCLT.InterfaceWithDB
import Text.PCLT
import Text.PCLT.Catalog
import Text.PCLT.Config

import Database.ReadableFromDB__
import Database.PCLT.InterfaceWithDB__
import Text.PCLT.HasStaticRawPCLTs
import Text.PCLT.InitialDefaultCatalog (PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog)

-- | This type is a special instance of 'HasStaticRawPCLTs' - it accumulates
-- all other instances of 'HasStaticRawPCLTs' from the whole PCLT-DB package,
-- and also from PCLT package.
--
-- This instance is best to be used as an intial input for the catalog
-- formation in your applications.
data PCLTRawCatalog__Database_PCLT_UpdatableCatalog = PCLTRawCatalog__Database_PCLT_UpdatableCatalog
instance HasStaticRawPCLTs PCLTRawCatalog__Database_PCLT_UpdatableCatalog where
     widenessOfStaticRawPCLTsSet _ = Package_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Database_PCLT_InterfaceWithDB)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Database_ReadableFromDB)
                        ]

-- | Input contains current catalog version. If the update failed
-- and returned no new catalog version, or if update isn't needed,
-- then current catalog version is returned.
--
-- If there appeared any errors, they get acted with. The action is
-- specified by user.
catalogUpdatorIteration :: Connection -> PCLT_CatalogID -> (PCLT_Catalog -> CatalogUpdateFromDBErrors -> IO ()) -> PCLT_Catalog -> IO (Maybe PCLT_Catalog)
catalogUpdatorIteration db_conn cat_id errorsReporter old_cat = do
        (mb_new_catalog, mb_cu_errors) <- considerCatalogUpdate db_conn cat_id
        case mb_cu_errors of
            Just cue ->
                case emptyCUE cue of -- double work here with "maybe empty ..."; well... lazy to reduce it
                    False -> errorsReporter (maybe old_cat id mb_new_catalog) cue
                    True  -> return ()
            Nothing -> return ()
        return mb_new_catalog

-- | Type alias used by 'runCatalogUpdatorService'.
type DisconnectDBConnWhenFinished_shouldwe = Bool
-- | Type alias used by 'runCatalogUpdatorService'.
type MakeIterationAndContinueCycle_shouldwe = Bool

-- | The first version of catalog, that service forms is always
-- from nonDB source, but is from specified in the first triple
-- /(catalog ID, config and some instance of HasStaticRawPCLTs)/.
-- That way, even if DB never responds, service always
-- has at least this version of catalog available.
--
-- In the 3rd argument user specifies action, that has a role of errors
-- processor (reporter).
--
-- The 4th argument is a @Chan@, which this service listens to.
--
-- * If @True@ comes, service makes iteration (reads catalog from DB if needed,
-- and writes new version into the MVar), and continues cycle.
--
-- * If @False@ comes, service stops.
--
-- The returned MVar is created by service itself and is a container,
-- where it keeps last version of catalog.
runCatalogUpdatorService
        :: HasStaticRawPCLTs a
        => (PCLT_CatalogID, PCLT_InnerConfig, a)
        -> (Connection, DisconnectDBConnWhenFinished_shouldwe)
        -> (PCLT_Catalog -> CatalogUpdateFromDBErrors -> IO ())
        -> Chan MakeIterationAndContinueCycle_shouldwe
        -> IO (MVar PCLT_Catalog, ThreadId)
runCatalogUpdatorService
        (cat_id, inner_config, instance_of_hasStaticRawPCLTs)
        (db_conn, disconnectDBConWhenFinished_shouldwe)
        errorsReporter
        aliveChan = do
                cat_mv <- let (cat, cfhie) = initCatalogFromHSRT instance_of_hasStaticRawPCLTs inner_config cat_id
                              cue = cfhie2cue cfhie
                           in newMVar cat << case emptyCUE cue of
                                                 False -> errorsReporter cat cue
                                                 True  -> return ()
                updator_TID <- forkIO $ do
                        let dbFinalizer =
                                case disconnectDBConWhenFinished_shouldwe of
                                    True  -> disconnect db_conn
                                    False -> return ()
                        let serviceCycle :: IO ()
                            serviceCycle = do
                                continue_shouldwe <- readChan aliveChan
                                case continue_shouldwe of
                                    False -> return () -- service finish
                                    True  -> do
                                        old_cat <- readMVar cat_mv
                                        mb_new_cat <- catalogUpdatorIteration db_conn cat_id errorsReporter old_cat
                                        case mb_new_cat of
                                            Just new_catalog -> modifyMVar_ cat_mv (\ _ -> return new_catalog)
                                            Nothing -> return ()
                                        serviceCycle
                        finally serviceCycle dbFinalizer
                return (cat_mv, updator_TID)

-- | Wrapper around 'runCatalogUpdatorService'. The service starts from
-- default config ('Text.PCLT.Config.defaultPCLTInnerConfig') and
-- default set of templates ('PCLTRawCatalog__Database_PCLT_UpdatableCatalog').
-- But updates from DB orienting on given 'Text.PCLT.Catalog.PCLT_CatalogID',
-- which may be different set of templates and different config.
runCatalogUpdatorService_wDefaultInitial
        :: PCLT_CatalogID
        -> (Connection, DisconnectDBConnWhenFinished_shouldwe)
        -> (PCLT_Catalog -> CatalogUpdateFromDBErrors -> IO ())
        -> Chan MakeIterationAndContinueCycle_shouldwe
        -> IO (MVar PCLT_Catalog, ThreadId)
runCatalogUpdatorService_wDefaultInitial cat_id (db_conn, disconnectDBConWhenFinished_shouldwe) errorsReporter aliveChan =
        runCatalogUpdatorService
                ( cat_id
                , defaultPCLTInnerConfig
                , PCLTRawCatalog__Database_PCLT_UpdatableCatalog
                )
                (db_conn, disconnectDBConWhenFinished_shouldwe)
                errorsReporter
                aliveChan
