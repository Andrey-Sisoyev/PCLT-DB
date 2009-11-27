{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

-- 1)  HelloWorld program that doesn't use       PCLT     and PCLT-DB  packages is to be found in PCLT packages examples directory.
-- 2)  HelloWorld program that         uses only PCLT (but no PCLT-DB) package  is to be found in PCLT packages examples directory. All the explanations about using PCLT are also there, but here are omited.
-- 3)< HelloWorld program that         uses both PCLT     and PCLT-DB  packages is the one you are currently looking at.
-- It's best to understand (1) and (2) before trying to understand what's going on in this listing here.
-- Before running this exaple user must run HelloWorld.sql

module HelloWorld where

-----------------------------------------------------
-- Modules necessary for our PCLTCatalog

import Control.Concurrent
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Typeable


import qualified Text.ConstraintedLBS as CB
import Database.PCLT -- this module exports most required PCLT-DB modules
import Text.PCLT

import Prelude   hiding (putStrLn, readLn)
import System.IO hiding (putStrLn, readLn, hPutStr)
import System.IO.UTF8 -- putStrLn from here will be used to correctly output russian letters

-----------------------------------------------------
-----------------------------------------------------
-- Application specific modules
import Control.Exception
import HelloWorld_Data -- Application specific ADTs and type synonyms
import HelloWorld__    -- instances for ShowAsPCSI and HasStaticRawPCLTs

-----------------------------------------------------
-----------------------------------------------------
-- Functional part of app

type SayHelloWorld_Mode = Int
sayHelloWorld :: SayHelloWorld_Mode -> Either HelloWorldError HelloWorld
sayHelloWorld mode =
        case mode of
            0 -> Right HelloWorld
            1 -> Left NoWorld_HWE
            2 -> Left $ AmbiguousChoiceOfWorlds_HWE ("RealWorld", 1) ("VirtualWorld", 2) [("OtherWorld1", 3), ("OtherWorld2", 4), ("OtherWorld3", 5)]
            3 -> Left $ SomeVeryStrangeError_HWE 5789 "Noise..." True (Just True) Nothing (SomeException DivideByZero)
            4 -> Left $ FailedDueToErrorInSubsystem_HWE ErrorType1_EIS
            5 -> Left $ FailedDueToErrorInSubsystem_HWE ErrorType2_EIS
            6 -> Left $ FailedDueToErrorInSubsystem_HWE $ FailedDueToErrorInSub_sub_system_EIS ErrorType1_EISS
            7 -> Left $ FailedDueToErrorInSubsystem_HWE $ FailedDueToErrorInSub_sub_system_EIS ErrorType2_EISS

-------------------------------------

--some constants required by catalogizing system
__db_name = "pcltcatalogs"
__dbms_user = "user_pcltcatalogs_data_reader"
__dbms_user_password = "data_reader_password"
__connection_string_delimiter = " "
__connectionString   =                        "dbname="   ++ __db_name            ++ __connection_string_delimiter
__connectionString'  = __connectionString  ++ "user="     ++ __dbms_user          ++ __connection_string_delimiter
__connectionString'' = __connectionString' ++ "password=" ++ __dbms_user_password ++ __connection_string_delimiter

__update_frequency_sec = 8
__my_lng = "rus"     -- possible values: "eng", "rus", "hs_"
__my_sdl = InfinitelyBig_SDL    -- possible values: Zero_SDL < One_SDL < SDL Int < InfinitelyBig_SDL
__my_stderr_clbs_size = 50000
__my_stdout_clbs_size = 50000
__catalog_id = 50

type Microseconds = Int

data CatalogControl = -- MVar of this will be in between HelloWorld mainmodule and thread that will regularly update catalog from DB
        CatalogControl {
                catcCatalogMV              :: MVar PCLT_Catalog
              , catcUpdateFrequency        :: MVar Microseconds
              , catcUpdatorLoopHolder      :: MVar Bool -- initialized as True; whenever we putthis to False, the next iteration on of update cycle begins with service termination
              , catcUpdatorTaskTriggerChan :: Chan Bool -- between iterations service awaits on this channel; we regularly send thereit gets catcUpdatorLoopHolder value; if False is sent, updator finishes
              , catcUpdatorTaskTriggerThreadID :: ThreadId
              , catcUpdatorThreadID            :: ThreadId
        } deriving (Typeable)

acquireCatalog :: StdErr_CLBS -> (ShowDetalizationLevel, LanguageName) -> IO (CatalogControl, MVar StdErr_CLBS)
acquireCatalog stderr_clbs (stderr_sdl, stderr_lng) = do
               let catalog_config =
                            defaultPCLTInnerConfig {
                                  pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets =
                                        (pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets defaultPCLTInnerConfig) {
                                                soExcludingCompParameters = soExcludingCompParameters (pcsStrictOrient_ofParamsAndCmpsts_onDfltLngTplsSets defaultPCLTInnerConfig)
                                                                         ++ [("E_HWE_AMBWRLDCH_OW", "__row_idx")] -- this is described in a HelloWorld that uses only PCLT (but no PCLT-DB)
                                        }
                            }
               updateFrequency_mv   <- newMVar (__update_frequency_sec * 1000000)
               updatorLoopHolder_mv <- newMVar True
               let taskEnablerCycle :: Chan Bool -> IO ()
                   taskEnablerCycle ch = do
                        freq <- readMVar updateFrequency_mv
                        threadDelay freq
                        enabled_isit <- readMVar updatorLoopHolder_mv
                        putStrLn "#catalog updated...#"
                        writeChan ch enabled_isit
                        case enabled_isit of
                            True  -> taskEnablerCycle ch
                            False -> return ()
               let taskEnablerThread :: IO (Chan Bool, ThreadId) -- this thread will regularly stimulate iterations of catalog updator
                   taskEnablerThread = do
                        ch <- newChan
                        tm_TID <- forkIO $ taskEnablerCycle ch
                        return (ch, tm_TID)
               (aliveChan, tm_TID) <- taskEnablerThread -- thread started
               stderr_clbs_mv      <- newMVar stderr_clbs
               let errorsReporter :: PCLT_Catalog -> CatalogUpdateFromDBErrors -> IO () -- ths is how the catalog updator service must react on catalog read failure (due to error); if catalog isn't read, the previous version of catalog is used here
                   errorsReporter cat cue = modifyMVar_
                                stderr_clbs_mv
                                (\ _errs_clbs -> return $ pcsi2text_plus_errs_1 _errs_clbs (showAsPCSI cue) (__my_sdl, __my_lng) cat )
                   serviceStart :: IO CatalogControl
                   serviceStart = do
                        db_conn <- connectPostgreSQL __connectionString''
                        serv_db_conn <- clone db_conn -- good practice for cases, when have multiple same connections - make a prototype and clone it as many times as needed
                        (cat_mv, us_TID) <- runCatalogUpdatorService
                                                (__catalog_id, catalog_config, PCLTRawCatalog__HelloWorld) -- With this catalog updator will start it's service. This way we ensure, that even if DB connection fails, we still have a catalog available.
                                                (serv_db_conn, True) -- True says, that server must disconnect assigned to it DB connection, when it finishes
                                                errorsReporter
                                                aliveChan
                        disconnect db_conn -- prototype isn't needed anymore
                        return CatalogControl {
                                catcCatalogMV              = cat_mv
                              , catcUpdateFrequency        = updateFrequency_mv
                              , catcUpdatorLoopHolder      = updatorLoopHolder_mv
                              , catcUpdatorTaskTriggerChan = aliveChan
                              , catcUpdatorTaskTriggerThreadID = tm_TID
                              , catcUpdatorThreadID            = us_TID
                        }
               cc <- serviceStart
               return (cc, stderr_clbs_mv)

showHelloWorld :: SayHelloWorld_Mode -> (StdOut_CLBS, StdErr_CLBS) -> (ShowDetalizationLevel, LanguageName, PCLT_Catalog) -> (StdOut_CLBS, StdErr_CLBS)
showHelloWorld mode (stdout_clbs, stderr_clbs) (sdl, lng, catalog) = -- showHelloWorld here doesn't differ the HelloWorld, that uses only PCLT (but no PCLT-DB) - it is described there
        let err_or_HelloWorld = sayHelloWorld mode
            (new_stdout_clbs, new_stderr_clbs) =
                pcsi2text_plus_errs_2
                        (stdout_clbs, stderr_clbs)
                        (showAsPCSI err_or_HelloWorld)
                        (sdl, lng)
                        catalog
         in (new_stdout_clbs, new_stderr_clbs)

main = run_test __my_lng __my_sdl

run_test _lng _sdl =
       let stderr_clbs0 = newCLBS __my_stderr_clbs_size
           stdout_clbs0 = newCLBS __my_stdout_clbs_size
        in do (catalog_control, stderr_clbs_mv) <- acquireCatalog stderr_clbs0 (__my_sdl, __my_lng)
              putStrLn ("Language, SDL (detailization level), update frequency (sec): " ++ show (_lng, _sdl, __update_frequency_sec))
              putStrLn "----Init-errors:-----------------"
              stderr_clbs1 <- modifyMVar stderr_clbs_mv (\ stderr_clbs1 -> return (stderr_clbs0, stderr_clbs1))
              putStrLn $ show stderr_clbs1
              dump stderr_clbs1
              putStrLn "----Cycle-start:-----------------"
              let iterate_ = do
                  putStrLn "----New-iteration:---------------"
                  putStrLn "Input sayHelloWorld mode (0-7; '-1' to exit): "
                  mode <- readLn
                  case mode >= 0 && mode <= 7 of
                      True -> do (stdout_clbs1, stderr_clbs1) <- -- stderr_clbs1 accumulates all errors from work of catalog udator servece, and independently also accumulates errors from interaction with user
                                           withMVar
                                                (catcCatalogMV catalog_control)
                                                (\ catalog ->
                                                      modifyMVar
                                                          stderr_clbs_mv
                                                          (\ _stderr_clbs ->
                                                                return ( stderr_clbs0 -- in each iteration of interaction with user, stderr gets shown and emptyed
                                                                       , showHelloWorld
                                                                                mode
                                                                                (stdout_clbs0, _stderr_clbs)
                                                                                (__my_sdl, __my_lng, catalog)
                                                                       )

                                                          )
                                                )
                                 putStrLn "----Errors:----------------------"
                                 putStrLn_paged 12 $ show stderr_clbs1
                                 putStrLn "----Output:----------------------"
                                 putStrLn $ show stdout_clbs1
                                 iterate_
                      False -> case mode == (-1) of
                                   True  -> modifyMVar_ (catcUpdatorLoopHolder catalog_control) (\ _ -> return False) -- tell updator services to finish
                                   False -> iterate_
              iterate_


putStrLn_paged :: Int -> String -> IO ()
putStrLn_paged page_size s = f $ lines s
        where
          f lines_list =
                let (to_print, to_next_itera) = splitAt page_size lines_list
                 in do putStrLn (concat $ intersperse "\n" to_print)
                       case null to_next_itera of
                           True  -> return ()
                           False -> f to_next_itera << hGetChar stdin << putStrLn "\n-------Press any key to continue...-------"

dump :: Show a => a -> IO ()
dump a = do
        h <- openFile "./dump.out" WriteMode -- WriteMode -- AppendMode
        hPutStr h $ show a
        hClose h

infixr 1 <<
(<<)   :: Monad m => m b -> m a -> m b
f << x = x >> f

-----------------------------------------------------
-----------------------------------------------------
-- Representations

-- moved to file HelloWorld__.hs

{-
-------------------------------------------------------------------------
-- CONCLUSION

It is a lot of work to play multilinguality, using PCLT-DB. The ShowAsPCSI instaniations, HasStaticRawPCLTs instaniations, templates management... however PCLT-DB was built with an aim, that once catalog is setup, tested and put to work, it's management cost is minimal possible. When using PCLT only (no DB) the following management points are there:
a) Choose catalog configuration in haskell code.
b) Adjust (together with (c)) templates in an electronic table (f.e. using OpenOffice Calc).
c) Adjust (together with (b)) templates "calls" in ShowAsPCSI instances.
d) From (b) fill in HasStaticRawPCLTs instances
e) Use HasStaticRawPCLTs instances to unite into module-wide and package-wide raw template sets.

When using PCLT-DB the following management points there are:
f) Choose catalog configuration and store it in DB.
g) Adjust (together with (c)) templates in DB.
j) Use collections in DB in order to unite module-wide and package-wide template sets.
i) If needed, manage configure and representation policies in DB - a way to set different schemes of detalization requirements "with one key press".

Possible economies:
(f) makes unnecessary (a)
(g) makes unnecessary (b)
(j) makes unnecessary (d) and (e)
(f,g,j) is considered to require less code and management cost than (a,b,d,e)

One might get confused: why then Database.PCLT.runCatalogUpdatorService input still requires a some PCLT_InnerConfig and an instance of HasStaticRawPCLTs, if all this is now kept in DB?
The answer:
0) If DB for some reasons is unawailable, catalog will be formed from the data hardcoded in HasStaticRawPCLTs instances, and using some hardcoded config.
1) Despite (a,b,d,e) becomes unnecessary, one may still choose to use them. (a,b,d,e) and (f,g,j) may work together - if DB is off, hardcoded version (a,b,d,e) of catalog is always available! Double cost for double reliability, why not, if resources allow.
2) But even if programmer doesn't want to manage (a,b,d,e), it is asummed, that at least templates used by PCLT/-DB itselt are nice to have operatively available. In this case on input to
        Database.PCLT.runCatalogUpdatorService programmer puts
                Database.PCLT.PCLTRawCatalog__Database_PCLT_UpdatableCatalog
                (or Text.PCLT.PCLTRawCatalog__Text_PCLT_InitialDefaultCatalog)
                and Text.PCLT.Config.defaultPCLTInnerConfig.

Best regards, Belka
-}