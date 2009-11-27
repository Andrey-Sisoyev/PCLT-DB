{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- File with PCLT representations.
-- Main file is HelloWorld.hs

module HelloWorld__ where

import Database.PCLT
import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Text.PCLT.SH__
import Text.PCLT.ShowAsPCSI__
import HelloWorld_Data

-----------------------------------------------------
-----------------------------------------------------
-- Representations
-- IMPORTANT!!! : It is highly recommended to use ISO 639(3) standard for language names, since PCLT-DB package that addons a DBMS powers to PCLT catalogs management is oriented on 3 letters (not bigger) language names. Without modifications PCLT-DB won't work for bigger (then 3-letters) names.

instance (ShowAsPCSI HelloWorldError, ShowAsPCSI HelloWorld) => ShowAsPCSI (Either HelloWorldError HelloWorld) where
        showAsPCSI err_or_hw =
                case err_or_hw of
                    Right hw  -> showAsPCSI hw
                    Left  hwe -> showAsPCSI hwe

instance ShowAsPCSI HelloWorld where
     showAsPCSI hw = empPCSI "HW"
instance HasStaticRawPCLTs HelloWorld where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "HW"
                          , ( M.fromList
                                  [ ("rus", B.pack "Привет, Мир!")
                                  , ("eng", B.pack "Hello world!")
                                  , ("hs_", B.pack "HelloWorld")
                                  ]
                            , str2PCLT_SDL Required_SDLM "0" inner_cfg
                            )
                          )
                        ]

instance ShowAsPCSI HelloWorldError where
     showAsPCSI hwe = thePCSI "E_HWE" [ ("hwe_details_pcsi", PCSI_PV hwe_details_pcsi) ]
        where
           hwe_details_pcsi =
                case hwe of
                    NoWorld_HWE ->
                        empPCSI "E_HWE_NOWRLD"
                    AmbiguousChoiceOfWorlds_HWE (wn1, wi1) (wn2, wi2) other_worlds ->
                        thePCSI "E_HWE_AMBWRLDCH"
                                [ ("w_name_1", PlainText_PV wn1)
                                , ("w_idx_1" , PlainText_PV $ show wi1)
                                , ("w_name_2", PlainTextLBS_PV $ B.pack wn1)
                                , ("w_idx_2" , PlainTextLBS_PV $ B.pack $ show wi2)
                                , ("other_worlds", case null other_worlds of
                                                       True  -> PCSI_PV $ empPCSI "E_HWE_AMBWRLDCH_NOMORE"
                                                       False ->
                                                            Indented_PV 3 $
                                                                  PCSIList_PV
                                                                       (map (\ (w_name, w_idx) ->
                                                                                thePCSI
                                                                                     "E_HWE_AMBWRLDCH_OW"
                                                                                     [ ("w_name", PlainText_PV w_name)
                                                                                     , ("w_idx" , PlainText_PV $ show w_idx)
                                                                                     ]
                                                                            )
                                                                            other_worlds
                                                                       )
                                                                       (PCSI_PV $ empPCSI "E_HWE_AMBWRLDCH_OW_SEP")
                                  )
                                ]
                    SomeVeryStrangeError_HWE i s b mb_b1 mb_b2 sm_excpt ->
                        thePCSI "E_HWE_STRNGERR"
                                [ ("int"     , PlainText_PV $ show i)
                                , ("str"     , PlainText_PV s)
                                , ("bool"    , PCSI_PV $ showAsPCSI b)
                                , ("mb_bool1", PCSI_PV $ showAsPCSI mb_b1)
                                , ("mb_bool2", PCSI_PV $ showAsPCSI mb_b2)
                                , ("sm_excpt", PCSI_PV $ showAsPCSI sm_excpt)
                                ]
                    FailedDueToErrorInSubsystem_HWE eis ->
                        [showAsPCSI eis] `addToPCSI` empPCSI "E_HWE_EIS"

data PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng = PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng
instance HasStaticRawPCLTs PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
             PCLT_RawCatalogData $ M.fromList
                                        [
                                          ("TRUE", (M.fromList [("hs_",  B.pack "True")], str2PCLT_SDL Required_SDLM "one" inner_cfg)) -- the minimal SDL requirement here must be the same as for the representation in catalog, where these templates are persistent in other languages and where these templates will be added
                                        , ("FALSE", (M.fromList [("hs_",  B.pack "False")], str2PCLT_SDL Required_SDLM "##|TRUE##|" inner_cfg))

                                        , ("MAYBE_A", (M.fromList [("hs_",  B.pack "@@|maybe_cnstr@@|")], str2PCLT_SDL Required_SDLM "##|TRUE##|" inner_cfg)) -- we must stick to the composites and parameters sets used in original representation
                                        , ("MAYBE_JUST", (M.fromList [("hs_",  B.pack "Just @@|a@@|")], str2PCLT_SDL Required_SDLM "##|MAYBE_A##|" inner_cfg))
                                        , ("MAYBE_NOTHING", (M.fromList [("hs_", B.pack "Nothing")], str2PCLT_SDL Required_SDLM "##|MAYBE_A##|" inner_cfg))

                                        , ("LLEXCPT", (M.fromList [("hs_", B.pack "SomeException (ErrorCall \"@@|excpt_msg@@|\")")], str2PCLT_SDL Required_SDLM "1000" inner_cfg))
                                        -- this \/ wasn't in th previous version of HelloWorld (the one, where PCLT-DB was not used, but only PCLT)
                                        , ("DB_ERROR", (M.fromList [("hs_", B.pack "SqlError { seState = \"@@|seState@@|\", seNativeError = @@|seNativeError@@|, seErrorMsg = \"@@|seErrorMsg@@|\" }")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))
                                        ]

instance HasStaticRawPCLTs HelloWorldError where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "E_HWE"
                          , ( let same_tpl = B.pack "##|E_HWE_PREFIX##|@@|hwe_details_pcsi@@|"
                               in M.fromList
                                  [ ("rus", same_tpl)
                                  , ("eng", same_tpl)
                                  , ("hs_", same_tpl)
                                  ]
                            , str2PCLT_SDL Required_SDLM "10" inner_cfg
                            )
                          )
                        , ( "E_HWE_PREFIX"
                          , ( M.fromList
                                  [ ("rus", B.pack "Приветствие мира не удалось!\nПричина: ")
                                  , ("eng", B.pack "Hello world failure!\nReason: ")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_NOWRLD"
                          , ( M.fromList
                                  [ ("rus", B.pack "некого приветствовать (нет мира)!")
                                  , ("eng", B.pack "no world!")
                                  , ("hs_", B.pack "NoWorld_HWE")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH"
                          , ( M.fromList
                                  [ ("rus", B.pack "неясно, какой из миров приветствовать - их несколько!\nПервый мир: [имя: '@@|w_name_1@@|', индекс: @@|w_idx_1@@|].\nВторой мир: [имя: '@@|w_name_2@@|', индекс: @@|w_idx_2@@|].\nА так же эти миры: \n   @@|other_worlds@@|.")
                                  , ("eng", B.pack "ambiguous choice of worlds!\nFirst world: [name: '@@|w_name_1@@|', index: @@|w_idx_1@@|].\nSecond world: [name: '@@|w_name_2@@|', index: @@|w_idx_2@@|].\nAnd also these worlds: \n   @@|other_worlds@@|.")
                                  , ("hs_", B.pack "AmbiguousChoiceOfWorlds_HWE\n   (\"@@|w_name_1@@|\", @@|w_idx_1@@|)\n   (\"@@|w_name_2@@|\", @@|w_idx_2@@|)\n   [ @@|other_worlds@@|\n   ]")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH_OW"
                          , ( M.fromList
                                  [ ("rus", B.pack "@@|__row_idx@@|) мир [имя: '@@|w_name@@|', индекс: @@|w_idx@@|]") -- __row_idx is an implicit parameter, that can be used by templates that are guaranteed to be wrapped in a PCSIList_PV (not PVList_PV !!!) parameter value wrapper
                                  , ("eng", B.pack "@@|__row_idx@@|) world [name: '@@|w_name@@|', index: @@|w_idx@@|]")
                                  , ("hs_", B.pack "(\"@@|w_name@@|\", @@|w_idx@@|)")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE_AMBWRLDCH##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH_OW_SEP"
                          , ( M.fromList
                                  [ ("rus", B.pack "\n")
                                  , ("eng", B.pack "\n")
                                  , ("hs_", B.pack "\n, ")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE_AMBWRLDCH_OW##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_AMBWRLDCH_NOMORE"
                          , ( M.fromList
                                  [ ("rus", B.pack "список пуст.")
                                  , ("eng", B.pack "empty list.")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE_AMBWRLDCH_OW##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_STRNGERR"
                          , ( M.fromList
                                  [ ("rus", B.pack "какая-то странная непонятная ошибка! Данные: @@|int@@| \"@@|str@@|\" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) { @@|sm_excpt@@| }")
                                  , ("eng", B.pack "some very strange error! Data: @@|int@@| \"@@|str@@|\" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) { @@|sm_excpt@@| }")
                                  , ("hs_", B.pack "SomeVeryStrangeError_HWE @@|int@@| \"@@|str@@|\" @@|bool@@| (@@|mb_bool1@@|) (@@|mb_bool2@@|) (@@|sm_excpt@@|)")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        , ( "E_HWE_EIS"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка в подсистеме!\nТекст исключения уровнем ниже:\n##|E_EIS##|")
                                  , ("eng", B.pack "failed due to error(s) in subsystem!\nLower level exception message:\n##|E_EIS##|")
                                  , ("hs_", B.pack "FailedDueToErrorInSubsystem_HWE (##|E_EIS##|)")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_HWE##|" inner_cfg
                            )
                          )
                        ]

instance ShowAsPCSI ErrorInSubsystem where
     showAsPCSI eis = thePCSI "E_EIS" [ ("eis_details_pcsi", PCSI_PV eis_details_pcsi) ]
        where
           eis_details_pcsi =
                case eis of
                    ErrorType1_EIS ->
                        empPCSI "E_EIS_ET1"
                    ErrorType2_EIS ->
                        empPCSI "E_EIS_ET2"
                    FailedDueToErrorInSub_sub_system_EIS eiss ->
                        thePCSI "E_EIS_EISS" [("e_eiss", PCSI_PV $ showAsPCSI eiss)]
instance HasStaticRawPCLTs ErrorInSubsystem where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "E_EIS"
                          , ( let same_tpl = B.pack "##|E_EIS_PREFIX##|@@|eis_details_pcsi@@|"
                               in M.fromList
                                  [ ("rus", same_tpl)
                                  , ("eng", same_tpl)
                                  , ("hs_", same_tpl)
                                  ]
                            , str2PCLT_SDL Required_SDLM "20" inner_cfg
                            )
                          )
                        , ( "E_EIS_PREFIX"
                          , ( M.fromList
                                  [ ("rus", B.pack "Сбой в подсистеме!\nПричина: ")
                                  , ("eng", B.pack "Subsystem failure!\nReason: ")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EIS##|" inner_cfg
                            )
                          )
                        , ( "E_EIS_ET1"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #1!")
                                  , ("eng", B.pack "error of type #1!")
                                  , ("hs_", B.pack "ErrorType1_EIS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EIS##|" inner_cfg
                            )
                          )
                        , ( "E_EIS_ET2"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #2!")
                                  , ("eng", B.pack "error of type #2!")
                                  , ("hs_", B.pack "ErrorType2_EIS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EIS##|" inner_cfg
                            )
                          )
                        , ( "E_EIS_EISS"
                          , ( M.fromList
                                  [ ("rus", B.pack "сбой в подПОДсистеме! Текст исключения уровнем ниже: @@|e_eiss@@|")
                                  , ("eng", B.pack "failed due to error(s) in subSUBsystem!\nLower level exception message:\n@@|e_eiss@@|")
                                  , ("hs_", B.pack "FailedDueToErrorInSub_sub_system_EIS (@@|e_eiss@@|)")
                                  ]
                            , str2PCLT_SDL Required_SDLM "@@|e_eiss@@|" inner_cfg
                            )
                          )
                        ]

instance ShowAsPCSI ErrorInSub_sub_system where
     showAsPCSI eiss = thePCSI "E_EISS" [ ("eiss_details_pcsi", PCSI_PV eiss_details_pcsi) ]
        where
           eiss_details_pcsi =
                case eiss of
                    ErrorType1_EISS ->
                        empPCSI "E_EISS_ET1"
                    ErrorType2_EISS ->
                        empPCSI "E_EISS_ET2"
instance HasStaticRawPCLTs ErrorInSub_sub_system where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [ ( "E_EISS"
                          , ( let same_tpl = B.pack "##|E_EISS_PREFIX##|@@|eiss_details_pcsi@@|"
                               in M.fromList
                                  [ ("rus", same_tpl)
                                  , ("eng", same_tpl)
                                  , ("hs_", same_tpl)
                                  ]
                            , str2PCLT_SDL Required_SDLM "30" inner_cfg
                            )
                          )
                        , ( "E_EISS_PREFIX"
                          , ( M.fromList
                                  [ ("rus", B.pack "Сбой в подПОДсистеме!\nПричина: ")
                                  , ("eng", B.pack "SubSUBsystem failure!\nReason: ")
                                  , ("hs_", B.empty)
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EISS##|" inner_cfg
                            )
                          )
                        , ( "E_EISS_ET1"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #1!")
                                  , ("eng", B.pack "error of type #1!")
                                  , ("hs_", B.pack "ErrorType1_EISS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EISS##|" inner_cfg
                            )
                          )
                        , ( "E_EISS_ET2"
                          , ( M.fromList
                                  [ ("rus", B.pack "ошибка типа #2!")
                                  , ("eng", B.pack "error of type #2!")
                                  , ("hs_", B.pack "ErrorType2_EISS")
                                  ]
                            , str2PCLT_SDL Required_SDLM "##|E_EISS##|" inner_cfg
                            )
                          )
                        ]

------------------------------------
-- Gathering all HasStaticRawPCLTs insances and sticking them to one data type.

data PCLTRawCatalog__HelloWorld = PCLTRawCatalog__HelloWorld
instance HasStaticRawPCLTs PCLTRawCatalog__HelloWorld where
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
     getStaticRawPCLTs inner_cfg _ =
                mergeRawCatalogDataSets2 True
                        [ getStaticRawPCLTs inner_cfg (undefined :: HelloWorld)
                        , getStaticRawPCLTs inner_cfg (undefined :: HelloWorldError)
                        , getStaticRawPCLTs inner_cfg (undefined :: ErrorInSubsystem)
                        , getStaticRawPCLTs inner_cfg (undefined :: ErrorInSub_sub_system)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Text_PCLT_ShowAsPCSI_GeneralCommons__addon_for_haskell_lng)
                        , getStaticRawPCLTs inner_cfg (undefined :: PCLTRawCatalog__Database_PCLT_UpdatableCatalog) -- this collection unites all templates used by PCLT and PCLT-DB
                        ]
