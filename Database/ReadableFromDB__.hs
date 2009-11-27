{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- | For representation of 'AddressedReadFromDBError' a special trick is used.
-- SDL requirements for representation of error is referenced to be
-- the same as specified for template with such ID:
--
-- @\"ARFDBE_SDLR.\" ++ (show \$ 'arfdbeEntityType' err)@
--
-- So, whenever an instance of 'Database.ReadableFromDB.ReadableFromDB'
-- is declared, in order to make 'AddressedReadFromDBError' show correctly,
-- developer must also add an empty template with corresponding ID to PCLT
-- catalog.
--
-- For example, template with ID
-- @\"ARFDBE_SDLR.Text.PCLT.Config.PCLT_InnerConfig\"@ is to be added, when
-- @PCLT_InnerConfig@ is made an instance of
-- 'Database.ReadableFromDB.ReadableFromDB'.
-- The content of this template is not important - just leave it empty, it's
-- never shown. Only it's SDL requirement is important and is used.
--
-- In order to keep @(show \$ 'arfdbeEntityType' err)@ string as short
-- as possible it's recommended to use @newtype@, when type is composite,
-- like tuple, Either, Map etc.
module Database.ReadableFromDB__ where

import qualified Data.ByteString.Lazy.UTF8.Unified as Lazy     (ByteString)
import qualified Data.ByteString.Lazy.UTF8.Unified as B hiding (ByteString)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Typeable
import Database.HDBC
import Database.HDBC.PostgreSQL
import Database.ReadableFromDB
import Text.PCLT.SH__
import Text.PCLT.ShowAsPCSI__

instance ShowAsPCSI SqlError where
        showAsPCSI e =
                thePCSI
                        "DB_ERROR"
                        [ ("seState"      , PlainText_PV $        seState       e)
                        , ("seNativeError", PlainText_PV $ show $ seNativeError e)
                        , ("seErrorMsg"   , PlainText_PV $        seErrorMsg    e)
                        ]

instance ShowAsPCSI AddressedReadFromDBError where
        showAsPCSI err =
                let err_pcsi =
                        case arfdbeErr err of
                            RecieveError_RFDBE e ->
                                addToPCSI [showAsPCSI e] (empPCSI "ARFDBE_DET_RCVERR")
                            NotFoundInDB_RFDBE ->
                                empPCSI "ARFDBE_DET_NOTFND"
                            TooManyResultsFoundInDB_RFDBE max_rows_count ->
                                thePCSI "ARFDBE_DET_TOOMUCHRES" [("max_rows_count", PlainText_PV $ show max_rows_count)]
                            RowParseError_RFDBE e ->
                                addToPCSI [showAsPCSI e] (empPCSI "ARFDBE_DET_PARSERR")
                            SubReadError_RFDBE arfdbe ->
                                thePCSI "ARFDBE_DET_SUBREADE" [("sub_arfdbe", PCSI_PV $ showAsPCSI arfdbe)]
                    -------------------------------------
                    sdl_reference = ("sdl_reference", PCSI_PV $ empPCSI $ (++) "ARFDBE_SDLR." $ show $ arfdbeEntityType err)
                                        -- Some thoughts about usage of (Show TypeRep) here:
                                        -- 1) Remember. It ignores type synonyms. F.e. no String, but [Char] must be used, orelse (even better) wrap it into newtype
                                        -- 2) Making PCSI ID "ARFDBE_SDLR.(Type1, Type2)" is WRONG - tuple is shown with no whitespace after comma
                                        -- 3) Nonptelude types are shown with module path. So template ID "ARFDBE_SDLR.AddressedReadFromDBError" is wrong - it must be "ARFDBE_SDLR.Database.ReadableFromDB.AddressedReadFromDBError"
                                        -- 4) Assumming (1) and (3), it's highly recommended to use newtypes for complex target types of ReadFromDB, like tuples or others.
                 in thePCSI
                        "ARFDBE"
                        [ ("read_object_type"  , PlainText_PV $ show $ arfdbeEntityType err)
                        , ("arfdbe_err_details", PCSI_PV $ addPVs2PCSI [sdl_reference] err_pcsi) -- << notice: have to duplicate sdl_reference, since PCSI_PV separates valuespaces of paremeters
                        , ("obj_pk"            , PlainText_PV $ arfdbePK err)
                        , sdl_reference
                        ]



data PCLTRawCatalog__Database_ReadableFromDB = PCLTRawCatalog__Database_ReadableFromDB
instance HasStaticRawPCLTs PCLTRawCatalog__Database_ReadableFromDB where
     getStaticRawPCLTs inner_cfg _ = flip (,) [] $
                PCLT_RawCatalogData $ M.fromList
                        [
                          ("DB_ERROR", (M.fromList [("rus", B.pack "Провал операции в связи с БД. Состояние: \"@@|seState@@|\". Уровень: @@|seNativeError@@|. Сообщение: \"@@|seErrorMsg@@|\"."), ("eng", B.pack "Operation with DB failed. State: \"@@|seState@@|\". Layer: @@|seNativeError@@|. Message: \"@@|seErrorMsg@@|\".")], str2PCLT_SDL Required_SDLM "##|LLEXCPT##|" inner_cfg))

                        , ("ARFDBE", (M.fromList [("rus", B.pack "Ошибка при чтении объекта из БД. ##|ARFDBE_OBJ_TYPE##| ##|ARFDBE_OBJ_PK##| ##|ARFDBE_DET##|"), ("eng", B.pack "An error occurred, when tried to read an object from DB. ##|ARFDBE_OBJ_TYPE##| ##|ARFDBE_OBJ_PK##| ##|ARFDBE_DET##|")], str2PCLT_SDL Required_SDLM "##|ARFDBE_DET##|" inner_cfg))
                        , ("ARFDBE_OBJ_TYPE", (M.fromList [("rus", B.pack "Тип объекта: @@|read_object_type@@|."), ("eng", B.pack "Object type: @@|read_object_type@@|.")], str2PCLT_SDL Required_SDLM "##|ARFDBE##|" inner_cfg))
                        , ("ARFDBE_SDLR", (M.fromList [("rus", B.pack "@@|sdl_reference@@|"), ("eng", B.pack "@@|sdl_reference@@|")], str2PCLT_SDL Required_SDLM "@@|sdl_reference@@|" inner_cfg))
                        , ("ARFDBE_OBJ_PK", (M.fromList [("rus", B.pack "Первичный ключ: @@|obj_pk@@|."), ("eng", B.pack "Object primary key: @@|obj_pk@@|.")], str2PCLT_SDL Required_SDLM "##|ARFDBE_SDLR##|" inner_cfg))
                        , ("ARFDBE_DET", (M.fromList [("rus", B.pack "@@|arfdbe_err_details@@|"), ("eng", B.pack "@@|arfdbe_err_details@@|")], str2PCLT_SDL Required_SDLM "##|ARFDBE_SDLR##|" inner_cfg))
                        , ("ARFDBE_DET_RCVERR", (M.fromList [("rus", B.pack "##|DB_ERROR##|"), ("eng", B.pack "##|DB_ERROR##|")], str2PCLT_SDL Required_SDLM "##|DB_ERROR##|" inner_cfg))
                        , ("ARFDBE_DET_NOTFND", (M.fromList [("rus", B.pack "Объект не найден!"), ("eng", B.pack "Object not found!")], str2PCLT_SDL Required_SDLM "##|ARFDBE_DET##|" inner_cfg))
                        , ("ARFDBE_DET_TOOMUCHRES", (M.fromList [("rus", B.pack "Запрос к БД вернул слишком много результатов, при допустимом максимуме - @@|max_rows_count@@| строк."), ("eng", B.pack "DB query returned too many rows. Allowed rows count maximum is @@|max_rows_count@@|.")], str2PCLT_SDL Required_SDLM "##|ARFDBE_DET##|" inner_cfg))
                        , ("ARFDBE_DET_PARSERR", (M.fromList [("rus", B.pack "Провал перевода данных из формата БД в оперативный формат: ##|LLEXCPT##|"), ("eng", B.pack "Failed to parse object data from DB format to operative one: ##|LLEXCPT##|")], str2PCLT_SDL Required_SDLM "##|ARFDBE_DET##|" inner_cfg))
                        , ("ARFDBE_DET_SUBREADE", (M.fromList [("rus", B.pack "Провал подоперации, которая также связана с чтением из БД: @@|sub_arfdbe@@|"), ("eng", B.pack "Subroutine, that is also about reading from DB, failed: @@|sub_arfdbe@@|")], str2PCLT_SDL Required_SDLM "##|ARFDBE_DET##|" inner_cfg))
                        ]
     widenessOfStaticRawPCLTsSet _ = Module_RPSW
