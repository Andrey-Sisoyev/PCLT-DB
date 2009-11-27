{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, FlexibleContexts #-}

-- | An attemt of generalization of \"read from DB\" operation.
module Database.ReadableFromDB where

import Control.Exception
import Data.Convertible
import Data.Char
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.MyHelpers
import Data.Typeable
import Database.HDBC
import Database.HDBC.PostgreSQL

-- * ReadableFromDB

type FieldName = String
-- | Type aliase for use by 'readOneFromDB'.
type NoMoreThanOne_shoulditbe = Bool

-- | It's recommended to use newtypes for subject type variable (\"readable\"),
-- when it is composite (like tuple, Either, Map or other). Reason for that
-- is described near the 'ShowAsPCSI' instaniation
-- for 'AddressedReadFromDBError'.
class (Typeable readable, Show pk) => ReadableFromDB readable pk where
     -- | Read rows from DB and parse each at once
     -- using 'parseDBrow' class method
     readFromDB    :: Connection -> pk                             -> IO [Either AddressedReadFromDBError readable]
     parseDBrow    :: Connection -> pk -> Map FieldName SqlValue   -> IO (Either AddressedReadFromDBError readable)
     -- | Class method derived from 'readFromDB' and 'parseDBrow'.
     -- No need to overload it, if not for special cases.
     readOneFromDB :: Connection -> pk -> NoMoreThanOne_shoulditbe -> IO (Either AddressedReadFromDBError readable)
     readOneFromDB db_conn pk nomorethanone_shoulditbe = do
        results_list <- readFromDB db_conn pk
        return $
            case results_list of
                [] -> wrapParseResult_1row pk $ Left NotFoundInDB_RFDBE
                err_or_readable : [] -> err_or_readable
                err_or_readable : _  -> case nomorethanone_shoulditbe of
                                     True  -> wrapParseResult_1row pk $ Left $ TooManyResultsFoundInDB_RFDBE 1
                                     False -> err_or_readable

-- | Type aliase for use by 'TooManyResultsFoundInDB_RFDBE'.
type AllowedMaximumOfRowsCount = Int
-- | What kind of errors are possible in the result
-- of \"read from DB\" operation
data ReadFromDBError =
          RecieveError_RFDBE SqlError
        | NotFoundInDB_RFDBE
        | TooManyResultsFoundInDB_RFDBE AllowedMaximumOfRowsCount
        | RowParseError_RFDBE SomeException
        | SubReadError_RFDBE AddressedReadFromDBError
      deriving (Show, Typeable)

-- | A wrapper around 'ReadFromDBError', added information about type of
-- read subject and by showing the primary key.
--
-- ___________
--
-- If PCLT is to be used for representation of 'AddressedReadFromDBError'
-- then user must declare an additional localizable template in his PCLT
-- catalog with ID of form:
--
-- @\"ARFDBE_SDLR.\" ++ (show $ 'arfdbeEntityType' err)@
--
-- , where the @show 'arfdbeEntityType'@ will include full modules path
-- of type. The templete content if not important - leave it empty, but what
-- is important - is it's SDL requirement. This SDL requirement wil be in
-- effect for the representation of 'AddressedReadFromDBError'.
--
-- For more information on that, see description for @ShowAsPCSI@ instance for
-- 'AddressedReadFromDBError' - in module "Database.ReadableFromDB__".
data AddressedReadFromDBError = AddressedReadFromDBError {
             arfdbeEntityType  :: TypeRep
           , arfdbePK          :: String
           , arfdbeErr         :: ReadFromDBError
        } deriving (Show, Typeable)

-- | In case of error it wraps it 'ReadFromDBError'
-- into 'AddressedReadFromDBError'.
wrapParseResult_1row :: (Typeable readable, Show pk) => pk -> Either ReadFromDBError readable -> Either AddressedReadFromDBError readable
wrapParseResult_1row pk err_or_readable =
                case err_or_readable of
                    Right readable -> Right readable
                    Left err -> Left AddressedReadFromDBError {
                                          arfdbeEntityType = rightType_formReadResOrErr err_or_readable
                                        , arfdbePK         = show pk
                                        , arfdbeErr        = err
                                        }

-- | Wrapper around 'wrapParseResult_1row'.
wrapParseResult_Nrows :: (Typeable a, Show pk) => pk -> [Either ReadFromDBError a] -> [Either AddressedReadFromDBError a]
wrapParseResult_Nrows pk err_or_a__list = map (wrapParseResult_1row pk) err_or_a__list

-----------------------------------------------
-- * Helpers

-- | Return the 'TypeRep' of what can be under @Right@ in @Either@
rightType_formReadResOrErr :: (Typeable left, Typeable right) => Either left right -> TypeRep
rightType_formReadResOrErr left_or_right = let _ : right_type : [] = typeRepArgs (typeOf left_or_right) in right_type

-- | This is better than @row ! field_name@, because if an error occurs
-- (field not found), then it returns in error message this field name.
takeFieldValue :: Convertible SqlValue a => FieldName -> Map FieldName SqlValue -> a
takeFieldValue f_name row_map =
                let mb_sqlv = M.lookup f_name row_map
                 in case mb_sqlv of
                        Just  v -> fromSql v
                        Nothing -> error ("Function 'takeFieldValue' failed!\nField '" ++ f_name ++ "' not found.\nRow: " ++ show row_map)

-- | Uppercase given field name, and apply it with 'takeFieldValue'.
takeUFieldValue :: Convertible SqlValue a => FieldName -> Map FieldName SqlValue -> a
takeUFieldValue f_name row_map = takeFieldValue (map toUpper f_name) row_map

-- | WARNING !!!
-- HDBC lowercases field names, when using @fetchAllRowsMap@.
uppercaseMapKeys :: Map String a -> Map String a
uppercaseMapKeys = M.mapKeys (map toUpper)

-- |
--
-- > liftInList a = a : []
liftInList :: a -> [a]
liftInList a = a : []