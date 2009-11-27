{-
Copyright (C) 2009 Andrejs Sisojevs <andrejs.sisojevs@nextmail.ru>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- File with HelloWorld data structures. Had to separate this, since we separated PCLT templates in HelloWorld__.hs and we want to avoid recursive module imports.
-- Main file is HelloWorld.hs

module HelloWorld_Data where

-----------------------------------------------------
-----------------------------------------------------
-- Application specific modules
import Control.Exception

-----------------------------------------------------
-----------------------------------------------------
-- Application specific data structures

-- \/ data
data HelloWorld = HelloWorld

-- \/ errors
type WorldName = String
type WorldIndex = Int
data HelloWorldError =
          NoWorld_HWE
        | AmbiguousChoiceOfWorlds_HWE (WorldName, WorldIndex) (WorldName, WorldIndex) [(WorldName, WorldIndex)]
        | SomeVeryStrangeError_HWE Int String Bool (Maybe Bool) (Maybe Bool) SomeException
        | FailedDueToErrorInSubsystem_HWE ErrorInSubsystem

data ErrorInSubsystem =
          ErrorType1_EIS
        | ErrorType2_EIS
        | FailedDueToErrorInSub_sub_system_EIS ErrorInSub_sub_system

data ErrorInSub_sub_system =
          ErrorType1_EISS
        | ErrorType2_EISS