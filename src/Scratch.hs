{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances #-}
--{-# OPTIONS_GHC -ddump-splices #-}

module Scratch where

import Prelude hiding ((.))
import Data.Time.Calendar

import Data.RecordAccess
import Data.RecordAccess.TH

$(record [d|
  data Person = Person 
    { name    :: String
    , dob     :: Day
    } deriving Show
  |])

$(record [d|
  data Company = Company 
    { name    :: String
    , number  :: String
    } deriving Show
  |])

$(subrecord [d|
  data Named = Named 
    { name    :: String
    }
  |])

printName :: (Named f) => f -> IO ()
printName = putStrLn . name

bozo = Person "Bozo The Clown" (fromGregorian 1971 01 10)

acme = Company "Acme Inc" "12345678"

main = do
  printName bozo
  printName acme