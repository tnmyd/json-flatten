{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import GHC.Generics
import AesonFun


someFunc :: IO ()
someFunc = do --putStrLn "someFunc"
    let val = collapse input
    print val

data Foo = Foo
  { field1 :: Int
  , field2 :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)
  -- ToJSON so that we can encode *to* a JSON string,
  -- FromJSON so that we can parse *from* a JSON string

jsonString :: LB.ByteString
jsonString = "{ \"field1\": 27, \"field2\": \"hello!\" }"

maybeFoo :: Maybe Value
maybeFoo = decode jsonString 

input = "{ \"foo\" : \"baz\", \"bar\" : {\"qux\" : \"quux\", \"baz\" : {\"abracadabra\" : \"alakazam\", \"abc\" : \"xyz\" } } }"


