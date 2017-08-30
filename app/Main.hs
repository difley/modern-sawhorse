{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V

main :: IO ()
main = do
  print (decode "[1,2,3]" :: Maybe [Integer])
  print (eitherDecode "[]" :: Either String Integer)
  print (eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool)))



  s <- T.encodeUtf8 <$> T.getLine
  print (decode s :: Maybe Value)
  print (parseMaybe parseArray =<< decode s)

parseArray :: Value -> Parser [(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
               mapM parseTuple (V.toList arr)

-- parseArray (Array arr) = mapM parseTuple (V.toList arr)
-- parseArray _           = fail "expected an array"


-- parseTuple (Object obj) = do
--   let mbFieldA = HM.lookup "a" obj
-- 
--   fieldA <- case mbFieldA of
--     Just x -> return x
--     Nothing -> fail "no field 'a'"
-- 
--   a <- case fieldA of
--     String x -> return (T.unpack x)
--     _        -> fail "expected a string"
-- 
--   let b = True
-- 
--   return (a, b)

-- parseTuple = withObject "tuple" $ \obj -> do
--  a <- case HM.lookup "a" obj of
--    Just x -> parseJSON x
--    Nothing -> fail "no field 'a'"
--
--  b <- case HM.lookup "b" obj of
--    Just x -> parseJSON x
--    Nothing -> fail "no field 'b'"
--
--  return (a, b)

-- parseTuple = withObject "tuple" $ \o -> do
--  a <- o .: "a"
--  b <- o .: "b"
--  return (a, b)

parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)
