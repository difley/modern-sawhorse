{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (toJSON, eitherDecodeStrict, encode, withObject, withArray)
import Control.Monad as CM (join)
import Data.Aeson.Types (Value (String), Parser, parseMaybe, (.:))
import Control.Monad.Catch (throwM)
import Data.ByteString (ByteString)
import GHC.Exts (fromList, IsString)
import qualified Data.HashMap.Strict as HM (HashMap, fromList)
import qualified Data.ByteString as B (readFile)
import qualified Data.List as DL (sortBy, groupBy)
import qualified Data.Vector as V (toList)
import qualified Data.ByteString.Lazy as BSL (writeFile)
import Data.Hashable (Hashable)
import Data.Maybe (isJust, fromMaybe)

main :: IO ()
main = do
  u <- readJSONFileStrict "example.json"
  print $ mapFromPairList $ fromMaybe [] (parseMaybe parseArray' u)
  writeJSONFileLazy "example_grouped.json" $ toJSON (mapFromPairList $ fromMaybe [] (parseMaybe parseArray' u))



mapFromPairList :: (Eq b, IsString a, Ord a, Hashable a) => [(a, b)] -> HM.HashMap a [b]
mapFromPairList myList = let unzippedGroups = fmap unzip (groupTupleList myList)
                         in HM.fromList $ fmap (\group -> (head (fst group), snd group)) unzippedGroups


sortTupleList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [(a, b)]
sortTupleList myList = DL.sortBy (\a b -> compare (fst a) (fst b)) myList

groupTupleList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [[(a, b)]]
groupTupleList myList = DL.groupBy pairKeyMatches $ sortTupleList myList

pairKeyMatches :: (Eq a) => (a, b) -> (a, b) -> Bool
pairKeyMatches x y = (fst x) == (fst y)

parseArray' :: Value -> Parser [(String, String)]
parseArray' = withArray "array of tuples" $ \arr ->
               mapM parseTuple' (V.toList arr)

parseTuple' :: Value -> Parser (String, String)
parseTuple' = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)


-- modified from https://stackoverflow.com/a/41566055/382936
readJSONFileStrict :: FilePath -> IO Value
readJSONFileStrict fp = do
  bs <- B.readFile fp
  case eitherDecodeStrict bs of
    Left e -> throwM $ userError e
    Right x -> return x


writeJSONFileLazy :: FilePath -> Value -> IO ()
writeJSONFileLazy fp jsonValue = 
  BSL.writeFile fp (encode jsonValue)
