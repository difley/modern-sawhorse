{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Control.Monad as CM (join)
import Data.Aeson.Types
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import GHC.Exts (fromList, IsString)
import qualified Data.Map as DM
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (Hashable)
import Data.Maybe (isJust, fromMaybe)

main :: IO ()
main = do
  print (decode "[1,2,3]" :: Maybe [Integer])
  print (decode "[{\"e\":1},{\"e\":2},{\"e\":3}]" :: Maybe [Object])
  print (decode "[{\"e\":1},{\"e\":2},{\"e\":3}]" :: Maybe [DM.Map String Integer])
  print $ fromMaybe [] (decode "[{\"a\":\"4\",\"b\":\"wow\"},{\"b\":\"ew\",\"a\":\"3\"},{\"a\":\"4\",\"b\":\"coonhound\"}]" :: Maybe [DM.Map String String])
  print $ fmap DM.size $ fromMaybe [] (decode "[{\"e\":1},{\"e\":2,\"r\":7},{\"e\":3}]" :: Maybe [DM.Map String Integer])
  print $ fmap (DM.lookup "a") $ fromMaybe [] (decode "[{\"a\":\"4\",\"b\":\"wow\"},{\"b\":\"ew\",\"a\":\"3\"},{\"a\":\"4\",\"b\":\"coonhound\"}]" :: Maybe [DM.Map String String])
  print (eitherDecode "[]" :: Either String Integer)
  print (eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool)))
  x <- readJSONFileStrict "test.json"
  print (Just (x :: Value))
  print (Just ((revStrings x):: Value))
  print (parseMaybe parseArray x)
  u <- readJSONFileStrict "coonhound.json"
  print $ mapFromPairList $ fromMaybe [] (parseMaybe parseArray' u)
  writeJSONFileLazy "coonhound_grouped.json" $ toJSON (mapFromPairList $ fromMaybe [] (parseMaybe parseArray' u))
  z <- print(encode x)
  y <- B.readFile "test.json"
  t <- print (decode (("{\"bar\":1,\"foo\":1}" :: BSL.ByteString)) :: Maybe Object)
  B.writeFile "output.json" y
  writeJSONFileLazy "more_output.json" x
  writeJSONFileLazy "reversed.json" (revStrings x)
  writeJSONFileLazy "groupedbyb.json" (groupbyb x)
  print $ groupList [3,4,5,6,3,5,5]
  print $ groupList [("a", 3),("a", 4),("a", 5),("a", 6),("a", 3),("a", 5),("a", 5)]
  print $ groupTupleList [("a", 3),("a", 4),("b", 5),("a", 6),("a", 3),("a", 5),("a", 5)]
  print $ fmap unzip (groupTupleList [("a", 3),("a", 4),("b", 5),("a", 6),("a", 3),("a", 5),("a", 5)])
  -- print $ mapFromPairList [("a", 3),("a", 4),("b", 5),("a", 6),("a", 3),("a", 5),("a", 5)]
  writeJSONFileLazy "handrwritten.json" $ Array $ fromList $ fmap (Object . fromList) [[("a", "4"), ("b", "wow")], [("b", "ew"), ("a", "3")]]
  -- print $ mapFromPairList [("a", Number 3),("a", Number 4),("b", Number 5),("a", Number 6),("a", Number 3),("a", Number 5),("a", Number 5)]
  -- print $ mapFromPairList  $ getValuePairs  [[("a", "4"), ("b", "wow")], [("b", "ew"), ("a", "3")], [("a", "4"), ("b", "coonhound")]]
  print $ Object $ fromList [("a", "4"), ("b", "wow")]
  -- print $ fmap HM.toList $ Object $ fromList [("a", "4"), ("b", "wow")]

--  result <- decode "{\"name\":\"Dave\",\"age\":2}"
--    flip parseMaybe result $ \obj -> do
--      age <- obj .: "age"
--      name <- obj .: "name"
--      return (name ++ ": " ++ show (age*2))
--  print result



revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x)  = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k, v) = (T.reverse k, revStrings v)
                        in  Object . fromList . map revPair . HM.toList $ x
revStrings other      = other


groupbyb :: Value -> Value
groupbyb (String x) = String x
groupbyb (Array x) = Array (fmap groupbyb x)
groupbyb (Object x) = let myPair (k, v) = (k, String "yes")
                      in Object . fromList . fmap myPair . HM.toList $ x
groupbyb other     = other


groupValue :: Value -> Value
groupValue (String x) = String x
groupValue (Number x) = Number x
groupValue (Array x) = Array (fmap groupbyb x)
groupValue (Object x) = let myPair (k, v) = (k, v)
                      in Object . fromList . fmap id . HM.toList $ x
groupValue other     = other


-- groupObject :: Value -> Value
-- groupObject (Array x) = foldl (++) Array 

-- mapToValue :: (Eq a, Eq b, Ord a) => [(a, [String b])] -> Value
-- mapToValue myList = Object (fromList myList)


getbvalue :: Value -> Parser String
getbvalue (Object x) = x .: "a"
-- getbvalue (Object x) = DM.lookup "a" (fromList [("a", x)])
-- getbvalue (Object x) = Object . fromList . (DM.lookup "a" (fromList [("a", 3)]))

mapFromPairList :: (Eq b, IsString a, Ord a, Hashable a) => [(a, b)] -> HM.HashMap a [b]
mapFromPairList myList = let unzippedGroups = fmap unzip (groupTupleList myList)
                         in HM.fromList $ fmap (\group -> (head (fst group), snd group)) unzippedGroups

-- mapFromPairList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [(a, [b])]
-- mapFromPairList myList = let unzippedGroups = fmap unzip (groupTupleList myList)
--                          in fmap (\group -> (head (fst group), snd group)) unzippedGroups


groupList :: Ord a => [a] -> [[a]]
groupList myList = DL.group $ DL.sort myList

sortTupleList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [(a, b)]
sortTupleList myList = DL.sortBy (\a b -> compare (fst a) (fst b)) myList

groupTupleList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [[(a, b)]]
groupTupleList myList = DL.groupBy pairKeyMatches $ sortTupleList myList

pairKeyMatches :: (Eq a) => (a, b) -> (a, b) -> Bool
pairKeyMatches x y = (fst x) == (fst y)


getaValue :: (Eq a) => a -> (a, b) -> Maybe b
getaValue key myPair 
          | (fst myPair) == key = Just (snd myPair)
          | otherwise = Nothing

getFirstValue :: (Eq a, Eq b) => a -> [(a, b)] -> Maybe b
getFirstValue key myPairs = CM.join $ DL.find (/= Nothing) $ fmap (getaValue key) myPairs

getFirstValuePair :: (Eq a, Eq b, IsString a, IsString b) => [(a, b)] -> (Maybe b, Maybe b)
getFirstValuePair myPairs = (getFirstValue "a" myPairs, getFirstValue "b" myPairs)

getValuePairs :: (Eq a, Eq b, IsString a, IsString b) => [[(a, b)]] -> [(b, b)]
getValuePairs myPairs = fmap (\a -> (maybe "" id (fst a), maybe "" id (snd a))) $ filter (\a -> (isJust (fst a)) && (isJust (snd a))) $ fmap getFirstValuePair myPairs

parseArray :: Value -> Parser [(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
               mapM parseTuple (V.toList arr)


parseTuple :: Value -> Parser (String, Bool)
parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)


parseArray' :: Value -> Parser [(String, String)]
parseArray' = withArray "array of tuples" $ \arr ->
               mapM parseTuple' (V.toList arr)

parseTuple' :: Value -> Parser (String, String)
parseTuple' = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)


-- from https://stackoverflow.com/a/41566055/382936
-- readJSONFileStrict :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFileStrict :: FilePath -> IO Value
-- readJSONFileStrict fp = liftIO $ do
readJSONFileStrict fp = do
  bs <- B.readFile fp
  case eitherDecodeStrict bs of
    Left e -> throwM $ userError e
    Right x -> return x


-- groupbyParser :: Parser [(String, Bool)] -> Parser Object
-- groupbyParser parsedinput = parsedinput


writeJSONFileLazy :: FilePath -> Value -> IO ()
writeJSONFileLazy fp jsonValue = 
  BSL.writeFile fp (encode jsonValue)
