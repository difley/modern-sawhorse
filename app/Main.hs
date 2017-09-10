{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import GHC.Exts
import qualified Data.Map as DM
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  print (decode "[1,2,3]" :: Maybe [Integer])
  print (eitherDecode "[]" :: Either String Integer)
  print (eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool)))
  x <- readJSONFileStrict "test.json"
  print (Just (x :: Value))
  print (Just ((revStrings x):: Value))
  print (parseMaybe parseArray x)
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
  print $ mapFromPairList [("a", 3),("a", 4),("b", 5),("a", 6),("a", 3),("a", 5),("a", 5)]
  testIn "temple"


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
--groupbyb (Object x) = let myPair (k, v) = (k, groupbyb v)
                      in Object . fromList . fmap myPair . HM.toList $ x
groupbyb other     = other




getbvalue :: Value -> Parser String
getbvalue (Object x) = x .: "a" 
-- getbvalue (Object x) = DM.lookup "a" (fromList [("a", x)])
-- getbvalue (Object x) = Object . fromList . (DM.lookup "a" (fromList [("a", 3)]))

mapFromPairList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [(a, [b])]
mapFromPairList myList = let unzippedGroups = fmap unzip (groupTupleList myList)
                         in fmap (\group -> (head (fst group), snd group)) unzippedGroups

-- [(a, b)] -> [[(a, b)]] -> [([a], [b])] ->  ? -> [(a, b)]


testIn :: [Char] -> IO ()
testIn a = let b = "|||" ++ a ++ "|||"
           in print b

groupList :: Ord a => [a] -> [[a]]
groupList myList = DL.group $ DL.sort myList

sortTupleList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [(a, b)]
sortTupleList myList = DL.sortBy (\a b -> compare (fst a) (fst b)) myList

groupTupleList :: (Eq a, Eq b, Ord a) => [(a, b)] -> [[(a, b)]]
groupTupleList myList = DL.groupBy pairKeyMatches $ sortTupleList myList

pairKeyMatches :: (Eq a) => (a, b) -> (a, b) -> Bool
pairKeyMatches x y = (fst x) == (fst y)

parseArray :: Value -> Parser [(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
               mapM parseTuple (V.toList arr)


parseTuple :: Value -> Parser (String, Bool)
parseTuple = withObject "tuple" $ \o -> do
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
