{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V

main :: IO ()
main = do
  print (decode "[1,2,3]" :: Maybe [Integer])
  print (eitherDecode "[]" :: Either String Integer)
  print (eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool)))
  x <- readJSONFileStrict "test.json"
  print (Just (x :: Value))
  print (parseMaybe parseArray x)


parseArray :: Value -> Parser [(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
               mapM parseTuple (V.toList arr)


parseTuple :: Value -> Parser (String, Bool)
parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)

-- readJSONFileStrict :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFileStrict :: FilePath -> IO Value
readJSONFileStrict fp = liftIO $ do
  bs <- B.readFile fp
  case eitherDecodeStrict' bs of
    Left e -> throwM $ userError e
    Right x -> return x
