{-# LANGUAGE OverloadedStrings #-}
module Ecumenical
    ( retrieve
    , put
    ) where

import Prelude hiding (readFile, writeFile)
import Data.ByteString.Char8
import Control.Exception

-- Get a value from the store by key, if it exists.
retrieve :: ByteString -> IO (Maybe ByteString)
retrieve key = do
    value <- fileContents key
    return value

put :: ByteString -> ByteString -> IO ()
put key value = do
    writeFile (unpack key) value
    return ()

fileContents :: ByteString -> IO (Maybe ByteString)
fileContents path = do
    contents <- (try $ readFile $ unpack path) :: IO (Either IOException ByteString)
    case contents  of
        Left _ -> return Nothing
        Right text -> return $ Just text
