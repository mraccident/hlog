{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ecumenical
    ( retrieve
    , indirect
    , indexed
    , runMockFS
    ) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteString.Base64
import Data.ByteString.Char8
import Prelude hiding (readFile, writeFile)

-- This typeclass provides an abstraction of the DB's backing KV store
class Monad m => Ecumenical m where
    get :: ByteString -> m (Maybe ByteString)

-- The IO instance does an unimaginably bad thing by treating keys as
-- filenames and reading the value from the filesystem.
instance Ecumenical IO where
    get = retrieveFromFile . encode

-- ...but a Reader monad instance is also possible, which allows testing
-- the database functionality without relying on filesystem side effects.
newtype Mockumenical m a =
    Mockumenical (ReaderT (ByteString -> Maybe ByteString) m a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadTrans
             , MonadReader (ByteString -> Maybe ByteString)
             )

instance Monad m => Ecumenical (Mockumenical m) where
    get k = asks ($ k)

runMockFS :: Mockumenical m a -> (ByteString -> Maybe ByteString) -> m a
runMockFS (Mockumenical s) = runReaderT s

-- New version of retrieve using the monad transformer backing store.
-- Note that the IO instance of MonadDB just delegates to the old retrieve
-- function for now; this will be changed. Probably.
retrieve :: Ecumenical m => ByteString -> m (Maybe ByteString)
retrieve = get

indirect :: Ecumenical m => ByteString -> m (Maybe ByteString)
indirect x = do
    y <- retrieve x
    result <- case y of
        Nothing -> return Nothing
        Just y -> retrieve y
    return result

indexed :: Ecumenical m => ByteString -> Int -> m (Maybe ByteString)
indexed indexName n = do
    index <- retrieve indexName
    result <- case index of
        Nothing -> return Nothing
        Just index -> retrieve $ (split ' ' index) !! n
    return result

-- Get a value from the store by key, if it exists.
retrieveFromFile :: ByteString -> IO (Maybe ByteString)
retrieveFromFile = fileContents . dataPath

-- XXX: probably need a State or Free monad transformer wrapper for put as well
put :: ByteString -> ByteString -> IO ()
put key value = writeFile (dataPath key) value

fileContents :: FilePath -> IO (Maybe ByteString)
fileContents path = do
    contents <- (try $ readFile path) :: IO (Either IOException ByteString)
    case contents  of
        Left _ -> return Nothing
        Right text -> return $ Just text

dataPath :: ByteString -> FilePath
dataPath key = "data/" ++ (unpack key)
