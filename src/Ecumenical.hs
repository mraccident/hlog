{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ecumenical
    ( retrieve
    , retrieve'
    , runMockFS
    , put
    ) where

import Prelude hiding (readFile, writeFile)
import Data.ByteString.Char8
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans

newtype MockDB m a = MockDB
    { db :: ReaderT (Maybe ByteString) m a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadTrans
             , MonadReader (Maybe ByteString)
             )

class Monad m => MonadDB m where
    get :: ByteString -> m (Maybe ByteString)

instance MonadDB IO where
    get = retrieve

instance Monad m => MonadDB (MockDB m) where
    get _ = ask

runMockFS :: MockDB m a -> Maybe ByteString -> m a
runMockFS (MockDB s) = runReaderT s

-- New version of retrieve using the monad transformer backing store.
-- Note that the IO instance of MonadDB just delegates to the old retrieve
-- function for now; this will be changed. Probably.
retrieve' :: MonadDB m => ByteString -> m (Maybe ByteString)
retrieve' = get

-- Get a value from the store by key, if it exists.
retrieve :: ByteString -> IO (Maybe ByteString)
retrieve key = fileContents $ dataPath key

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
