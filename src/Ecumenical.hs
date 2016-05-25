{-# LANGUAGE OverloadedStrings #-}
module Ecumenical
    ( retrieve
    ) where

import Data.ByteString.Char8

-- Key/value store
retrieve :: ByteString -> IO (Maybe ByteString)
retrieve key = do
    return Nothing
