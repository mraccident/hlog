{-# LANGUAGE OverloadedStrings #-}
module EcumenicalSpec
    (spec) where

import Test.Hspec
import Ecumenical

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Ecumenical DB" $ do
        -- There is no longer any need to run hlog from a directory ℵ₀ levels
        -- beneath root in your filesystem.
        it "does not traverse paths in keys" $ do
            result <- retrieve "../../../../../../../../../../../../etc/passwd"
            result `shouldBe` Nothing

    describe "Mockable DB" $ do
        it "actually works" $ do
            let env "arbitrary key" = Just "foof"
            result <- runMockFS
                (retrieve "arbitrary key") env
            result `shouldBe` Just "foof"
        it "does indirect lookup" $ do
            let env "foo" = Just "bar"
                env "bar" = Just "baz"
            result <- runMockFS
                (indirect "foo") env
            result `shouldBe` Just "baz"
        it "does indexed lookup" $ do
            -- XXX: this requires the test to understand the index format,
            -- which is a little bit garbage
            let env "index" = Just "foo bar baz"
                env "bar" = Just "correct"
            result <- runMockFS
                (indexed "index" 1) env
            result `shouldBe` Just "correct"
        it "does indexed lookup again" $ do
            let env "index" = Just "foo bar baz"
                env "bar" = Just "wrong"
                env "baz" = Just "correct"
            result <- runMockFS
                (indexed "index" 2) env
            result `shouldBe` Just "correct"
