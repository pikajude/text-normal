{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NFD (specs) where

import Data.Monoid
import Data.Text.Normal.NFC
import Data.String
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Instances ()

instance Arbitrary Normal where
    arbitrary = fmap fromText arbitrary

angstrom_sign, a_with_ring, e_acute, e_with_acute :: IsString a => a

-- should be identical under NFKC
angstrom_sign = "\8491"
a_with_ring = "\197"

e_acute = "\233"
e_with_acute = "e\769"

specs = do
    describe "Normal type" $ do
        prop "should look identical to Text type" $
            \a -> show (toText a) == show a

        prop "should Read identical to Text type" $
            \a -> fromText (read (show a))
               == read (show $ toText a)

        it "should normalize when using IsString instance" $ do
            "a" `shouldBe` fromText "a"
            angstrom_sign `shouldBe` fromText a_with_ring

    describe "fromText" $ do
        prop "is idempotent" $
            \a -> toText a == toText (fromText (toText a))

        it "normalizes" $ do
            fromText angstrom_sign `shouldBe` fromText a_with_ring
            fromText e_with_acute `shouldBe` fromText e_acute

    describe "appending" $ do
        describe "follows monoid laws" $ do
            prop "mappend mempty x = x" $ \(a :: Normal) -> a == mempty <> a
            prop "mappend x mempty = x" $ \(a :: Normal) -> a == a <> mempty
            prop "mappend x (mappend y z) = mappend (mappend x y) z" $
                \(a :: Normal) b c -> a <> (b <> c) == (a <> b) <> c
            prop "mconcat = foldr mappend mempty" $
                \(as :: [Normal]) -> mconcat as == foldr (<>) mempty as

        it "normalizes the result" $
            property $ \a b -> fromText a <> fromText b == fromText (a <> b)

        it "normalizes the result, take 2" $
            fromText "e" <> fromText "\769" `shouldBe` fromText e_with_acute

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
