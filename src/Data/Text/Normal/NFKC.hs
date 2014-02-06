{-# LANGUAGE DeriveDataTypeable #-}

-- | Strings normalized according to Normalization Form Compatibility
-- Composition.
module Data.Text.Normal.NFKC (
    Normal,
    fromText, toText
) where

import Control.Arrow (first)
import Control.DeepSeq
import Data.Data
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Text.ICU.Normalize

-- | Normalized text.
newtype Normal = Normal Text deriving (Eq, Ord, Data, Typeable)

-- | Convert 'Text' efficiently to 'Normal'.
fromText :: Text -> Normal
fromText t = Normal $ case quickCheck NFKC t of
    Nothing | isNormalized NFKC t -> t
            | otherwise -> normalize NFKC t
    Just False -> normalize NFKC t
    Just True -> t

-- | Convert 'Normal' to 'Text'. This function has zero runtime cost.
toText :: Normal -> Text
toText (Normal t) = t

instance Show Normal where
    show = show . toText

instance Read Normal where
    readsPrec i = map (first fromText) . readsPrec i

instance Monoid Normal where
    mappend (Normal t1) (Normal t2) = Normal $ t1 <> t2
    mempty = Normal mempty

instance IsString Normal where
    fromString = fromText . fromString

instance NFData Normal where
    rnf = rnf . toText
