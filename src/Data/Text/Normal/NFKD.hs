{-# LANGUAGE DeriveDataTypeable #-}

-- | Strings normalized according to Normalization Form Compatibility
-- Decomposition.
module Data.Text.Normal.NFKD (
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
newtype Normal = Normal
               { -- | Convert 'Normal' to 'Text'. This function just unwraps the newtype, so there is zero runtime cost.
                 toText :: Text } deriving (Eq, Ord, Data, Typeable)

-- | Convert 'Text' efficiently to 'Normal'.
fromText :: Text -> Normal
fromText t = Normal $ case quickCheck NFKD t of
    Nothing | isNormalized NFKD t -> t
            | otherwise -> normalize NFKD t
    Just False -> normalize NFKD t
    Just True -> t

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
