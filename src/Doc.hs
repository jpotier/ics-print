{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Doc
  ( text
  , nest
  , line
  , prefix
  , Doc
  , render
  )
  where

import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (fold)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- From A Prettier Printer

data DocF k
  = Nil
  | Text Builder k
  | Line k
  | Nest Int k
  deriving Functor

newtype Fix f = Fix { getFix :: f (Fix f) }

type Doc = Fix DocF

instance Semigroup Doc where
  Fix Nil <> y = y
  Fix (Text c x) <> y = Fix (Text c (x <> y))
  Fix (Line x) <> y = Fix (Line (x <> y))
  Fix (Nest i x) <> y = Fix (Nest i (x <> y))

instance Monoid Doc where
  mempty = nil

instance IsString Doc where
  fromString = text . T.pack

nil :: Doc
nil = Fix Nil

text :: Text -> Doc
text str = Fix (Text (byteString $ TE.encodeUtf8 str) nil)

-- | Add a newline
line :: Doc
line = Fix (Line nil)

-- | Nest all the newlines
nest :: Int -> Doc -> Doc
nest n x = Fix (Nest n x)

prefix :: Doc -> Doc -> Doc
prefix a b = a <> b

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = c where c = f . fmap c . getFix

render :: Doc -> Builder
render doc = cata go doc 0
  where
    go :: DocF (Int -> Builder) -> Int -> Builder
    go = \case
      Nil -> const ""
      Text str k -> \i -> str <> k i
      Line k -> \i -> "\n" <> fold (replicate i " ") <> k i
      Nest j k -> \i -> k (i + j)
