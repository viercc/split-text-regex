{-# LANGUAGE DeriveTraversable #-}

module Template
  ( AList (..),
    Hole (..),
    Template,
    parseTemplate,
    parseHole,
    unparseTemplate,
    unparseHole,
    applyTemplate,
  )
where

import qualified Data.Array as Array
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (isDigit)
import Data.Either.Validation
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Text.Regex.Base
import Text.Regex.TDFA

data AList str var = Last str | Next str var (AList str var)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Bifunctor AList where
  first f (Last a) = Last (f a)
  first f (Next a b r) = Next (f a) b (first f r)

  second = fmap

  bimap = bimapDefault

instance Bifoldable AList where
  bifoldMap = bifoldMapDefault

instance Bitraversable AList where
  bitraverse f g = go
    where
      go (Last a) = Last <$> f a
      go (Next a b r) = Next <$> f a <*> g b <*> go r

data Hole = Escaped String | MatchAt Int
  deriving (Show, Eq)

splitBy :: (Monoid str, RegexLike re str) => re -> str -> AList str (MatchText str)
splitBy re = go
  where
    go str = case matchOnceText re str of
      Nothing -> Last str
      Just (pre, matched, post) -> Next pre matched (go post)

getWholeMatch :: MatchText str -> str
getWholeMatch m = fst (m Array.! 0)

-- Special sequences:
--  * \\
--  * \0, \1, ..., \10, \11, ...
--  * \{0}, \{1}, ..., \{10}, \{11}, ...
patSpecial :: Regex
patSpecial = makeRegex "\\\\(\\\\|[[:digit:]]+|\\{[[:digit:]]+\\})"

type Template str = AList str Hole

parseTemplate :: String -> Template String
parseTemplate = second (toHole . getWholeMatch) . splitBy patSpecial
  where
    toHole special = fromMaybe err $ parseHole special
      where
        err = error $ "parseTemplate: impossible: special=" ++ show special

parseHole :: String -> Maybe Hole
parseHole special = case special of
  "\\\\" -> Just $ Escaped "\\"
  ('\\' : '{' : digits) -> Just $ MatchAt (read (takeWhile isDigit digits))
  ('\\' : digits) -> Just $ MatchAt (read (takeWhile isDigit digits))
  _ -> Nothing

unparseTemplate :: Template String -> String
unparseTemplate = bifoldMap id unparseHole

unparseHole :: Hole -> String
unparseHole (Escaped s) = '\\' : s
unparseHole (MatchAt a) = '\\' : show a

applyTemplate :: (Monoid str, IsString str) => Template str -> MatchText str -> Either [String] str
applyTemplate templ matched = fmap (mconcat . biToList) . validationToEither $ bitraverse pure fillHole templ
  where
    biToList = bifoldr (:) (:) []

    boundsCheck = Array.inRange (Array.bounds matched)

    fillHole (Escaped s) = Success (fromString s)
    fillHole (MatchAt n)
      | boundsCheck n = Success $ fst (matched Array.! n)
      | otherwise = Failure ["Out of bounds:" ++ show (unparseHole (MatchAt n))]