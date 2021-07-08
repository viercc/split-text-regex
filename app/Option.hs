{-# LANGUAGE DerivingVia #-}
module Option(
  parseOption,
  Option(..),
  ErrorPolicy(..),
  Overwrite(..),
  Rule(..)
) where

import Options.Applicative

import Text.Regex.Base
import Text.Regex.TDFA

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT

import Template
import Data.Bifunctor

data Option = Option {
    optionUnmatched :: ErrorPolicy,
    optionOverwrite :: Either ErrorPolicy Overwrite,
    optionRules :: [Rule]
  }

data ErrorPolicy = Fatal | ReportAll | Ignore
  deriving (Show, Read, Eq, Enum, Bounded)

listErrorPolicy :: [String]
listErrorPolicy = show <$> [Fatal ..]

data Overwrite = Overwrite | Append
  deriving (Show, Read, Eq, Enum, Bounded)

listOverwrite :: [String]
listOverwrite = show <$> [Overwrite ..]

data Rule = Rule {
    pat :: Regex
  , outFmt :: Template Text
  , destFmt :: Template Text
  }

parseOption :: IO Option
parseOption = execParser opts
  where
    opts = info (helper <*> optionP) fullDesc

optionP :: Parser Option
optionP = Option <$> unmatchedP <*> overwriteP <*> some ruleP
  where
    unmatchedP = option auto $
      long "unmatched" <>
      short 'u' <>
      completer (listCompleter listErrorPolicy) <>
      value Fatal <>
      help unmatchedPHelp
    
    overwriteP = option (Left <$> auto <|> Right <$> auto) $
      long "overwrite" <>
      short 'w' <>
      completer (listCompleter $ listOverwrite ++ listErrorPolicy) <>
      value (Left Fatal) <>
      help overwritePHelp

    ruleP = Rule <$> regexP <*> outputP <*> destP
    regexP = argument readRegex $ metavar "Regex"
    outputP = first LT.pack . parseTemplate <$> strArgument (metavar "OutFormat")
    destP = first LT.pack . parseTemplate <$> strArgument (metavar "DestFilenameFormat")

overwritePHelp :: String
overwritePHelp =
  "What to do when a destination file already exists"

unmatchedPHelp :: String
unmatchedPHelp =
  "What to do with an input line not matched with any of rules"

newtype Fail a = Fail { runFail :: Either String a }
  deriving (Functor, Applicative, Monad) via (Either String)

instance MonadFail Fail where
  fail = Fail . Left

readRegex :: ReadM Regex
readRegex = eitherReader (runFail . makeRegexM)
