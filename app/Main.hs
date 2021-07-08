{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Data.List.NonEmpty (groupWith, NonEmpty ((:|)))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.List (sortBy)

import System.IO
import System.Exit (die)
import System.Directory

import Option
import Template (applyTemplate)
import Data.Traversable (for)
import Text.Regex.Base
import Data.Char (isSpace)

main :: IO ()
main = do
  opt <- parseOption
  inputs <- LT.lines <$> LT.getContents
  scanlM'_ (body opt) Set.empty . chunk 1000 $ zip [1..] inputs

scanlM'_ :: Monad m => (s -> a -> m s) -> s -> [a] -> m ()
scanlM'_ step = go
  where
    go _  [] = return ()
    go !s (a:as) = step s a >>= \s' -> go s' as

body :: Option -> Set FilePath -> [(Int, LT.Text)] -> IO (Set FilePath)
body opt touchedFiles inputs =
  do outputs <- traverse (convertLine opt) inputs
     let outputs' = groupWith fst $ sortBy (comparing fst) (concat outputs)
     destFiless <- for outputs' $ \((destFile, t) :| rest) ->
       withValidatedFile opt touchedFiles destFile $ \h ->
         do LT.hPutStr h $ LT.unlines (t : map snd rest)
            return [destFile]
     return $ Set.union touchedFiles (Set.fromList (concat destFiless))

withValidatedFile :: Monoid r => Option -> Set FilePath -> FilePath -> (Handle -> IO r) -> IO r
withValidatedFile opt touchedFiles destFile k
  | Set.member destFile touchedFiles = withFile destFile AppendMode k
  | otherwise = do
      exists <- doesFileExist destFile
      if exists
        then case optionOverwrite opt of
               Left Fatal     -> die errMsg
               Left ReportAll -> hPutStrLn stderr errMsg >> return mempty
               Left Ignore    -> return mempty
               Right Overwrite -> withFile destFile WriteMode k
               Right Append    -> withFile destFile AppendMode k
        else withFile destFile WriteMode k
  where
    errMsg = "Destination file already exists:" ++ destFile

convertLine :: Option -> (Int, LT.Text) -> IO [(FilePath, LT.Text)]
convertLine opt (lineNo, line) = go (optionRules opt)
  where
    errMsg = "Unmatchable line at " ++ show lineNo ++ ":" ++ LT.unpack line
    go [] = case optionUnmatched opt of
      Fatal -> die errMsg
      ReportAll -> hPutStrLn stderr errMsg >> return []
      Ignore -> return []
    go (Rule p outF destF : rest) =
      case matchOnceText p line of
        Nothing -> go rest
        Just (pre, context, post) ->
          if LT.all isSpace (pre <> post)
            then let result = (,) <$> applyTemplate destF context <*> applyTemplate outF context
                 in case result of
                      Left errors -> die (unlines errors)
                      Right (dest, out) -> return [(LT.unpack dest, out)]
            else go rest

chunk :: Int -> [a] -> [[a]]
chunk n = go where
  go [] = []
  go as = case splitAt n as of
    (ch, rest) -> ch : go rest
