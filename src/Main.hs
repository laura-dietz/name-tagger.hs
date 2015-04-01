{-# LANGUAGE DeriveFunctor #-}

module Main where

import NameTagger.SuffixTree as ST
import NameTagger.Matcher

import Data.Tuple
import Data.Maybe (mapMaybe)
import Control.Applicative
import Control.Monad (forever)
import Data.Char (isSpace)
import Data.Foldable
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

readDict :: FilePath -> IO (SuffixTree Char Text)
readDict file = parseDict <$> T.readFile file

-- [char] \t id
parseDict :: Text -> SuffixTree Char Text
parseDict input =
    let textsWithValues = map (swap . dropFirst . T.break (=='\t')) (T.lines input)
        dropFirst (a,b) = (a, T.unpack $ T.tail b)
    in build textsWithValues

normalize :: Text -> Text
normalize = T.map (\c->if isSpace c then ' ' else c)

main = do
--     let suffix = build [("Laura",1), ("Laben",0), ("ben",-1)]
    dict <- readDict "./data/names.txt"
    print dict
    forever $ do
        line <- T.getLine
        print $ seqMatches (skipping (== ' ') $ treeMatcher dict) (T.unpack line++" ")
    return ()

testMatcher :: (Show v, Show c) => Matcher c v -> [c] -> String
testMatcher = go
  where
    go _ [] = "Failed: needs more input"
    go m (c:rest)
      | Left m' <- res = go m' rest
      | Right ret <- res = "Matched: "++show ret
      where res = runMatcher m c