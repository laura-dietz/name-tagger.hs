module Main where

import NameTagger.SuffixTree

main = do
    let suffix = build [("Laura",1), ("Laben",0), ("ben",-1)]
    print suffix
    return ()


