module NameTagger.SuffixTree where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Applicative hiding (empty)

data SuffixTree char value
    = SuffixTree { suffixes :: !(M.Map char (SuffixTree char value))
                 , terminus :: !(Maybe value)
                 }
    deriving (Show)

instance Ord char => Functor (SuffixTree char) where
    fmap f (SuffixTree suffixes terminus) = SuffixTree (fmap (fmap f) suffixes) (fmap f terminus)

instance (Ord char, Monoid value) => Monoid (SuffixTree char value)  where
    mempty = empty
    mappend = merge mappend

lookupSeq :: Ord char => SuffixTree char value -> [char] -> Maybe value
lookupSeq (SuffixTree children _) (ch:text) =
    case M.lookup ch children of
        Nothing -> Nothing
        Just node -> lookupSeq node text
lookupSeq (SuffixTree _ nodevalue) [] =
    nodevalue

lookup :: Ord char => SuffixTree char value -> char -> Maybe (SuffixTree char value)
lookup (SuffixTree children _) ch = M.lookup ch children

empty :: SuffixTree char value
empty = SuffixTree M.empty Nothing

singleton :: [char] -> value -> SuffixTree char value
singleton (ch:text) termvalue =
    SuffixTree (M.singleton ch (singleton text termvalue) ) Nothing
singleton [] termvalue =
    SuffixTree M.empty (Just termvalue)

merge :: Ord char
      => (value -> value ->  value)
      -> SuffixTree char value
      -> SuffixTree char value
      -> SuffixTree char value
merge reduce (SuffixTree chmap1 v1) (SuffixTree chmap2 v2)  =
    let childmap = M.unionWith (merge reduce) chmap1 chmap2
        nodevalue =
            case (value1, value2) of
                (Nothing, Nothing) -> Nothing
                (Just v1, Nothing) -> Just v1
                (Nothing, Just v2) -> Just v2
                (Just v1, Just v2) -> Just (reduce v1 v2)
    in SuffixTree childmap nodevalue

defaultReduceValue v1 v2 = if v1==v2 then v1 else error ("Need to merge values that are not the same ("++show v1++" and "++show v2++")")

build :: (Ord char, Show value, Eq value)
      => [([char], value)] -> SuffixTree char value
build textsWithValues =
    let listofsingles = map (uncurry singleton) textsWithValues
        reducer = merge defaultReduceValue
    in foldl reducer empty listofsingles


