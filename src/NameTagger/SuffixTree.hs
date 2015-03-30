module NameTagger.SuffixTree where


import Prelude hiding (lookup)
import qualified Data.Map.Strict as M
import Data.Monoid

data SuffixTree char value
    = SuffixTree (M.Map char (SuffixTree char value)) (Maybe value)
    deriving Show

instance (Ord char, Show value, Eq value) => Monoid (SuffixTree char value)  where
    mempty = empty
    mappend = merge defaultReduceValue

lookup :: Ord char => SuffixTree char value -> [char] -> Maybe value
lookup (SuffixTree children _) (ch:text) =
    case M.lookup ch children of
        Nothing -> Nothing
        Just node -> lookup node text
lookup (SuffixTree _ nodevalue) [] =
    nodevalue

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
merge reducevalue (SuffixTree chmap1 value1) (SuffixTree chmap2 value2)  =
    let childmap = M.unionWith (merge reducevalue) chmap1 chmap2
        nodevalue =
            case (value1, value2) of
                (Nothing, Nothing) -> Nothing
                (Just v1, Nothing) -> Just v1
                (Nothing, Just v2) -> Just v2
                (Just v1, Just v2) -> Just (reducevalue v1 v2)
    in SuffixTree childmap nodevalue

defaultReduceValue v1 v2 = if v1==v2 then v1 else error ("Need to merge values that are not the same ("++show v1++" and "++show v2++")")

build :: (Ord char, Show value, Eq value) => [([char], value)] -> SuffixTree char value
build textsWithValues =
    let listofsingles = map (uncurry singleton) textsWithValues
        reducer = merge defaultReduceValue
    in foldl reducer empty listofsingles


