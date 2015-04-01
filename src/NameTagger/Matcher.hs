{-# LANGUAGE DeriveFunctor #-}

module NameTagger.Matcher where

import NameTagger.SuffixTree as ST
import Data.Char
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import Data.List (tails)
import qualified  Data.Text as T

data MatchResult c v = MatchFailed (SuffixTree c v)
                     | Matched v
                     deriving (Functor, Show)

isMatch :: MatchResult c v -> Maybe v
isMatch (Matched v) = Just v
isMatch _           = Nothing

newtype Matcher c v = Matcher {runMatcher :: c -> Either (Matcher c v) (MatchResult c v)}

instance Ord c => Functor (Matcher c) where
    fmap f (Matcher g) = Matcher $ \c ->
        case g c of
            Left m -> Left (fmap f m)
            Right result -> Right (fmap f result)

{-
instance Applicative Matcher where
    pure x = Matcher $ const $ Right $ Matched x
    Matcher ma <*> Matcher mb = Matcher $ \c->case (ma c <*> mb c

instance Monad Matcher where
    return = pure
    (>>=) = fallback

instance Alternative Matcher where
    Matcher ma <|> Matcher mb = Matcher $ \c ->
        case ma c of
            Right (MatchFailed _) -> ma c
            result -> result
-}

mapChar :: (c -> c) -> Matcher c v -> Matcher c v
mapChar f (Matcher m) = Matcher $ m . f

-- | Tree matcher matches against entries in a 'SuffixTree'
treeMatcher :: Ord c => SuffixTree c v -> Matcher c v
treeMatcher = Matcher . go
  where
    go tree c
      | Just next <- ST.lookup tree c = Left (Matcher $ go next)
      | otherwise = Right $ maybe (MatchFailed tree) Matched (terminus tree)

-- | @skipping canSkip m@ allows 'Matcher' @m@ to fail on characters @c@ where @canSkipc == True@
skipping :: (c -> Bool)  -- ^ Can the given character be skipped?
         -> Matcher c v
         -> Matcher c v
skipping canSkip = go
  where
    go m = Matcher $ \c -> case runMatcher m c of
                               Right (MatchFailed _) | canSkip c -> Left (go m)
                               other -> other

fallback :: Matcher c v -> (SuffixTree c v -> Matcher c v) -> Matcher c v
fallback ma mb = Matcher $ \c -> case runMatcher ma c of
                                Right (MatchFailed failPos) -> runMatcher (mb failPos) c
                                result -> result

-- | @a `orTry` b@ attempts to match @a@ and if that fails then attempts to match @b@.
orTry :: Matcher c v -> Matcher c v -> Matcher c v
Matcher ma `orTry` Matcher mb = Matcher $ \c ->
    case ma c of
        Right (MatchFailed _) -> mb c
        result -> result

-- | @caseInsensitive m@ matches m in a case-insensitive manner.
caseInsensitive :: Matcher Char v -> Matcher Char v
caseInsensitive m = mapChar toUpper m `orTry` mapChar toLower m

-- | limit the depth of a match
limited :: Int -> Matcher c v -> Matcher c v
limited = go
  where
    go 0 m = Matcher $ const $ Right $ MatchFailed undefined                       -- FIXME
    go n m = Matcher $ \c -> case runMatcher m c of
                                 Left cont -> Left $ go (n-1) cont
                                 other     -> other

-- | Run the given 'Matcher's until one returns a result
race :: [Matcher c v] -> Matcher c v
race = go
  where
    go matchers = Matcher $ \c -> case partitionEithers $ map (\m->runMatcher m c) matchers of
                                      (_, finished) | x:_ <- mapMaybe isMatch finished   -> Right $ Matched x
                                      (matchers', _) -> Left $ go matchers'

matchesString :: Matcher c v -> [c] -> [v]
matchesString m = go []
  where
    go _ [] = []
    go candidates (c:rest) =
      let (remaining, matches) = partitionEithers $ map (\m->runMatcher m c) candidates
      in mapMaybe isMatch matches ++ go (m:remaining) rest

matchesText :: Matcher Char v -> T.Text -> [v]
matchesText m = go []
  where
    go _ text | T.null text = []
    go candidates text | c <- T.head text =
      let (remaining, matches) = partitionEithers $ map (\m->runMatcher m c) candidates
      in mapMaybe isMatch matches ++ go (m:remaining) (T.tail text)

seqMatches :: Matcher c v -> [c] -> [v]
seqMatches m = concatMap (go m) . tails
  where
    go m [] = []
    go m (c:rest) = case runMatcher m c of
                    Left cont -> go cont rest
                    Right (Matched v) -> [v]
                    other -> []