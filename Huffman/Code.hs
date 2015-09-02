module Huffman.Code (
  Code(..),
  buildCode,
  toMapping
) where

import qualified Data.Map as M
import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding(..))
import qualified Huffman.Frequency as Freq

data Code a = Leaf a | Branch (Code a) (Code a) deriving (Eq, Ord, Show)

toPSQueue :: Ord v => Freq.FrequencyTable v -> PQ.PSQ v Int
toPSQueue = PQ.fromList . map (uncurry (:->)) . M.toList

buildCode :: (Ord v) => Freq.FrequencyTable v -> Maybe (Code v)
buildCode = combineCodes . toPSQueue . M.mapKeys Leaf

combineCodes :: (Ord v) => PQ.PSQ (Code v) Int -> Maybe (Code v)
combineCodes = combinePairs combineBinding
  where combineBinding (c1 :-> p1) (c2 :-> p2) = Branch c1 c2 :-> (p1 + p2)

combinePairs :: (Ord p, Ord v) => (PQ.Binding v p -> PQ.Binding v p -> PQ.Binding v p) -> PQ.PSQ v p -> Maybe v
combinePairs f pq = case PQ.minView pq of
  Nothing -> Nothing
  Just (min1, pq') -> case PQ.minView pq' of
      Nothing -> Just $ PQ.key min1
      Just (min2, pq'') -> let (v :-> p) = f min1 min2 in
        combinePairs f $ PQ.insert v p pq''

toMapping :: Code a -> M.Map String a
toMapping (Leaf val) = M.singleton "" val
toMapping (Branch l r) = M.union (prepend '0' $ toMapping l) (prepend '1' $ toMapping r)
  where prepend c = M.mapKeys ((:) c)
