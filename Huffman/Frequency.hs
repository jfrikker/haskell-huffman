{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Huffman.Frequency (
  FrequencyTable,
  Stream(..),
  empty,
  fromFold,
  fromStream,
  fromPrintableChars,
  incrementFromFold,
  incrementFromStream
) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified System.IO as IO

type FrequencyTable a = M.Map a Int

class (Monad m) => Stream s v m | s -> v, s -> m where
  foldM' :: (a -> v -> a) -> a -> s -> m a

instance Stream IO.Handle Char IO where
  foldM' f v h = do
    done <- IO.hIsEOF h
    if done
      then return v
      else do
        c <- IO.hGetChar h
        let v' = f v c
        v' `seq` foldM' f v' h

empty :: FrequencyTable v
empty = M.empty

fromFold :: (F.Foldable f, Ord v) => f v -> FrequencyTable v
fromFold f = incrementFromFold f empty

fromStream :: (Stream s v m, Ord v) => s -> m (FrequencyTable v)
fromStream s = incrementFromStream s empty

printableChars :: String
printableChars = "\t\r\n" ++ [' ' .. '~']

fromPrintableChars :: FrequencyTable Char
fromPrintableChars = fromFold printableChars

insertWithIncrement :: (Ord k, Num v) => k -> M.Map k v -> M.Map k v
insertWithIncrement k = M.insertWith (+) k 1

incrementFromFold :: (F.Foldable f, Ord v) => f v -> FrequencyTable v -> FrequencyTable v
incrementFromFold f t = F.foldl (flip insertWithIncrement) t f

incrementFromStream :: (Stream s v m, Ord v) => s -> FrequencyTable v -> m (FrequencyTable v)
incrementFromStream f t = foldM' (flip insertWithIncrement) t f
