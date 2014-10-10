{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s fn =
  (\dict -> ncString <$> (listh . S.elems $ S.intersection (wordset dict) (candidates s))) <$> (readFile fn)
  where
    wordset dict' = S.fromDistinctAscList . hlist $ NoCaseString <$> lines dict'
    candidates s' = S.fromList . hlist $ NoCaseString <$> (flatten $ inits <$> permutations s')
    inits (x:.xs) = (x:.Nil) :. ((x :.) <$> (inits xs))
    inits _ = Nil

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
