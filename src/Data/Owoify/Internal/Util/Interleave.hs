module Data.Owoify.Internal.Util.Interleave where

import Prelude

-- | Utility function to interleave two lists.
interleave :: [a] -> [a] -> [a]
interleave a b = go [] a b 0
  where
    go result [] other round        | even round = if not $ null other then other <> result else result
    go result (x : xs) other round  | even round = go (x : result) xs other (round + 1)
    go result _ [] _                             = result
    go result arr (x : xs) round                 = go (x : result) arr xs (round + 1)