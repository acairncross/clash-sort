{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Clash.Sized.Vector.Sort where

import           Clash.Prelude  hiding (gather, merge, scatter)
import qualified Clash.Prelude  as Clash
import           Data.Bifunctor (bimap)

-- $setup
-- >>> :set -XDataKinds
-- >>> import Data.List (sort)
-- >>> let two     = Dbl One
-- >>> let sixteen = Dbl $ Dbl $ Dbl $ Dbl One

data DNat (n :: Nat) where
  One :: DNat 1
  Dbl :: (KnownNat n, KnownNat m, m ~ (2*n)) => DNat n -> DNat m

instance Show (DNat n) where
  show One     = "One"
  show (Dbl p) = "Dbl (" <> show p <> ")"

dnatToSnat :: DNat n -> SNat n
dnatToSnat One     = SNat
dnatToSnat (Dbl n) = SNat

minmax :: Ord a => Vec 2 a -> Vec 2 a
minmax (x :> y :> Nil) = if x <= y then x :> y :> Nil else y :> x :> Nil

par
  :: KnownNat n
  => (Vec n a -> Vec n a)
  -> (Vec n a -> Vec n a)
  -> Vec (2*n) a
  -> Vec (2*n) a
par f g = uncurry (++) . bimap f g . splitAtI

shufflePattern :: DNat n -> Vec n (Index n)
shufflePattern (Dbl n) =
  let (lo, hi ) = splitAt (dnatToSnat n) indicesI
  in  Clash.merge lo hi

shuffle :: DNat n -> Vec n a -> Vec n a
shuffle (Dbl n) = scatter (shufflePattern (Dbl n))
  where
    scatter = Clash.scatter (lazyV undefined)

unshuffle :: DNat n -> Vec n a -> Vec n a
unshuffle (Dbl n) = gather (shufflePattern (Dbl n))
  where
    gather = flip Clash.gather

-- | Sorts a vector of size 2^k, k > 0
--
-- prop> toList (bitonic two     xs) == sort (toList xs)
-- prop> toList (bitonic sixteen xs) == sort (toList xs)
bitonic :: Ord a => DNat n -> Vec n a -> Vec n a
bitonic (Dbl One) = minmax
bitonic (Dbl n)
  = merge (Dbl n)
  . par id reverse
  . par (bitonic n) (bitonic n)

merge :: Ord a => DNat n -> Vec n a -> Vec n a
merge (Dbl One) = minmax
merge (Dbl n)
  = concat . map minmax . unconcatI
  . unshuffle (Dbl n)
  . par (merge n) (merge n)
  . shuffle (Dbl n)
