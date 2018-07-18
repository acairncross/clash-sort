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

swap :: Vec 2 a -> Vec 2 a
swap (x :> y :> Nil) = y :> x :> Nil

par
  :: (KnownNat n, KnownNat m)
  => (Vec n a -> Vec n a)
  -> (Vec m a -> Vec m a)
  -> Vec (n+m) a
  -> Vec (n+m) a
par f g = uncurry (++) . bimap f g . splitAtI

shufflePattern :: DNat n -> Vec n (Index n)
shufflePattern (Dbl (Dbl n)) =
  let (lo,  hi ) = splitAt (dnatToSnat $ Dbl n) indicesI
      (lo', hi') = splitAt (dnatToSnat $ Dbl n) $ Clash.merge lo hi
  in  lo' ++ (concat $ map swap $ unconcatI hi')

scatter :: (Enum i, KnownNat n, KnownNat m) => Vec m i -> Vec (m+k) a -> Vec n a
scatter = Clash.scatter (lazyV undefined)

gather :: (Enum i, KnownNat n) => Vec m i -> Vec n a -> Vec m a
gather = flip Clash.gather

-- | Sorts a vector of size 2^k, k > 0
--
-- prop> toList (bitonic two     xs) == sort (toList xs)
-- prop> toList (bitonic sixteen xs) == sort (toList xs)
bitonic :: Ord a => DNat n -> Vec n a -> Vec n a
bitonic (Dbl One) = minmax
bitonic (Dbl n)   = merge (Dbl n) . par (bitonic n) (bitonic n)

merge :: Ord a => DNat n -> Vec n a -> Vec n a
merge (Dbl One) = minmax
merge (Dbl n)
  = concat . map minmax . unconcatI
  . gather (shufflePattern (Dbl n))
  . par (merge n) (merge n)
  . scatter (shufflePattern (Dbl n))
