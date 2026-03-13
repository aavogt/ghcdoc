{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Search.LensExtra where

import Control.Lens
import Data.Either

-- | 'adjoin' except include indices
iadjoinish ::
  AnIndexedTraversal' i s a ->
  AnIndexedTraversal' j s b ->
  Lens' s [Either (i, a) (j, b)]
iadjoinish l1 l2 f x =
  f
    ( (x ^@.. cloneIndexedTraversal l1 <&> Left)
        <> (x ^@.. cloneIndexedTraversal l2 <&> Right)
    )
    <&> \ys ->
      x
        & partsOf (cloneIndexedTraversal l1) .~ map snd (lefts ys)
        & partsOf (cloneIndexedTraversal l2) .~ map snd (rights ys)

-- vaguely Data.Profunctor.Strong.first' :: p a b -> p (a, c) (b, c)
collectSnd :: Iso' (Either (a, x) (b, x)) (Either a b, x)
collectSnd =
  iso
    ( \case
        Left (a, x) -> (Left a, x)
        Right (b, x) -> (Right b, x)
    )
    ( \case
        (Left a, x) -> Left (a, x)
        (Right b, x) -> Right (b, x)
    )
