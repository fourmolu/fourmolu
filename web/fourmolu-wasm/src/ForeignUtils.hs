{-# LANGUAGE AllowAmbiguousTypes #-}

module ForeignUtils
  ( sizeFor,
    alignmentFor,
    nextPtr,
  )
where

import Foreign

sizeFor :: forall a. (Storable a) => Int
sizeFor = sizeOf (undefined :: a)

alignmentFor :: forall a. (Storable a) => Int
alignmentFor = alignment (undefined :: a)

-- | Get a pointer to the address after the given pointer.
nextPtr :: forall a b. (Storable a, Storable b) => Ptr a -> Ptr b
nextPtr ptr = alignPtr (plusPtr ptr (sizeFor @a)) (alignmentFor @b)
