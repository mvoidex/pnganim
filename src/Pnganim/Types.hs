module Pnganim.Types (
	-- * Slice
	Slice(..),
	sliceWidth, sliceHeight, emptySlice, pixelSlice, sliceNull, slice, unionSlice
	) where

import Prelude.Unicode

import Data.Monoid

-- | Slice of image
data Slice = Slice {
	sliceLeft :: Int,
	sliceRight :: Int,
	sliceTop :: Int,
	sliceBottom :: Int }

-- | Width of slice
sliceWidth :: Slice → Int
sliceWidth s = sliceRight s - sliceLeft s

-- | Height of slice
sliceHeight :: Slice → Int
sliceHeight s = sliceBottom s - sliceTop s

-- | Empty slice
emptySlice :: Slice
emptySlice = Slice 0 0 0 0

-- | Slice of one pixel
pixelSlice :: Int → Int → Slice
pixelSlice x y = slice x y (succ x) (succ y)

-- | Is slice null
sliceNull :: Slice → Bool
sliceNull s = sliceWidth s ≡ 0 ∨ sliceHeight s ≡ 0

-- | Make slice
slice :: Int → Int → Int → Int → Slice
slice l r t b = if sliceNull s then emptySlice else s where
	s = Slice (min l r) (max l r) (min t b) (max t b)

-- | Union slices
unionSlice :: Slice → Slice → Slice
unionSlice l r = slice
	((min `both` sliceLeft) l r)
	((max `both` sliceRight) l r)
	((min `both` sliceTop) l r)
	((max `both` sliceBottom) l r)
	where
		both :: (b → b → b) → (a → b) → a → a → b
		both f g x y = f (g x) (g y)

instance Monoid Slice where
	mempty = emptySlice
	mappend = unionSlice
