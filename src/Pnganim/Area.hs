module Pnganim.Area (
	areas,

	-- * Internal
	AreaState(..), AreaM, runArea, newArea, unionArea
	) where

import Prelude.Unicode

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Vector.Storable as V
import Data.Monoid.Unicode
import Data.Tuple (swap)
import Codec.Picture

import Pnganim.Types

-- | Get connected areas of image: find areas with connected non-null pixels
areas :: Image PixelRGB8 → [Slice]
areas = areas' ∘ pixelMap grey where
	grey :: PixelRGB8 → Pixel8
	grey p = fromIntegral $ fromEnum $ p ≢ PixelRGB8 0 0 0

	areas' :: Image Pixel8 → [Slice]
	areas' img = runArea $ V.foldM ((step ∘) ∘ markArea) V.empty (imageData img) where

		markArea :: V.Vector Pixel8 → Pixel8 → AreaM (V.Vector Pixel8)
		markArea indices 0 = return $ V.snoc indices 0
		markArea indices _ = do
			(x, y) ← fmap (swap ∘ (`divMod` h) ∘ fromIntegral) $ gets currentPixel
			i ← maybe newArea unionArea (near x y) (pixelSlice x y)
			return $ V.snoc indices $ fromIntegral i
			where
				near x y = listToMaybe $ mapMaybe getPixel [
					(pred x, y),
					(x, pred y),
					(pred x, pred y)]

				getPixel :: (Int, Int) → Maybe Int
				getPixel (x, y)
					| x ≥ 0 ∧ x < w ∧ y ≥ 0 ∧ y < h = fmap fromIntegral $ indices V.!? (pixelBaseIndex img x y)
					| otherwise = Nothing

		w = imageWidth img
		h = imageHeight img

	step :: AreaM a → AreaM a
	step f = do
		r ← f
		modify $ \ss → ss { currentPixel = succ (currentPixel ss) }
		return r

-- | Area monad state
data AreaState = AreaState {
	currentPixel :: Int,
	areasCount :: Int,
	sliceMap :: Map Int Slice }

-- | Area monad
type AreaM a = State AreaState a

-- | Run area monad
runArea :: AreaM a → [Slice]
runArea act = M.elems $ sliceMap $ execState act (AreaState 0 0 M.empty)

-- | Make new slice and return its id
newArea :: Slice → AreaM Int
newArea s = do
	i ← gets areasCount
	modify $ \ss → ss {
		areasCount = succ i,
		sliceMap = M.insert (succ i) s (sliceMap ss) }
	return i

-- | Add slice to existing one by id
unionArea :: Int → Slice → AreaM Int
unionArea i s = do
	modify $ \ss → ss {
		sliceMap = M.alter (fmap (⊕ s)) i (sliceMap ss) }
	return i
