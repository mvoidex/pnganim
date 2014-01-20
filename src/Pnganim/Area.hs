module Pnganim.Area (
	areas,

	-- * Internal
	AreaState(..), AreaM, runArea, newArea, unionArea
	) where

import Prelude.Unicode

import Control.Applicative
import Control.Monad.State
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Vector.Storable as V
import Data.Monoid.Unicode
import Codec.Picture

import Pnganim.Types

-- | Get connected areas of image: find areas with connected non-null pixels
areas :: Image Pixel8 → [Slice]
areas img = runArea $ forM_ [0 .. h - 1] $ \y → forM_ [0 .. w - 1] $ \x → case pixelAt img x y of
	0 → modify $ \ss → ss { areasMarks = V.snoc (areasMarks ss) 0 }
	_ → do
		i ← near x y
		case i of
			[] → do
				idx ← newArea (pixelSlice x y)
				modify $ \ss → ss { areasMarks = V.snoc (areasMarks ss) idx }
			[i'] → do
				idx ← unionArea i' (pixelSlice x y)
				modify $ \ss → ss { areasMarks = V.snoc (areasMarks ss) idx }
			(i':is) → do
				mergeAreas i' is
				idx ← unionArea i' (pixelSlice x y)
				modify $ \ss → ss { areasMarks = V.snoc (areasMarks ss) idx }
	where
		near :: Int → Int → AreaM [Int]
		near x y = nub ∘ catMaybes <$> mapM getMark [
			(pred x, y),
			(x, pred y),
			(pred x, pred y)]

		getMark :: (Int, Int) → AreaM (Maybe Int)
		getMark (x, y)
			| x ≥ 0 ∧ x < w ∧ y ≥ 0 ∧ y < h = do
				ms ← gets areasMarks
				return $ do
					ix ← ms V.!? (y * w + x)
					if ix ≡ 0 then Nothing else return ix
			| otherwise = return Nothing

		w = imageWidth img
		h = imageHeight img

-- | Area monad state
data AreaState = AreaState {
	areasMarks :: V.Vector Int,
	areasCount :: Int,
	sliceMap :: Map Int Slice }

-- | Area monad
type AreaM a = State AreaState a

-- | Run area monad
runArea :: AreaM a → [Slice]
runArea act = M.elems $ sliceMap $ execState act (AreaState V.empty 0 M.empty)

-- | Make new slice and return its id
newArea :: Slice → AreaM Int
newArea s = do
	i ← gets areasCount
	modify $ \ss → ss {
		areasCount = succ i,
		sliceMap = M.insert (succ i) s (sliceMap ss) }
	return $ succ i

-- | Add slice to existing one by id
unionArea :: Int → Slice → AreaM Int
unionArea i s = do
	modify $ \ss → ss {
		sliceMap = M.alter (fmap (⊕ s)) i (sliceMap ss) }
	return i

mergeAreas :: Int → [Int] → AreaM ()
mergeAreas i is = forM_ is $ \i' → modify $ \ss → ss {
	areasMarks = V.map (\v → if v ≡ i' then i else v) (areasMarks ss),
	sliceMap = fromMaybe (sliceMap ss) $ do
		islice ← M.lookup i' (sliceMap ss)
		return $ M.alter (fmap (⊕ islice)) i $ M.delete i' (sliceMap ss) }
