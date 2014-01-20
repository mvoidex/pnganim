module Pnganim.Image (
	-- * Convert pixels and images
	pixel16to8, imageToRGB8,

	-- * Image ops
	diffImages, diffImageList,

	-- * Loading
	decodePngAsRGB8, readPngAsRGB8
	) where

import Prelude.Unicode

import Control.Monad.Error
import Data.ByteString (ByteString)
import Codec.Picture
import Codec.Picture.Types (promotePixel, promoteImage, dropTransparency, convertPixel)

-- | Convert 'Pixedl16' to 'Pixel8'
pixel16to8 :: Pixel16 → Pixel8
pixel16to8 = fromIntegral ∘ (`div` 256)

-- | Convert dynamic image to RGB8 image
imageToRGB8 :: DynamicImage → Image PixelRGB8
imageToRGB8 (ImageY8 i) = promoteImage i
imageToRGB8 (ImageY16 i) = pixelMap (promotePixel ∘ pixel16to8) i
imageToRGB8 (ImageYF i) = pixelMap (promotePixel ∘ (round :: Float -> Pixel8) ∘ (* 255.0)) i
imageToRGB8 (ImageYA8 i) = promoteImage i
imageToRGB8 (ImageYA16 i) = pixelMap (promotePixel ∘ pixel16to8 ∘ dropTransparency) i
imageToRGB8 (ImageRGB8 i) = i
imageToRGB8 (ImageRGB16 i) = pixelMap down i where
	down (PixelRGB16 r g b) = PixelRGB8 (pixel16to8 r) (pixel16to8 g) (pixel16to8 b)
imageToRGB8 (ImageRGBA8 i) = pixelMap dropTransparency i
imageToRGB8 (ImageRGBA16 i) = pixelMap down i where
	down (PixelRGBA16 r g b _) = PixelRGB8 (pixel16to8 r) (pixel16to8 g) (pixel16to8 b)
imageToRGB8 (ImageYCbCr8 i) = pixelMap convertPixel i
imageToRGB8 (ImageCMYK16 i) = pixelMap (down ∘ convertPixel) i where
	down (PixelRGB16 r g b) = PixelRGB8 (pixel16to8 r) (pixel16to8 g) (pixel16to8 b)

-- | Subtract one image from another. Images must be of same size
diffImages :: Pixel a ⇒ Image a → Image a → Maybe (Image Pixel8)
diffImages l r
	| (imageWidth l ≡ imageWidth r) ∧ (imageHeight l ≡ imageHeight r) =
		Just $ generateImage
			(\x y → if pixelAt l x y ≢ pixelAt r x y then 255 else 0)
			(imageWidth l)
			(imageHeight l)
	| otherwise = Nothing

-- | Get diffs between images
diffImageList :: Pixel a ⇒ [Image a] → Maybe [Image Pixel8]
diffImageList [] = Nothing
diffImageList [_] = Just []
diffImageList imgs = sequence $ zipWith diffImages imgs (tail imgs)

-- | Decode png and convert image to RGB8
decodePngAsRGB8 :: ByteString → Either String (Image PixelRGB8)
decodePngAsRGB8 = fmap imageToRGB8 ∘ decodePng

-- | Read png from file and convert image to RGB8
readPngAsRGB8 :: FilePath → ErrorT String IO (Image PixelRGB8)
readPngAsRGB8 = fmap imageToRGB8 ∘ ErrorT ∘ readPng
