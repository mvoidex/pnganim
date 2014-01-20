module Main (
	main
	) where

import Prelude.Unicode

import Control.Applicative
import Control.Monad.Error
import Data.List
import System.Directory
import System.Environment
import System.FilePath

import Codec.Picture

import Pnganim.Image
import Pnganim.Area

main :: IO ()
main = do
	args ← getArgs
	case args of
		[path] → do
			path' ← canonicalizePath path
			putStrLn $ "Processing directory " ++ path'
			cts ← (map (path' </>) ∘ sort ∘ filter isPng) <$> getDirectoryContents path'
			putStrLn $ "Found files: " ++ intercalate ", " cts
			void $ runErrorT $ do
				imgs ← mapM readPngAsRGB8 cts
				liftIO $ putStrLn "Images read"
				diffs ← maybe (throwError "No images") return $ diffImageList imgs
				liftIO $ do
					createDirectoryIfMissing True "diffs"
					zipWithM_ (\i d → writePng ("diffs" </> show (i :: Int) <.> "png") d) [1..] diffs
					putStrLn "Diffs computed"
					--forM_ diffs $ \d → do
					--	putStrLn "xxxxxxxxxxxxxxxxx"
					--	forM_ [0 .. imageHeight d - 1] $ \y → forM_ [0 .. imageWidth d - 1] $ \x → do
					--		putStrLn $ show x ++ ":" ++ show y ++ " = " ++ show (pixelAt d x y)

					forM_ (map areas diffs) $ \sls → do
						putStrLn "-------------------"
						mapM_ print sls
		_ -> putStrLn "Unknown command"

isPng :: FilePath → Bool
isPng f = takeExtension f ≡ ".png"
