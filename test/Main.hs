{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.LibRSvg
import Prelude hiding (id)

import qualified Data.ByteString as BS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "can render" $ withImageSurface FormatARGB32 28 33 $ \surf -> do
    Right svg <- fromBuffer =<< BS.readFile "test/flame.svg"
    dimensions svg `shouldReturn` (28,33)
    renderWith surf (render svg) `shouldReturn` True
    --surfaceWriteToPNG surf "flame.png"
