{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.Rendering.Cairo.LibRSvg (
  Svg
, dimensions
, fromBuffer
, setDpi
, setDpiXY
, render
, renderSub
) where

import Data.ByteString (useAsCString)
import Data.ByteString.Internal
import Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackMallocCString)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.LibRSvg.Context
import Foreign.Ptr
import Foreign.ForeignPtr
import Protolude

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

C.context (rsvgCtx <> C.bsCtx <> C.fptrCtx)
C.include "string.h"
C.include "<librsvg/rsvg.h>"

render :: Svg -> Render Bool
render svg = do
  cairo <- asks unCairo
  toEnum . fromIntegral <$> liftIO [C.exp|int {
  rsvg_handle_render_cairo($fptr-ptr:(RsvgHandle *svg), $(cairo_t *cairo))
  }|]

renderSub :: Svg -> ByteString -> Render Bool
renderSub svg id = do
  cairo <- asks unCairo
  toEnum . fromIntegral <$> liftIO (useAsCString id $ \id_ -> [C.exp|int {
  rsvg_handle_render_cairo_sub($fptr-ptr:(RsvgHandle *svg), $(cairo_t *cairo), $(char *id_))
  }|])

fromBuffer :: ByteString -> IO (Either Text Svg)
fromBuffer bs = unsafeUseAsCStringLen bs $ \(buf, fromIntegral -> len) -> do
  (ptr, err) <- C.withPtrs_ $ \(p,errMsg) -> [C.block|void {
    GError *err = NULL;
    *$(RsvgHandle **p) = rsvg_handle_new_from_data($(char *buf), $(size_t len) , &err);
    if (err && err->message) {
      *$(char **errMsg) = strdup(err->message);
    }
    }|]
  if ptr==nullPtr
    then Left  . toS <$> unsafePackMallocCString err
    else Right . Svg <$> newForeignPtr freeSvg ptr

dimensions :: Svg -> IO (Int, Int)
dimensions svg = bimap fromIntegral fromIntegral <$> C.withPtrs_ (\(w,h) -> [CU.block|void {
  RsvgDimensionData dimensions;
  rsvg_handle_get_dimensions ($fptr-ptr:(RsvgHandle *svg), &dimensions);
  *$(int *w) = dimensions.width;
  *$(int *h) = dimensions.height;
  }|])

setDpi :: Svg -> Double -> IO ()
setDpi svg (realToFrac -> dpi) = [CU.exp|void {
  rsvg_handle_set_dpi ($fptr-ptr:(RsvgHandle *svg), $(double dpi));
  }|]

setDpiXY :: Svg -> Double -> Double -> IO ()
setDpiXY svg (realToFrac -> dpix) (realToFrac -> dpiy) = [CU.exp|void {
  rsvg_handle_set_dpi_x_y($fptr-ptr:(RsvgHandle *svg), $(double dpix), $(double dpiy));
  }|]

foreign import ccall "&g_object_unref" freeSvg :: FinalizerPtr Svg
