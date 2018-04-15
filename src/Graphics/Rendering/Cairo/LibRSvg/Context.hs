{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Rendering.Cairo.LibRSvg.Context (Svg(..), Cairo, unCairo, rsvgCtx) where

import Data.Monoid (mempty, (<>))
import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import Foreign.ForeignPtr
import Graphics.Rendering.Cairo.Types

newtype Svg = Svg (ForeignPtr Svg)

rsvgCtx :: C.Context
rsvgCtx = C.baseCtx <> ctx'
  where ctx' = mempty {
    ctxTypesTable =
      [ (C.TypeName "cairo_t", [t| Cairo |])
      , (C.TypeName "RsvgHandle", [t| Svg |])
      ]
    }
