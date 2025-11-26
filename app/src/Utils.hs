module Utils where

-- Módulos del sistema
import qualified SDL
import qualified Foreign.C.Types as FCT

-- Helper: Convertir coordenadas de Float a CInt
toSDLRect :: SDL.V2 Float -> SDL.V2 Float -> SDL.Rectangle FCT.CInt
toSDLRect (SDL.V2 x y) (SDL.V2 w h) = 
    SDL.Rectangle (SDL.P (SDL.V2 (round x) (round y))) (SDL.V2 (round w) (round h))