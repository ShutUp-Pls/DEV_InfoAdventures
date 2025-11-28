module Graficos.Dibujado where

import qualified SDL
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

screenWidth, screenHeight :: FCT.CInt
screenWidth = 800
screenHeight = 600

screenCenter :: SDL.V2 Float
screenCenter = SDL.V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

worldToScreen :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float
worldToScreen worldPos camPos zoom = ((worldPos - camPos) LV.^* zoom) + screenCenter

dibujarTextura :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V3 DW.Word8 -> IO ()
dibujarTextura renderer texture camPos zoom worldPos size angle (SDL.V3 r g b) = do
    -- Aplicamos zoom a la posición
    let screenPos = worldToScreen worldPos camPos zoom
    let SDL.V2 x y = screenPos
    
    -- Aplicamos zoom al tamaño (ancho y alto)
    let SDL.V2 w h = size LV.^* zoom 
    
    let rect = SDL.Rectangle
            (SDL.P (SDL.V2 (round x) (round y)))
            (SDL.V2 (round w) (round h))
    let angleDouble = realToFrac angle :: FCT.CDouble

    SDL.textureColorMod texture SDL.$= SDL.V3 r g b
    SDL.copyEx renderer 
               texture 
               Nothing 
               (Just rect) 
               angleDouble 
               Nothing 
               (SDL.V2 False False)

dibujarLinea :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V3 DW.Word8 -> IO ()
dibujarLinea renderer texture camPos zoom start end grosor color = do
    let diff = end - start
    let (SDL.V2 dx dy) = diff
    
    let longitud = LM.norm diff
    let anguloRadianes = atan2 dy dx
    let anguloGrados = anguloRadianes * 180 / pi
    
    let size = SDL.V2 longitud grosor
    let center = (start + end) LV.^* 0.5
    let pos = center - (size LV.^* 0.5)

    -- Pasamos el zoom a dibujarTextura
    dibujarTextura renderer texture camPos zoom pos size anguloGrados color