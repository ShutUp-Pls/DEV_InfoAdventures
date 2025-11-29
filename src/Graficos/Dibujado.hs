module Graficos.Dibujado where
-- Módulos del sistema
import qualified SDL
import qualified SDL.Font           as Font
import qualified Data.Word          as DW
import qualified Foreign.C.Types    as FCT
import qualified Linear.Metric      as LMe
import qualified Linear.Vector      as LV
import qualified Lens.Micro         as LMi
import qualified Data.Text          as DT
-- Módulos propios
import qualified Globals.Types as GType

screenWidth, screenHeight :: FCT.CInt
screenWidth = 800
screenHeight = 600

screenCenter :: SDL.V2 Float
screenCenter = SDL.V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

worldToScreen :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float
worldToScreen worldPos camPos zoom = ((worldPos - camPos) LV.^* zoom) + screenCenter

dibujarTextura :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V4 DW.Word8 -> IO ()
dibujarTextura renderer texture camPos zoom worldPos size angle (SDL.V4 r g b a) = do
    let screenPos = worldToScreen worldPos camPos zoom
    let SDL.V2 x y = screenPos
    let SDL.V2 w h = size LV.^* zoom 
    
    let rect = SDL.Rectangle
            (SDL.P (SDL.V2 (round x) (round y)))
            (SDL.V2 (round w) (round h))
    let angleDouble = realToFrac angle :: FCT.CDouble

    SDL.textureColorMod texture SDL.$= SDL.V3 r g b
    SDL.textureAlphaMod texture SDL.$= a
    SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend

    SDL.copyEx renderer 
               texture 
               Nothing 
               (Just rect) 
               angleDouble 
               Nothing 
               (SDL.V2 False False)

    SDL.textureAlphaMod texture SDL.$= 255
    SDL.textureBlendMode texture SDL.$= SDL.BlendNone

dibujarLinea :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V4 DW.Word8 -> IO ()
dibujarLinea renderer texture camPos zoom start end grosor color = do
    let diff = end - start
    let (SDL.V2 dx dy) = diff
    
    let longitud = LMe.norm diff
    let anguloRadianes = atan2 dy dx
    let anguloGrados = anguloRadianes * 180 / pi
    
    let size = SDL.V2 longitud grosor
    let center = (start + end) LV.^* 0.5
    let pos = center - (size LV.^* 0.5)

    dibujarTextura renderer texture camPos zoom pos size anguloGrados color

dibujarBox :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Box -> IO ()
dibujarBox renderer texture camPos zoom box = do
    let pos = box LMi.^. GType.boxPos
    let tam = box LMi.^. GType.boxTam
    let ang = box LMi.^. GType.boxAng

    dibujarTextura renderer texture camPos zoom pos tam ang (SDL.V4 255 255 255 255)

dibujarTexto :: SDL.Renderer -> Font.Font -> DT.Text -> SDL.V2 FCT.CInt -> SDL.V4 DW.Word8 -> IO ()
dibujarTexto renderer font text (SDL.V2 x y) color = do
    surface <- Font.solid font color text 
    texture <- SDL.createTextureFromSurface renderer surface
    info    <- SDL.queryTexture texture
    SDL.freeSurface surface
    
    let w = SDL.textureWidth info
    let h = SDL.textureHeight info
    let rect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)
    
    SDL.copy renderer texture Nothing (Just rect)
    SDL.destroyTexture texture

dibujarTextoCentrado :: SDL.Renderer -> Font.Font -> DT.Text -> SDL.V2 FCT.CInt -> SDL.V4 DW.Word8 -> IO ()
dibujarTextoCentrado renderer font text (SDL.V2 x y) color = do
    surface <- Font.blended font color text
    texture <- SDL.createTextureFromSurface renderer surface
    info    <- SDL.queryTexture texture
    SDL.freeSurface surface
    
    let w = SDL.textureWidth info
    let h = SDL.textureHeight info
    let rect = SDL.Rectangle (SDL.P (SDL.V2 (x - w `div` 2) (y - h `div` 2))) 
                             (SDL.V2 w h)
    
    SDL.copy renderer texture Nothing (Just rect)
    SDL.destroyTexture texture