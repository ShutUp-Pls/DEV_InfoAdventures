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
import qualified Globals.Types      as GType

type ParteArma = (SDL.V2 Float, SDL.V2 Float, SDL.V4 DW.Word8)

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

dibujarBoxColor :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Box -> SDL.V4 DW.Word8 -> IO ()
dibujarBoxColor renderer texture camPos zoom box color = do
    let pos = box LMi.^. GType.boxPos
    let tam = box LMi.^. GType.boxTam
    let ang = box LMi.^. GType.boxAng
    dibujarTextura renderer texture camPos zoom pos tam ang color

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

dibujarOverlay :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> SDL.V2 Float -> SDL.V4 DW.Word8 -> IO ()
dibujarOverlay renderer texture screenPos size color = do
    dibujarTextura renderer texture screenCenter 1.0 screenPos size 0 color

dibujarGlock :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> IO ()
dibujarGlock renderer texture camPos zoom basePos tam _ang = do
    let SDL.V2 ancho _ = tam
        escalaBase     = if ancho <= 0 then 1 else ancho / 70.0
        esc f          = f * escalaBase

    let colSlide    = SDL.V4 40 40 40 255
        colFrame    = SDL.V4 20 20 20 255 
        colDetalle  = SDL.V4  8  8  8 255 

    let partes :: [ParteArma]
        partes = [
            (SDL.V2 0 0,   SDL.V2 70 11, colSlide),
            (SDL.V2 8 11,  SDL.V2 52 10, colFrame),
            (SDL.V2 43 20, SDL.V2 14 26, colFrame),
            (SDL.V2 22 16, SDL.V2 16 9,  colFrame)
            ]

    mapM_ (\(offsetLocal, sizeLocal, color) -> do
            let finalPos  = basePos + (offsetLocal LV.^* escalaBase)
            let finalSize = sizeLocal LV.^* escalaBase
            dibujarTextura renderer texture camPos zoom finalPos finalSize _ang color
        ) partes

    let triggerStart = basePos + SDL.V2 (esc 28) (esc 19)
    let triggerEnd   = triggerStart + SDL.V2 0 (esc 6)
    dibujarLinea renderer texture camPos zoom triggerStart triggerEnd (esc 2) colDetalle

    let sepStart = basePos + SDL.V2 (esc 3)  (esc 11)
    let sepEnd   = basePos + SDL.V2 (esc 67) (esc 11)
    dibujarLinea renderer texture camPos zoom sepStart sepEnd 1 colDetalle

dibujarEscopeta :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> IO ()
dibujarEscopeta renderer texture camPos zoom basePos tam _ang = do
    let SDL.V2 ancho _ = tam
        escalaBase     = if ancho <= 0 then 1 else ancho / 180.0

    let colMetalOscuro = SDL.V4 30 30 30 255
        colMetalClaro  = SDL.V4 60 60 60 255
        colMadera      = SDL.V4 101 67 33 255

    let partes :: [ParteArma]
        partes = [
            (SDL.V2 0 10,    SDL.V2 40 25, colMadera),
            (SDL.V2 40 5,    SDL.V2 30 20, colMetalOscuro),
            (SDL.V2 70 5,    SDL.V2 110 8, colMetalClaro),
            (SDL.V2 70 15,   SDL.V2 100 8, colMetalOscuro),
            (SDL.V2 90 14,   SDL.V2 40 10, colMadera),
            (SDL.V2 50 25,   SDL.V2 12 8,  colMetalOscuro)
            ]

    mapM_ (\(offsetLocal, sizeLocal, color) -> do
            let finalPos  = basePos + (offsetLocal LV.^* escalaBase)
            let finalSize = sizeLocal LV.^* escalaBase
            dibujarTextura renderer texture camPos zoom finalPos finalSize _ang color
        ) partes

dibujarFusilM52 :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> IO ()
dibujarFusilM52 renderer texture camPos zoom basePos tam _ang = do
    let SDL.V2 ancho _ = tam
        escalaBase     = if ancho <= 0 then 1 else ancho / 220.0

    let colCuerpo   = SDL.V4 50 50 55 255
        colStock    = SDL.V4 35 35 35 255
        colGrip     = SDL.V4 30 30 30 255
        colMag      = SDL.V4 20 20 20 255
        colMira     = SDL.V4 10 10 10 255
        colHandguard= SDL.V4 60 65 60 255

    let partes :: [ParteArma]
        partes = [
            (SDL.V2 0 5,     SDL.V2 50 20, colStock),
            (SDL.V2 50 0,    SDL.V2 90 28, colCuerpo),
            (SDL.V2 55 28,   SDL.V2 18 25, colGrip),
            (SDL.V2 95 28,   SDL.V2 25 40, colMag),
            (SDL.V2 140 2,   SDL.V2 70 24, colHandguard),
            (SDL.V2 210 10,  SDL.V2 15 6,  colCuerpo),
            (SDL.V2 70 (-8), SDL.V2 40 8,  colMira),
            (SDL.V2 75 0,    SDL.V2 10 5,  colMira)
            ]

    mapM_ (\(offsetLocal, sizeLocal, color) -> do
            let finalPos  = basePos + (offsetLocal LV.^* escalaBase)
            let finalSize = sizeLocal LV.^* escalaBase
            dibujarTextura renderer texture camPos zoom finalPos finalSize _ang color
        ) partes

dibujarLanzallamas :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> IO ()
dibujarLanzallamas renderer texture camPos zoom basePos tam _ang = do
    let SDL.V2 ancho _ = tam
        escalaBase     = if ancho <= 0 then 1 else ancho / 200.0

    let colTanque   = SDL.V4 180 40 40 255
        colMetal    = SDL.V4 100 100 100 255
        colBoquilla = SDL.V4 40 30 30 255
        colTuberia  = SDL.V4 160 140 40 255 
        colLlama    = SDL.V4 255 100 0 255

    let partes :: [ParteArma]
        partes = [
            (SDL.V2 40 30,   SDL.V2 100 35, colTanque),
            (SDL.V2 20 10,   SDL.V2 140 20, colMetal),
            (SDL.V2 0 10,    SDL.V2 20 40,  colMetal),
            (SDL.V2 100 65,  SDL.V2 10 15,  colTuberia),
            (SDL.V2 60 25,   SDL.V2 10 10,  colTuberia),
            (SDL.V2 160 5,   SDL.V2 30 30,  colBoquilla),
            (SDL.V2 185 15,  SDL.V2 5 5,    colLlama)
            ]

    mapM_ (\(offsetLocal, sizeLocal, color) -> do
            let finalPos  = basePos + (offsetLocal LV.^* escalaBase)
            let finalSize = sizeLocal LV.^* escalaBase
            dibujarTextura renderer texture camPos zoom finalPos finalSize _ang color
        ) partes