{-# LANGUAGE OverloadedStrings #-}
module Graficos.Dibujado where

-- Modulos del sistema
import qualified SDL
import qualified SDL.Font as Font
import qualified Foreign.C.Types as FCT
import qualified Data.Vector.Storable as DVS
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Objetos.Cono as OC

-- Configuración de Pantalla
screenWidth, screenHeight :: FCT.CInt
screenWidth = 800
screenHeight = 600

-- Calculamos el centrol
screenCenter :: SDL.V2 Float
screenCenter = SDL.V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

-- Convierte coordenadas del mundo a coordenadas de pantalla
worldToScreen :: SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float
worldToScreen worldPos camPos = worldPos - camPos + screenCenter

dibujarEntidadRotada :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V3 DW.Word8 -> IO ()
dibujarEntidadRotada renderer texture camPos pos size angle (SDL.V3 r g b) = do
    let screenPos = worldToScreen pos camPos
    let rect = Utils.toSDLRect screenPos size
    let angleDouble = realToFrac angle :: FCT.CDouble

    SDL.textureColorMod texture SDL.$= SDL.V3 r g b
    SDL.copyEx renderer 
               texture 
               Nothing 
               (Just rect) 
               angleDouble 
               Nothing 
               (SDL.V2 False False)

-- HUD
dibujarHUD :: SDL.Renderer -> Font.Font -> Float -> Float -> IO ()
dibujarHUD renderer font vida velocidad = do
    let color = SDL.V4 255 255 255 255

    let renderizarTexto texto x y = do
            surface <- Font.solid font color texto
            texture <- SDL.createTextureFromSurface renderer surface
            SDL.freeSurface surface
            
            ti <- SDL.queryTexture texture
            let w = SDL.textureWidth ti
            let h = SDL.textureHeight ti
            
            let rect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)
            SDL.copy renderer texture Nothing (Just rect)
            SDL.destroyTexture texture

    let txtVida = "Vida: " <> DT.pack (show (round vida :: Int))
    renderizarTexto txtVida 10 10

    let velRedondeada = (fromIntegral (round (velocidad * 100) :: Int) / 100) :: Float
    let txtVel = "Velocidad: " <> DT.pack (show velRedondeada)
    renderizarTexto txtVel 10 40

-- Visión dinamica de los buffs en el HUD
dibujarBuffs :: SDL.Renderer -> Font.Font -> [Types.Buff] -> IO ()
dibujarBuffs renderer font buffs = do
    let startX = 10
    let startY = 70
    
    let loopDraw [] _ = return ()
        loopDraw (b:bs) y = do
            let tiempoStr = DT.pack (show ((fromIntegral (round (Types.buffTiempo b * 10) :: Int) / 10.0) :: Float))
            let texto = Types.buffNombre b <> ": " <> tiempoStr <> "s"
            let color = SDL.V4 0 255 255 255 
            
            surface <- Font.solid font color texto
            texture <- SDL.createTextureFromSurface renderer surface
            SDL.freeSurface surface
            ti <- SDL.queryTexture texture
            let rect = SDL.Rectangle (SDL.P (SDL.V2 startX y)) (SDL.V2 (SDL.textureWidth ti) (SDL.textureHeight ti))
            SDL.copy renderer texture Nothing (Just rect)
            SDL.destroyTexture texture
            
            loopDraw bs (y + 30)

    loopDraw buffs startY

-- Debug: Deadzone
dibujarDeadzone :: SDL.Renderer -> SDL.V2 Float -> IO ()
dibujarDeadzone renderer (SDL.V2 w h) = do
    let topLeftX = (fromIntegral screenWidth - w) / 2
    let topLeftY = (fromIntegral screenHeight - h) / 2
    let rect = Utils.toSDLRect (SDL.V2 topLeftX topLeftY) (SDL.V2 w h)
    
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255 -- Rojo
    SDL.drawRect renderer (Just rect)

-- Elementos del juego
dibujarObstaculo :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Types.Obstaculo -> IO ()
dibujarObstaculo renderer texture camPos (Types.Obstaculo pos size angle) = do
    let screenPos = worldToScreen pos camPos
    let rect = Utils.toSDLRect screenPos size
    let angleCDouble = realToFrac angle :: FCT.CDouble

    SDL.textureColorMod texture SDL.$= SDL.V3 100 100 100
    SDL.copyEx renderer 
                texture 
                Nothing
                (Just rect)
                angleCDouble
                Nothing
                (SDL.V2 False False)
    

dibujarEnemigo :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Types.Enemigo -> IO ()
dibujarEnemigo renderer skinTexture camPos enem = do
    let posE = Types.posEnemigo enem
    let tamE = Types.tamEnemigo enem
    let angE = Types.angEnemigo enem
    dibujarEntidadRotada renderer skinTexture camPos posE tamE angE (SDL.V3 0 255 0)

    let centroE = posE + (tamE LV.^* 0.5)
    dibujarConoOutline renderer camPos centroE angE 100 45

dibujarJugador :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Types.Jugador -> IO ()
dibujarJugador renderer skinTexture camPos player = do
    let posJ = Types.posJugador player
    let tamJ = Types.tamJugador player
    let angJ = Types.angJugador player
    dibujarEntidadRotada renderer skinTexture camPos posJ tamJ angJ (SDL.V3 255 255 255)

    let centroJ = posJ + (tamJ LV.^* 0.5)
    dibujarConoOutline renderer camPos centroJ angJ 60 30

dibujarItems :: SDL.Renderer -> SDL.V2 Float -> Types.Item -> IO ()
dibujarItems renderer camPos item = do
    let posI = Types.posItem item
    let tamI = Types.tamItem item
    let itemScreePos = worldToScreen posI camPos
    let itemSkin = Utils.toSDLRect itemScreePos tamI
    SDL.fillRect renderer (Just itemSkin)

dibujarConoOutline :: SDL.Renderer -> SDL.V2 Float -> SDL.V2 Float -> Float -> Float -> Float -> IO ()
dibujarConoOutline renderer camPos origen angulo longitud apertura = do
    let verticesMundo = OC.calcularVerticesCono origen angulo longitud apertura
    let verticesPantalla = map (`worldToScreen` camPos) verticesMundo

    let toP (SDL.V2 x y) = SDL.P (SDL.V2 (round x) (round y))
    let puntosSDL = map toP verticesPantalla
    let puntosFinales = case puntosSDL of
            []     -> []
            (p1:_) -> puntosSDL ++ [p1]

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 0 255 
    SDL.drawLines renderer (DVS.fromList puntosFinales)