{-# LANGUAGE OverloadedStrings #-}
module Graficos.Dibujado where

-- Modulos del sistema
import qualified SDL
import qualified SDL.Font as Font
import qualified Foreign.C.Types as FCT
import qualified Data.Text as DT

-- Modulos propios
import qualified Types
import qualified Utils

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
    

dibujarEnemigo :: SDL.Renderer -> SDL.V2 Float -> Types.Enemigo -> IO ()
dibujarEnemigo renderer camPos enem = do
    let posE = Types.posEnemigo enem
    let tamE = Types.tamEnemigo enem
    let enemyScreePos = worldToScreen posE camPos
    let enemySkin = Utils.toSDLRect enemyScreePos tamE
    SDL.fillRect renderer (Just enemySkin)

dibujarItems :: SDL.Renderer -> SDL.V2 Float -> Types.Item -> IO ()
dibujarItems renderer camPos item = do
    let posI = Types.posItem item
    let tamI = Types.tamItem item
    let itemScreePos = worldToScreen posI camPos
    let itemSkin = Utils.toSDLRect itemScreePos tamI
    SDL.fillRect renderer (Just itemSkin)