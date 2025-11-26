{-# LANGUAGE OverloadedStrings #-}

module Render 
    ( renderGame
    , screenWidth
    , screenHeight
    ) where

import qualified SDL
import qualified SDL.Font as Font
import Foreign.C.Types (CInt)
import Data.Text (pack)

-- Importamos tus tipos definidos
import Types 

-- Configuración de Pantalla
screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

screenCenter :: SDL.V2 Float
screenCenter = SDL.V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

-- Helper: Convertir coordenadas de Float a SDL.Rectangle CInt
toSDLRect :: SDL.V2 Float -> SDL.V2 Float -> SDL.Rectangle CInt
toSDLRect (SDL.V2 x y) (SDL.V2 w h) = 
    SDL.Rectangle (SDL.P (SDL.V2 (round x) (round y))) (SDL.V2 (round w) (round h))

-- Helper: Cámara -> Pantalla
worldToScreen :: SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float
worldToScreen worldPos camPos = worldPos - camPos + screenCenter

-- HUD
dibujarHUD :: SDL.Renderer -> Font.Font -> Float -> Float -> IO ()
dibujarHUD renderer font vida velocidad = do
    let color = SDL.V4 255 255 255 255 -- Blanco

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

    let txtVida = "Vida: " <> pack (show (round vida :: Int))
    renderizarTexto txtVida 10 10

    let velRedondeada = (fromIntegral (round (velocidad * 100) :: Int) / 100) :: Float
    let txtVel = "Velocidad: " <> pack (show velRedondeada)
    renderizarTexto txtVel 10 40

-- Debug: Deadzone
dibujarDeadzone :: SDL.Renderer -> SDL.V2 Float -> IO ()
dibujarDeadzone renderer (SDL.V2 w h) = do
    let topLeftX = (fromIntegral screenWidth - w) / 2
    let topLeftY = (fromIntegral screenHeight - h) / 2
    let rect = toSDLRect (SDL.V2 topLeftX topLeftY) (SDL.V2 w h)
    
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255 -- Rojo
    SDL.drawRect renderer (Just rect)

-- Elementos del juego
dibujarObstaculo :: SDL.Renderer -> SDL.V2 Float -> Obstaculo -> IO ()
dibujarObstaculo renderer camPos (Obstaculo pos size) = do
    let screenPos = worldToScreen pos camPos
    let rect = toSDLRect screenPos size
    SDL.fillRect renderer (Just rect)

dibujarEnemigo :: SDL.Renderer -> SDL.V2 Float -> Enemigo -> IO ()
dibujarEnemigo renderer camPos enem = do
    let posE = posEnemigo enem
    let tamE = tamEnemigo enem
    let enemyScreePos = worldToScreen posE camPos
    let enemySkin = toSDLRect enemyScreePos tamE
    SDL.fillRect renderer (Just enemySkin)

dibujarItems :: SDL.Renderer -> SDL.V2 Float -> Item -> IO ()
dibujarItems renderer camPos item = do
    let posI = posItem item
    let tamI = tamItem item
    let itemScreePos = worldToScreen posI camPos
    let itemSkin = toSDLRect itemScreePos tamI
    SDL.fillRect renderer (Just itemSkin)

-- Función Principal de Renderizado (Exportada)
renderGame :: SDL.Renderer -> Font.Font -> GameState -> IO ()
renderGame renderer font gs = do
    let player = jugador gs
    let posJ   = posJugador player
    let tamJ   = tamJugador player
    let velJ   = velJugador player
    let vidJ   = vidJugador player

    let cam    = camara gs
    let camPos = posCamara cam
    let dzSize = deadzoneSize cam

    -- Limpiar pantalla (Fondo negro)
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.clear renderer

    -- Renderizar elementos (Orden: Fondo -> Objetos -> Jugador -> UI)
    
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 100 100 255
    mapM_ (dibujarObstaculo renderer camPos) (mapa gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 255
    mapM_ (dibujarEnemigo renderer camPos) (enemigos gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 0 255
    mapM_ (dibujarItems renderer camPos) (items gs)

    -- Jugador
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    let playerScreenPos = worldToScreen posJ camPos
    let playerSkin = toSDLRect playerScreenPos tamJ
    SDL.fillRect renderer (Just playerSkin)
    
    -- UI / Debug
    dibujarDeadzone renderer dzSize
    dibujarHUD renderer font vidJ velJ
    
    -- Swap buffers
    SDL.present renderer