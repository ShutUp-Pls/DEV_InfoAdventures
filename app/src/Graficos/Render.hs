{-# LANGUAGE OverloadedStrings #-}
module Graficos.Render where

-- Modulos del sistema
import qualified SDL
import qualified SDL.Font as Font
import qualified Foreign.C.Types as FCT

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Graficos.Dibujado as RD

-- Configuración de Pantalla
screenWidth, screenHeight :: FCT.CInt
screenWidth = 800
screenHeight = 600

-- Calculamos el centrol
screenCenter :: SDL.V2 Float
screenCenter = SDL.V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

-- Helper: Cámara -> Pantalla
worldToScreen :: SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float
worldToScreen worldPos camPos = worldPos - camPos + screenCenter

-- Función Principal de Renderizado (Exportada)
renderGame :: SDL.Renderer -> Font.Font -> Types.GameState -> IO ()
renderGame renderer font gs = do
    let player = Types.jugador gs
    let posJ   = Types.posJugador player
    let tamJ   = Types.tamJugador player
    let velJ   = Types.velJugador player
    let vidJ   = Types.vidJugador player

    let cam    = Types.camara gs
    let camPos = Types.posCamara cam
    let dzSize = Types.deadzoneSize cam

    -- Limpiar pantalla (Fondo negro)
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.clear renderer

    -- Renderizar elementos (Orden: Fondo -> Objetos -> Jugador -> UI)
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 100 100 255
    mapM_ (RD.dibujarObstaculo renderer camPos) (Types.mapa gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 255
    mapM_ (RD.dibujarEnemigo renderer camPos) (Types.enemigos gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 0 255
    mapM_ (RD.dibujarItems renderer camPos) (Types.items gs)

    -- Jugador
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    let playerScreenPos = worldToScreen posJ camPos
    let playerSkin = Utils.toSDLRect playerScreenPos tamJ
    SDL.fillRect renderer (Just playerSkin)
    
    -- UI / Debug
    RD.dibujarDeadzone renderer dzSize
    RD.dibujarHUD renderer font vidJ velJ
    RD.dibujarBuffs renderer font (Types.buffsActivos player)
    
    -- Swap buffers
    SDL.present renderer