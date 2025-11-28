{-# LANGUAGE OverloadedStrings #-}
module Graficos.Render where

-- Modulos del sistema
import qualified SDL
import qualified SDL.Font as Font

-- Modulos propios
import qualified Types
import qualified Graficos.Dibujado as RD

-- Función Principal de Renderizado (Exportada)
renderGame :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.GameState -> IO ()
renderGame renderer font blockTexture skinTexture gs = do
    let player   = Types.jugador gs
    let velJ     = Types.velJugador player
    let vidJ     = Types.vidJugador player

    let cam     = Types.camara gs
    let camPos  = Types.posCamara cam
    let dzSize  = Types.deadzoneSize cam

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 50 50 50 255
    SDL.clear renderer

    mapM_ (RD.dibujarObstaculo renderer blockTexture camPos) (Types.mapa gs)
    mapM_ (RD.dibujarEnemigo renderer skinTexture camPos) (Types.enemigos gs)
    RD.dibujarJugador renderer skinTexture camPos player

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 0 255
    mapM_ (RD.dibujarItems renderer camPos) (Types.items gs)

    RD.dibujarDeadzone renderer dzSize
    RD.dibujarHUD renderer font vidJ velJ
    RD.dibujarBuffs renderer font (Types.buffsActivos player)

    SDL.present renderer