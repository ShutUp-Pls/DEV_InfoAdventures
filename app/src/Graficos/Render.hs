{-# LANGUAGE OverloadedStrings #-}
module Graficos.Render where

-- Modulos del sistema
import qualified SDL
import qualified SDL.Font as Font
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Graficos.Dibujado as RD

-- Función Principal de Renderizado (Exportada)
renderGame :: SDL.Renderer -> Font.Font -> SDL.Texture -> Types.GameState -> IO ()
renderGame renderer font blockTexture gs = do
    let player = Types.jugador gs
    let posJ   = Types.posJugador player
    let tamJ   = Types.tamJugador player
    let velJ   = Types.velJugador player
    let vidJ   = Types.vidJugador player

    let cam    = Types.camara gs
    let camPos = Types.posCamara cam
    let dzSize = Types.deadzoneSize cam

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 50 50 50 255
    SDL.clear renderer

    mapM_ (RD.dibujarObstaculo renderer blockTexture camPos) (Types.mapa gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 255
    mapM_ (\enem -> do
        RD.dibujarEnemigo renderer camPos enem
        
        let centroE = Types.posEnemigo enem + (Types.tamEnemigo enem LV.^* 0.5)
        RD.dibujarConoOutline renderer camPos centroE (Types.angEnemigo enem) 100 45
        ) (Types.enemigos gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 0 255
    mapM_ (RD.dibujarItems renderer camPos) (Types.items gs)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    let playerScreenPos = RD.worldToScreen posJ camPos
    let playerSkin = Utils.toSDLRect playerScreenPos tamJ
    SDL.fillRect renderer (Just playerSkin)
    
    let centroJ = posJ + (tamJ LV.^* 0.5)
    RD.dibujarConoOutline renderer camPos centroJ (Types.angJugador player) 60 30

    RD.dibujarDeadzone renderer dzSize
    RD.dibujarHUD renderer font vidJ velJ
    RD.dibujarBuffs renderer font (Types.buffsActivos player)

    SDL.present renderer