{-# LANGUAGE OverloadedStrings #-}
module Graficos.Render where

import qualified SDL
import qualified SDL.Font as Font

import qualified Types
import qualified Objetos.Camara as OCamara
import qualified Objetos.HUD as OHUD
import qualified Objetos.Items as OItems
import qualified Objetos.Obstaculo as OObstaculo
import qualified Personajes.Enemigo as OEnemigo
import qualified Personajes.Jugador as OJugador

renderGame :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.GameState -> IO ()
renderGame renderer font blockTexture skinTexture gs = do
    let player   = Types.jugador gs
    let velJ     = Types.velJugador player
    let vidJ     = Types.vidJugador player
    let bufJ     = Types.buffsActivos player

    let cam     = Types.camara gs
    let camPos  = Types.posCamara cam
    let dzSize  = Types.deadzoneSize cam

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 50 50 50 255
    SDL.clear renderer

    mapM_ (OItems.dibujar renderer skinTexture camPos)      (Types.items gs)
    mapM_ (OObstaculo.dibujar renderer blockTexture camPos) (Types.mapa gs)
    mapM_ (OEnemigo.dibujar renderer skinTexture camPos)    (Types.enemigos gs)
    
    OHUD.dibujarBuffs renderer font skinTexture bufJ
    OHUD.dibujarHUD renderer font skinTexture vidJ velJ
    OJugador.dibujar renderer skinTexture camPos player
    OCamara.dibujarDeadzone renderer skinTexture dzSize

    SDL.present renderer