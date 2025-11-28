{-# LANGUAGE OverloadedStrings #-}
module Graficos.Render where

import qualified SDL
import qualified SDL.Font as Font

import qualified Types
import qualified Objetos.Camara as OCamara
import qualified Objetos.HUD as OHUD
import qualified Objetos.Items as OItems
import qualified Objetos.Obstaculo as OObstaculo
import qualified Objetos.Spawner as OSpawner
import qualified Personajes.Enemigo as OEnemigo
import qualified Personajes.Jugador as OJugador

renderGame :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.GameState -> IO ()
renderGame renderer font blockTexture skinTexture gs = do
    let player   = Types.jugador gs
    let velJ     = Types.velJugador player
    let vidJ     = Types.vidJugador player
    let bufJ     = Types.buffsActivos player

    let cam     = Types.camara gs
    let zoom    = Types.zoomLevel cam
    let camPos  = Types.posCamara cam

    let dzSize  = Types.deadzoneSize cam

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 50 50 50 255
    SDL.clear renderer

    mapM_ (OItems.dibujar renderer skinTexture camPos zoom)      (Types.items gs)
    mapM_ (OObstaculo.dibujar renderer blockTexture camPos zoom) (Types.mapa gs)
    mapM_ (OEnemigo.dibujar renderer skinTexture camPos zoom)    (Types.enemigos gs)
    mapM_ (OSpawner.dibujar renderer skinTexture camPos zoom)    (Types.spawners gs)
    OJugador.dibujar renderer skinTexture camPos zoom player
    OCamara.dibujarDeadzone renderer skinTexture dzSize zoom

    OHUD.dibujarBuffs renderer font skinTexture bufJ
    OHUD.dibujarHUD renderer font skinTexture vidJ velJ

    SDL.present renderer