{-# LANGUAGE OverloadedStrings #-}
module Graficos.Render where
-- Módulos del sistema
import qualified SDL
import qualified SDL.Font   as Font
import qualified Lens.Micro as LMi
-- Módulos propios
import qualified Types
import qualified Personajes.Types   as PType
import qualified Globals.Types      as GType
import qualified Objetos.Types      as OType

import qualified Graficos.Dibujado  as GD

import qualified Objetos.Camara     as OCamara
import qualified Objetos.HUD        as OHUD
import qualified Objetos.ItemBuff   as OIBuff
import qualified Objetos.Spawner    as OSpawner
import qualified Objetos.Particula  as OParticulas
import qualified Personajes.Zombie  as OZombie
import qualified Personajes.Jugador as OJugador

renderGame :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.GameState -> IO ()
renderGame renderer font blockTexture skinTexture gs = do

    let player      = gs LMi.^. Types.jugador
    let vidaStruct  = player LMi.^. PType.jugEnt . GType.entVid
    let vidAct      = vidaStruct LMi.^. GType.vidAct
    let bufJ        = player LMi.^. PType.jugEnt . GType.entBuf 
    let listaItems  = gs LMi.^. Types.itemsBuff
    let vivo        = vidAct > 0

    let cam      = gs  LMi.^. Types.camara
    let zoom     = cam LMi.^. OType.zoomLevel
    let camPos   = cam LMi.^. OType.posCamara
    let dzSize   = cam LMi.^. OType.deadzoneSize

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 50 50 50 255
    SDL.clear renderer

    -- 1. Renderizado del Mundo
    mapM_ (OIBuff.dibujar renderer skinTexture camPos zoom)      listaItems 
    mapM_ (OZombie.dibujar renderer skinTexture camPos zoom)     (gs LMi.^. Types.enemigos)
    mapM_ (OSpawner.dibujar renderer skinTexture camPos zoom)    (gs LMi.^. Types.spawners)
    mapM_ (OParticulas.dibujar renderer skinTexture camPos zoom) (gs LMi.^. Types.particulas)
    mapM_ (GD.dibujarBox renderer blockTexture camPos zoom)      (gs LMi.^. Types.mapa)

    -- 2. Renderizado de Jugador y UI
    if vivo then do
        OJugador.dibujar renderer skinTexture camPos zoom player
        OCamara.dibujarDeadzone renderer skinTexture dzSize zoom
        OHUD.dibujarBuffs renderer font skinTexture bufJ

        let screenDims = (fromIntegral GD.screenWidth, fromIntegral GD.screenHeight)
        OHUD.dibujarHUD renderer font skinTexture player screenDims
    else do
        let muertesVal = vidaStruct LMi.^. GType.vidMrt
        OHUD.dibujarMuerte renderer font skinTexture muertesVal

    SDL.present renderer