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

import qualified Graficos.Dibujado  as GD
import qualified Graficos.HUD       as GHUD
import qualified Globals.Camara     as GC

import qualified Mapas.Mapa         as MM

-- import qualified Objetos.Spawner    as OSpawner
import qualified Objetos.Particula  as OParticulas
import qualified Objetos.Buff       as OBuff
import qualified Objetos.Arma       as OArma

import qualified Personajes.Zombie  as OZombie
import qualified Personajes.Jugador as OJugador

renderGame :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.Input -> Types.GameState -> IO ()
renderGame renderer font blockTexture skinTexture input gs = do

    let player      = gs LMi.^. Types.jugador
    let vidaStruct  = player LMi.^. PType.jugEnt . GType.entVid
    let vidAct      = vidaStruct LMi.^. GType.vidAct
    let bufJ        = player LMi.^. PType.jugEnt . GType.entBuf 
    let listaItems  = gs LMi.^. Types.items
    let tiempo      = gs LMi.^. Types.tiempoJuego
    let tiempoTotal = gs LMi.^. Types.tiempoTotal
    
    let vivo        = vidAct > 0 && tiempo > 0

    let cam      = gs  LMi.^. Types.camara
    let zoom     = cam LMi.^. GType.zoomLevel
    let camPos   = cam LMi.^. GType.posCamara
    let dzSize   = cam LMi.^. GType.deadzoneSize

    let faseTutorial = gs LMi.^. Types.faseTutorial
    let esTutorial   = gs LMi.^. Types.tutorialActivo

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 120 120 120 255
    SDL.clear renderer

    mapM_ (\item -> 
        case item LMi.^. GType.iteTipo of
            GType.EsBuff _  -> OBuff.dibujar renderer skinTexture camPos zoom item
            GType.EsArma _  -> OArma.dibujar renderer skinTexture camPos zoom item
            GType.NoItem    -> return ()
        ) listaItems
    
    mapM_ (OZombie.dibujar renderer skinTexture camPos zoom)        (gs LMi.^. Types.enemigos)
    -- mapM_ (OSpawner.dibujar renderer skinTexture camPos zoom)       (gs LMi.^. Types.spawners)
    mapM_ (OParticulas.dibujar renderer skinTexture camPos zoom)    (gs LMi.^. Types.particulas)
    mapM_ (MM.dibujar renderer blockTexture camPos zoom)            (gs LMi.^. Types.mapa)

    if vivo then do
        OJugador.dibujar renderer skinTexture camPos zoom player

        let isModifyingDZ = input LMi.^. Types.increaseDZ || input LMi.^. Types.decreaseDZ
        if isModifyingDZ 
            then GC.dibujarDeadzone renderer skinTexture dzSize zoom
            else return ()

        let screenDims = (fromIntegral GD.screenWidth, fromIntegral GD.screenHeight)
        let (winW, _) = screenDims
        GHUD.dibujarFondosHUD renderer blockTexture screenDims
        GHUD.dibujarInventario renderer font skinTexture player
        GHUD.dibujarBuffs renderer font skinTexture bufJ winW
        GHUD.dibujarHUDAtasco renderer font blockTexture player
        GHUD.dibujarHUD renderer font blockTexture player tiempo screenDims

        if esTutorial 
            then GHUD.dibujarTutorialOverlay renderer font blockTexture faseTutorial screenDims
            else return ()

    else do
        GHUD.dibujarMuerte renderer font skinTexture tiempoTotal

    SDL.present renderer