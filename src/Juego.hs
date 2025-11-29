module Juego where
-- Módulos del sistema
import qualified Control.Monad.State as CMS
import qualified SDL
import qualified Control.Monad       as CM
import qualified Lens.Micro          as LMi
-- Módulos propios
import qualified Types
import qualified Globals.Types       as GType
import qualified Personajes.Types    as PType
import qualified Objetos.Types       as OType
import qualified Objetos.Camara      as OC
import qualified Objetos.ItemBuff    as OIBuff
import qualified Objetos.Spawner     as OS
import qualified Objetos.Particula   as OP 
import qualified Fisica.Colisiones   as FC
import qualified Personajes.Jugador  as PJ
import qualified Personajes.Zombie   as PZ

dt :: Float
dt = 0.016

updateGame :: Types.Input -> CMS.State Types.GameState ()
updateGame input = do
    updateGlobalSystems
    
    gs <- CMS.get
    let jug = gs LMi.^. Types.jugador
    let vidaActual = jug LMi.^. PType.jugEnt . GType.entVid . GType.vidAct

    if vidaActual > 0
        then handleGameplay input
        else handleGameOver input

updateGlobalSystems :: CMS.State Types.GameState ()
updateGlobalSystems = do
    gs <- CMS.get
    let (spawnersAct, newEnemies, newItems, nextRng) = 
            OS.actualizarSpawners dt (gs LMi.^. Types.rng) (gs LMi.^. Types.spawners)

    let particulasVivas = OP.actualizarParticulas dt (gs LMi.^. Types.particulas)

    CMS.put $ gs
        LMi.& Types.spawners   LMi..~ spawnersAct
        LMi.& Types.enemigos   LMi.%~ (++ newEnemies)
        LMi.& Types.itemsBuff  LMi.%~ (++ newItems)
        LMi.& Types.rng        LMi..~ nextRng
        LMi.& Types.particulas LMi..~ particulasVivas


handleGameOver :: Types.Input -> CMS.State Types.GameState ()
handleGameOver input = do
    CM.when (input LMi.^. Types.teclaRespawn) respawnPlayer

respawnPlayer :: CMS.State Types.GameState ()
respawnPlayer = do
    gs <- CMS.get
    let spawnP = gs LMi.^. Types.jugador . PType.spawnPoint

    CMS.modify $ Types.jugador LMi.%~ \j -> j
        LMi.& PType.jugEnt . GType.entVid . GType.vidAct LMi..~ (j LMi.^. PType.jugEnt . GType.entVid . GType.vidMax)
        LMi.& PType.jugEnt . GType.entBox . GType.boxPos LMi..~ spawnP
        LMi.& PType.jugEnt . GType.entEmp . GType.empVec LMi..~ SDL.V2 0 0
        LMi.& PType.jugEnt . GType.entBuf LMi..~ []

handleGameplay :: Types.Input -> CMS.State Types.GameState ()
handleGameplay input = do
    gs <- CMS.get
    
    let jugadorBase = gs LMi.^. Types.jugador
        enemigos    = gs LMi.^. Types.enemigos
        mapa        = gs LMi.^. Types.mapa
    let jugadorConBuffs = OIBuff.procesarBuffs dt jugadorBase
    let (jugadorGolpeado, enemigosPostCombate) = FC.resolverCombate jugadorConBuffs enemigos
    let vidaPostCombate = jugadorGolpeado LMi.^. PType.jugEnt . GType.entVid . GType.vidAct

    if vidaPostCombate <= 0
        then triggerDeath jugadorGolpeado enemigosPostCombate
        else do
            let jugadorMovido = PJ.moverJugador input jugadorGolpeado mapa
            let (jugadorConItems, itemsRestantes) = handleItems jugadorMovido (gs LMi.^. Types.itemsBuff)
            let enemigosFinales = PZ.updateEnemies jugadorConItems enemigosPostCombate mapa
            let hayPeligro = any (LMi.^. PType.eneVerJug) enemigosFinales
            let camaraFinal = OC.actualizarCamara input jugadorConItems hayPeligro (gs LMi.^. Types.camara)

            CMS.put $ gs
                LMi.& Types.jugador     LMi..~ jugadorConItems
                LMi.& Types.itemsBuff   LMi..~ itemsRestantes
                LMi.& Types.enemigos    LMi..~ enemigosFinales
                LMi.& Types.camara      LMi..~ camaraFinal

handleItems :: PType.Jugador -> [OType.ItemBuff] -> (PType.Jugador, [OType.ItemBuff])
handleItems jug items = 
    let boxJug = jug LMi.^. PType.jugEnt . GType.entBox
        itemsTocados   = FC.checkColisionsItems boxJug items
        itemsRestantes = filter (\it -> not (it `elem` itemsTocados)) items
        jugConPowerUps = foldr (\it j -> OIBuff.aplicarEfecto it j) jug itemsTocados
    in (jugConPowerUps, itemsRestantes)

triggerDeath :: PType.Jugador -> [PType.Zombie] -> CMS.State Types.GameState ()
triggerDeath jug enemigos = do
    gs <- CMS.get
    
    let posMuerte = jug LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    let cantidadParticulas = 50 
    let multiplicadorVel = 1.2
    let rngActual = gs LMi.^. Types.rng

    let (nuevasParticulas, nextRng) = OP.crearExplosionGradualDown posMuerte cantidadParticulas multiplicadorVel rngActual
    let jugActualizado = jug LMi.& (PType.jugEnt . GType.entVid . GType.vidMrt LMi.+~ 1)

    CMS.put $ gs 
        LMi.& Types.jugador    LMi..~ jugActualizado 
        LMi.& Types.enemigos   LMi..~ enemigos
        LMi.& Types.particulas LMi.%~ (++ nuevasParticulas)
        LMi.& Types.rng        LMi..~ nextRng