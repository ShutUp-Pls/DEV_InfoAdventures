module Juego where
-- Módulos del sistema
import qualified SDL
import qualified Control.Monad.State    as CMS
import qualified Lens.Micro             as LMi
import qualified Data.List              as DL
-- Módulos propios
import qualified Types
import qualified Globals.Types          as GType
import qualified Personajes.Types       as PType

import qualified Globals.Camara         as OC

import qualified Fisica.Colisiones      as FCol
import qualified Fisica.Combates        as FCom
import qualified Fisica.Disparo         as FD

import qualified Personajes.Jugador     as PJ
import qualified Personajes.Zombie      as PZ

import qualified Objetos.Buff           as OBuff
import qualified Objetos.Spawner        as OS
import qualified Objetos.Particula      as OPart

dt :: Float
dt = 0.016

updateGame :: Types.Input -> CMS.State Types.GameState ()
updateGame input = do
    updateGlobalSystems
    gs <- CMS.get
    let vidaActual = gs LMi.^. Types.jugador LMi.^. PType.jugEnt . GType.entVid . GType.vidAct
    let tiempoRestante = gs LMi.^. Types.tiempoJuego

    if vidaActual > 0 && tiempoRestante > 0
        then handleGameplay input
        else handleGameOver input

updateGlobalSystems :: CMS.State Types.GameState ()
updateGlobalSystems = do
    gs <- CMS.get
    let (spawnersAct, newEnemies, newItems, nextRng) = OS.actualizarSpawners dt (gs LMi.^. Types.rng) (gs LMi.^. Types.spawners)

    let particulasVivas = OPart.actualizarParticulas dt (gs LMi.^. Types.particulas)
    let nuevoTiempo = max 0.0 ((gs LMi.^. Types.tiempoJuego) - dt)
    let cdActual = gs LMi.^. Types.cooldownUI
    let cdNuevo  = max 0 (cdActual - dt)
    
    let vidaActual = gs LMi.^. Types.jugador LMi.^. PType.jugEnt . GType.entVid . GType.vidAct
    let tiempoRestante = gs LMi.^. Types.tiempoJuego
    let vivo = vidaActual > 0 && tiempoRestante > 0
    
    let nuevoTiempoTotal = if vivo 
                           then (gs LMi.^. Types.tiempoTotal) + dt 
                           else (gs LMi.^. Types.tiempoTotal)

    CMS.put $ gs
        LMi.& Types.cooldownUI  LMi..~ cdNuevo
        LMi.& Types.spawners    LMi..~ spawnersAct
        LMi.& Types.enemigos    LMi.%~ (++ newEnemies)
        LMi.& Types.items       LMi.%~ (++ newItems)
        LMi.& Types.rng         LMi..~ nextRng
        LMi.& Types.particulas  LMi..~ particulasVivas
        LMi.& Types.tiempoJuego LMi..~ nuevoTiempo
        LMi.& Types.tiempoTotal LMi..~ nuevoTiempoTotal

handleGameplay :: Types.Input -> CMS.State Types.GameState ()
handleGameplay input = do
    gs <- CMS.get
    
    let jugadorBase         = gs LMi.^. Types.jugador
        enemigosOriginales  = gs LMi.^. Types.enemigos
        partes              = gs LMi.^. Types.particulas
        mapa                = gs LMi.^. Types.mapa
        rngActual           = gs LMi.^. Types.rng
        camara              = gs LMi.^. Types.camara
        cdUI                = gs LMi.^. Types.cooldownUI

    let switchNext = input LMi.^. Types.nextWeapon
    let switchPrev = input LMi.^. Types.prevWeapon
    
    let (jugadorSwitched, nuevoCdUI) =
            if cdUI <= 0 
            then 
                if switchNext then (PJ.cambiarArmaSiguiente jugadorBase, 0.2)
                else if switchPrev then (PJ.cambiarArmaAnterior jugadorBase, 0.2)
                else (jugadorBase, 0)
            else 
                (jugadorBase, cdUI)

    let jugadorConBuffs = OBuff.procesarBuffs dt jugadorSwitched
    let (jugadorGolpeado, enemigosPostCombate) = FCom.choqueJugadorZombies jugadorConBuffs enemigosOriginales
    let vidaPostCombate = jugadorGolpeado LMi.^. PType.jugEnt . GType.entVid . GType.vidAct

    if vidaPostCombate <= 0
        then triggerDeath jugadorGolpeado enemigosPostCombate
        else do
            let jugadorMovido = PJ.moverJugadorP input jugadorGolpeado mapa
            let (jugadorConItems, itemsRestantes, tiempoGanado) = handleItems jugadorMovido (gs LMi.^. Types.items)

            let isShooting = input LMi.^. Types.disparar

            let (nuevasBalas, jugadorFinal, rngFinal) = FD.procesarDisparo dt isShooting rngActual jugadorConItems
            let partesMovidas   = OPart.actualizarParticulas dt partes
            let partesTotales   = partesMovidas ++ nuevasBalas

            let zombiesMovidos = PZ.moverZombies jugadorFinal enemigosPostCombate mapa

            let (particulasFinales, zombiesDañados) = FCom.choqueParticulasZombies partesTotales zombiesMovidos
            let zombiesVivos = PZ.limpiarZombiesMuertos zombiesDañados
            

            let camaraFinal = OC.actualizarCamara input jugadorFinal zombiesDañados camara

            CMS.put $ gs
                LMi.& Types.jugador     LMi..~ jugadorFinal  
                LMi.& Types.items       LMi..~ itemsRestantes
                LMi.& Types.enemigos    LMi..~ zombiesVivos
                LMi.& Types.particulas  LMi..~ particulasFinales
                LMi.& Types.camara      LMi..~ camaraFinal
                LMi.& Types.rng         LMi..~ rngFinal
                LMi.& Types.tiempoJuego LMi.+~ tiempoGanado
                LMi.& Types.cooldownUI  LMi..~ nuevoCdUI

handleGameOver :: Types.Input -> CMS.State Types.GameState ()
handleGameOver _ = do
    return ()

respawnPlayer :: CMS.State Types.GameState ()
respawnPlayer = do
    gs <- CMS.get
    let spawnP = gs LMi.^. Types.jugador . PType.spawnPoint

    CMS.modify $ Types.jugador LMi.%~ \j -> j
        LMi.& PType.jugEnt . GType.entVid . GType.vidAct LMi..~ (j LMi.^. PType.jugEnt . GType.entVid . GType.vidMax)
        LMi.& PType.jugEnt . GType.entBox . GType.boxPos LMi..~ spawnP
        LMi.& PType.jugEnt . GType.entEmp . GType.empVec LMi..~ SDL.V2 0 0
        LMi.& PType.jugEnt . GType.entBuf LMi..~ []

    CMS.modify $ Types.tiempoJuego LMi..~ OBuff.tiempoInicial

handleItems :: PType.Jugador -> [GType.Item] -> (PType.Jugador, [GType.Item], Float)
handleItems jug items = 
    let boxJug = jug LMi.^. PType.jugEnt . GType.entBox
        itemsTocados   = FCol.checkColisionsItems boxJug items
        itemsRestantes = filter (\it -> not (it `elem` itemsTocados)) items

        (itemsTiempo, itemsOtros) = DL.partition OBuff.esItemTiempo itemsTocados
        tiempoTotal = sum $ map (\it -> 
            case it LMi.^. GType.iteTipo of 
                 GType.EsBuff b -> b LMi.^. GType.bufVlr
                 _ -> 0
            ) itemsTiempo
        (equipables, consumibles) = DL.partition (\it -> it LMi.^. GType.iteInv) itemsOtros
        
        jugConBuffs = foldr (\it j -> OBuff.aplicarEfecto it j) jug consumibles
        jugFinal = foldr PJ.equiparItem jugConBuffs equipables

    in (jugFinal, itemsRestantes, tiempoTotal)

triggerDeath :: PType.Jugador -> [PType.Zombie] -> CMS.State Types.GameState ()
triggerDeath jug enemigos = do
    gs <- CMS.get
    
    let posMuerte = jug LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    let rngActual = gs LMi.^. Types.rng

    let (nuevasParticulas, rngNuevo) = OPart.generarExplosion
                                                    posMuerte 
                                                    50 
                                                    OPart.idParHumo
                                                    (50.0, 150.0) 
                                                    (1.0, 2.5) 
                                                    (15.0, 40.0)
                                                    rngActual

    let jugActualizado = jug LMi.& (PType.jugEnt . GType.entVid . GType.vidMrt LMi.+~ 1)

    CMS.put $ gs 
        LMi.& Types.jugador    LMi..~ jugActualizado 
        LMi.& Types.enemigos   LMi..~ enemigos
        LMi.& Types.particulas LMi.%~ (++ nuevasParticulas)
        LMi.& Types.rng        LMi..~ rngNuevo