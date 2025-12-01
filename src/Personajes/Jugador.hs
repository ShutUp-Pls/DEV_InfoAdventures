{-# LANGUAGE OverloadedStrings #-}
module Personajes.Jugador where
-- Módulos del sistema
import qualified SDL
import qualified Control.Monad.State as CMS
import qualified Lens.Micro          as LMi
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType

import qualified Fisica.Colisiones  as FC

import qualified Graficos.Dibujado  as GD
import qualified Personajes.Control as PControl

crearBoxJugador :: SDL.V2 Float -> GType.Box
crearBoxJugador pos = GType.Box
    { GType._boxPos = pos
    , GType._boxTam = SDL.V2 30 30
    , GType._boxAng = 0.0
    , GType._boxRad = 15.0
    }

crearStatsMovimiento :: GType.Movimiento
crearStatsMovimiento = GType.Movimiento 
    { GType._movVel = 4.0
    , GType._movRot = 5.0 
    , GType._movFac = 1.0
    , GType._movAct = 0.0
    }

crearStatsVida :: GType.Vida
crearStatsVida = GType.Vida 
    { GType._vidAct = 100.0
    , GType._vidMax = 100.0
    , GType._vidMrt = 0
    }

crearEmpujeNeutro :: GType.Empuje
crearEmpujeNeutro = GType.Empuje 
    { GType._empVec = SDL.V2 0 0
    , GType._empFrz = 10.0
    }

crearEntidadJugador :: SDL.V2 Float -> GType.Entidad
crearEntidadJugador pos = GType.Entidad
    { GType._entBox = crearBoxJugador pos
    , GType._entMov = crearStatsMovimiento
    , GType._entVid = crearStatsVida
    , GType._entEmp = crearEmpujeNeutro
    , GType._entBuf = []
    , GType._entInv = []
    , GType._entHnd = GType.itemVacio
    }

crearJugador :: SDL.V2 Float -> SDL.V2 Float -> PType.Jugador
crearJugador startPos spawnPos = PType.Jugador
    { PType._factCorrer = 0.75
    , PType._spawnPoint = spawnPos
    , PType._jugEnt     = crearEntidadJugador startPos
    }

moverJugador :: Types.Input -> PType.Jugador -> [GType.Box] -> PType.Jugador
moverJugador input jugadorActual mapObstaculos = 
    let 
        entidadInicial = jugadorActual LMi.^. PType.jugEnt
        bonusRun       = jugadorActual LMi.^. PType.factCorrer
        entidadFinal = CMS.execState (PControl.rutinaControl input bonusRun mapObstaculos) entidadInicial
    in
        jugadorActual LMi.& PType.jugEnt LMi..~ entidadFinal

resolverCombate :: PType.Jugador -> [PType.Zombie] -> (PType.Jugador, [PType.Zombie])
resolverCombate jug zombies = 
    let (res, state) = CMS.runState (mapM procesarZombie zombies) jug
    in (state, res)

procesarZombie :: PType.Zombie -> CMS.State PType.Jugador PType.Zombie
procesarZombie z = do
    jugadorActual <- CMS.get
    let jugEnt = jugadorActual LMi.^. PType.jugEnt
        zEnt   = z LMi.^. PType.zmbEnt
        zDmg   = z LMi.^. PType.zmbDamage

    let (zEntModificada, jugEntModificada) = CMS.runState (FC.interaccionFisicaEntreEntidades zDmg zEnt) jugEnt

    CMS.put (jugadorActual LMi.& PType.jugEnt LMi..~ jugEntModificada)
    return (z LMi.& PType.zmbEnt LMi..~ zEntModificada)

cambiarArmaSiguiente :: PType.Jugador -> PType.Jugador
cambiarArmaSiguiente jug =
    let 
        entidad = jug LMi.^. PType.jugEnt
        mano    = entidad LMi.^. GType.entHnd
        inv     = entidad LMi.^. GType.entInv
    in
        if null inv 
        then jug
        else 
            let nuevaMano = head inv
                nuevoInv  = tail inv ++ [mano]
            in jug LMi.& PType.jugEnt . GType.entHnd LMi..~ nuevaMano
                   LMi.& PType.jugEnt . GType.entInv LMi..~ nuevoInv

-- Lógica para cambiar al arma anterior ("K")
cambiarArmaAnterior :: PType.Jugador -> PType.Jugador
cambiarArmaAnterior jug =
    let 
        entidad = jug LMi.^. PType.jugEnt
        mano    = entidad LMi.^. GType.entHnd
        inv     = entidad LMi.^. GType.entInv
    in
        if null inv 
        then jug
        else 
            let nuevaMano = last inv
                nuevoInv  = mano : init inv
            in jug LMi.& PType.jugEnt . GType.entHnd LMi..~ nuevaMano
                   LMi.& PType.jugEnt . GType.entInv LMi..~ nuevoInv

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> PType.Jugador -> IO ()
dibujar renderer skinTexture camPos zoom player = do
    let posJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    let tamJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxTam
    let angJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxAng
    
    GD.dibujarTextura renderer skinTexture camPos zoom posJ tamJ angJ (SDL.V4 0 200 60 255)