{-# LANGUAGE OverloadedStrings #-}
module Personajes.Jugador where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro         as LMi
import qualified Linear.Vector      as LV
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType

import qualified Graficos.Dibujado  as GD

import qualified Fisica.Angulos     as FAng
import qualified Fisica.MovEntidad  as FMen
import qualified Fisica.Vectores as FAng

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
    { PType._factCorrer = 1.5
    , PType._spawnPoint = spawnPos
    , PType._jugEnt     = crearEntidadJugador startPos
    }

moverJugador :: Types.Input -> PType.Jugador -> [GType.Box] -> PType.Jugador
moverJugador input jugadorActual mapObstaculos = 
    let 
        entidadInicial  = jugadorActual LMi.^. PType.jugEnt
        runFactor       = jugadorActual LMi.^. PType.factCorrer
        entidadRotada   = FMen.girarEntidadPorTeclado input entidadInicial
        velBase         = entidadRotada LMi.^. GType.entMov . GType.movVel
        anguloActual    = entidadRotada LMi.^. GType.entBox . GType.boxAng
        magnitud        = FAng.magnitudPorTeclado input velBase runFactor anguloActual
        vecDir          = FAng.anguloAVector anguloActual
        velIntencion    = vecDir LV.^* magnitud
        jugadorFinal    = FMen.moverEntidad velIntencion mapObstaculos entidadRotada
    in  jugadorActual LMi.& PType.jugEnt LMi..~ jugadorFinal

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

equiparItem :: GType.Item -> PType.Jugador -> PType.Jugador
equiparItem nuevoItem jug =
    let 
        entidad         = jug LMi.^. PType.jugEnt
        mano            = entidad LMi.^. GType.entHnd
        inv             = entidad LMi.^. GType.entInv
        idMano          = mano LMi.^. GType.iteId
        itemsExistentes = filter (\i -> i LMi.^. GType.iteId /= 0) (mano : inv)
        
        obtenerSlot :: GType.Item -> Int
        obtenerSlot it = case it LMi.^. GType.iteBox . GType.boxPos of SDL.V2 x _ -> round x
        slotsUsados = map obtenerSlot itemsExistentes
        posibles = [0, 1, 2]
        slotLibre = head $ filter (\x -> not (x `elem` slotsUsados)) (posibles ++ [0])
        asignarSlot :: Int -> GType.Item
        asignarSlot s = nuevoItem LMi.& GType.iteBox . GType.boxPos LMi..~ SDL.V2 (fromIntegral s) 0

        cantInv = length inv
        inventarioLleno = cantInv >= 2 
    in 
        if idMano == 0 
        then jug LMi.& PType.jugEnt . GType.entHnd LMi..~ (asignarSlot 0)
        else if not inventarioLleno
             then jug LMi.& PType.jugEnt . GType.entInv LMi.%~ ((asignarSlot slotLibre) :)
             else 
                let slotActual = obtenerSlot mano
                in jug LMi.& PType.jugEnt . GType.entHnd LMi..~ (asignarSlot slotActual)

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> PType.Jugador -> IO ()
dibujar renderer skinTexture camPos zoom player = do
    let posJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    let tamJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxTam
    let angJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxAng
    
    GD.dibujarTextura renderer skinTexture camPos zoom posJ tamJ angJ (SDL.V4 0 200 60 255)