{-# LANGUAGE OverloadedStrings #-}
module Personajes.Jugador where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro             as LMi
import qualified Lens.Micro.Mtl         as LMi
import qualified Linear.Vector          as LV
import qualified Control.Monad.State    as CMS
-- Módulos propios
import qualified Types
import qualified Globals.Types          as GType
import qualified Personajes.Types       as PType
import qualified Objetos.Buff           as OBuff

import qualified Graficos.Dibujado      as GD

import qualified Fisica.Angulos         as FAng
import qualified Fisica.MovEntidad      as FMen

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
    , GType._movRot = 3.0 
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
    , PType._estaVivo   = True
    }

moverJugadorP :: Types.Input -> PType.Jugador -> [GType.Box] -> PType.Jugador
moverJugadorP input jugadorActual mapObstaculos = CMS.execState (moverJugador input mapObstaculos) jugadorActual

moverJugador :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
moverJugador input mapObstaculos = do
    girarJugadorM input
    desplazarJugadorM input mapObstaculos

girarJugadorM :: Types.Input -> CMS.State PType.Jugador ()
girarJugadorM input = do
    entidad <- CMS.gets (LMi.^. PType.jugEnt)
    
    let dirX = (if input LMi.^. Types.derecha then 1 else 0) - (if input LMi.^. Types.izquierda then 1 else 0) :: Int
        dirY = (if input LMi.^. Types.abajo   then 1 else 0) - (if input LMi.^. Types.arriba  then 1 else 0) :: Int
        vecDireccion = SDL.V2 (fromIntegral dirX) (fromIntegral dirY) :: SDL.V2 Float

        anguloActual = entidad LMi.^. GType.entBox . GType.boxAng
        velRotBase   = entidad LMi.^. GType.entMov . GType.movRot

    let nuevoAngulo =
            if vecDireccion == SDL.V2 0 0
            then anguloActual
            else
                let rads        = atan2 (fromIntegral dirY) (fromIntegral dirX)
                    targetAng   = rads * (180 / pi)
                    diff        = FAng.diferenciaAngular anguloActual targetAng
                    multiplicador = 1.0 + (diff / 180.0) * 5.0
                    velRotFinal   = velRotBase * multiplicador
                in FAng.suavizarAngulo anguloActual targetAng velRotFinal

    PType.jugEnt . GType.entBox . GType.boxAng LMi..= nuevoAngulo

desplazarJugadorM :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
desplazarJugadorM input mapObstaculos = do
    jugador <- CMS.get

    let entidad      = jugador LMi.^. PType.jugEnt
        runFactor    = jugador LMi.^. PType.factCorrer
        anguloActual = entidad LMi.^. GType.entBox . GType.boxAng
        velBase      = entidad LMi.^. GType.entMov . GType.movVel
        velFactor    = entidad LMi.^. GType.entMov . GType.movFac

    let dirX = (if input LMi.^. Types.derecha then 1 else 0) - (if input LMi.^. Types.izquierda then 1 else 0) :: Int
        dirY = (if input LMi.^. Types.abajo   then 1 else 0) - (if input LMi.^. Types.arriba  then 1 else 0) :: Int
        hayMovimiento = dirX /= 0 || dirY /= 0
        estaCorriendo = input LMi.^. Types.shift
        multCorrer    = if estaCorriendo then runFactor else 1.0
        ventanaTolerancia = 90.0

        magnitud = 
            if not hayMovimiento 
            then 0.0 
            else
                let velocidadFinal = velBase * multCorrer * velFactor
                in if estaCorriendo
                then velocidadFinal
                else
                    let rads      = atan2 (fromIntegral dirY) (fromIntegral dirX)
                        targetAng = rads * (180 / pi)
                        diff      = FAng.diferenciaAngular anguloActual targetAng
                        factorAlineacion = 
                            if diff >= ventanaTolerancia
                            then 0.0
                            else (ventanaTolerancia - diff) / ventanaTolerancia
                    in velocidadFinal * factorAlineacion

    let vecDir       = FAng.anguloAVector anguloActual
    let velIntencion = vecDir LV.^* magnitud
    let entidadMovida = FMen.moverEntidad velIntencion mapObstaculos entidad
    let entidadFinal = entidadMovida LMi.& GType.entMov . GType.movAct LMi..~ magnitud
    
    PType.jugEnt LMi..= entidadFinal

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
    
    if GD.esVisible posJ tamJ angJ camPos zoom 
        then do
            GD.dibujarTextura renderer skinTexture camPos zoom posJ tamJ angJ (SDL.V4 0 200 60 255)
        else return()

aplicarEfecto :: GType.Item -> PType.Jugador -> PType.Jugador
aplicarEfecto item j =
    let maybeBuff = item LMi.^? GType.iteTipo . GType._EsBuff
    in case maybeBuff of
        Nothing -> j
        Just buffOriginal ->
            let bid = buffOriginal LMi.^. GType.bufID
                val = buffOriginal LMi.^. GType.bufVlr
                currentBuffs = j LMi.^. PType.jugEnt . GType.entBuf
            in
                if bid `elem` OBuff.idsVida then
                    j LMi.& PType.jugEnt . GType.entVid . GType.vidAct
                        LMi.%~ (\v -> min 100 (v + val))
                else if bid `elem` OBuff.idsTiempo then
                    j
                else
                    let nuevosBuffs = OBuff.agregarBuff buffOriginal currentBuffs
                    in  j LMi.& PType.jugEnt . GType.entBuf LMi..~ nuevosBuffs

procesarBuffs :: Float -> PType.Jugador -> PType.Jugador
procesarBuffs dt jug = 
    let 
        buffsActuales  = jug LMi.^. PType.jugEnt . GType.entBuf
        buffsRestantes = map (\b -> b LMi.& GType.bufTmp LMi.%~ (\t -> t - dt)) buffsActuales
        buffsVivos     = filter (\b -> (b LMi.^. GType.bufTmp) > 0) buffsRestantes
        buffsVel  = filter (\b -> (b LMi.^. GType.bufID) `elem` OBuff.idsVelocidad) buffsVivos
        factorVel = case buffsVel of
            [] -> 1.0
            (b:_) -> b LMi.^. GType.bufVlr

    in jug 
        LMi.& PType.jugEnt . GType.entBuf LMi..~ buffsVivos
        LMi.& PType.jugEnt . GType.entMov . GType.movFac LMi..~ factorVel