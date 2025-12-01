{-# LANGUAGE OverloadedStrings #-}
module Personajes.Zombie where
-- Módulos del sistema
import qualified SDL
import qualified Control.Monad.State as CMS
import qualified Linear.Vector       as LV
import qualified Lens.Micro          as LMi
import qualified Data.Word           as DW
-- Módulos propios
import qualified Personajes.Types   as PType
import qualified Objetos.Types      as OType
import qualified Globals.Types      as GType
import qualified Personajes.IA      as IA
import qualified Objetos.Particula  as OPart
import qualified Graficos.Dibujado  as GD
import qualified Fisica.Colisiones  as FC

idZmbBasico, idZmbCorredor, idZmbTanque :: Int
idZmbBasico   = 3000
idZmbCorredor = 3001
idZmbTanque   = 3002


crearBoxZombie :: Int -> SDL.V2 Float -> GType.Box
crearBoxZombie zId pos
    | zId == idZmbBasico   = mkBox (SDL.V2 30 30) 15.0
    | zId == idZmbCorredor = mkBox (SDL.V2 24 24) 13.0
    | zId == idZmbTanque   = mkBox (SDL.V2 40 40) 20.0
    | otherwise            = error "crearBoxZombieTipo: id de zombie desconocido"
  where
    mkBox tam rad = GType.Box
        { GType._boxPos = pos
        , GType._boxTam = tam
        , GType._boxAng = 0.0
        , GType._boxRad = rad
        }


crearStatsMovimientoZombie :: Int -> GType.Movimiento
crearStatsMovimientoZombie zId
    | zId == idZmbBasico   = mkMov 2.5  15.0 1.0
    | zId == idZmbCorredor = mkMov 3.5  22.0 1.0
    | zId == idZmbTanque   = mkMov 1.5  10.0 1.0
    | otherwise            = error "crearStatsMovimientoZombieTipo: id de zombie desconocido"
  where
    mkMov vel rot fac = GType.Movimiento
        { GType._movVel = vel
        , GType._movRot = rot
        , GType._movFac = fac
        , GType._movAct = 0.0
        }

crearStatsVidaZombie :: Int -> GType.Vida
crearStatsVidaZombie zId
    | zId == idZmbBasico   = mkVida 100.0
    | zId == idZmbCorredor = mkVida  70.0
    | zId == idZmbTanque   = mkVida 220.0
    | otherwise            = error "crearStatsVidaZombieTipo: id de zombie desconocido"
  where
    mkVida v = GType.Vida
        { GType._vidAct = v
        , GType._vidMax = v
        , GType._vidMrt = 0
        }

crearEmpujeZombie :: Int -> GType.Empuje
crearEmpujeZombie zId
    | zId == idZmbBasico   = mkEmp 5.0
    | zId == idZmbCorredor = mkEmp 3.0
    | zId == idZmbTanque   = mkEmp 8.0
    | otherwise            = error "crearEmpujeZombieTipo: id de zombie desconocido"
  where
    mkEmp frz = GType.Empuje
        { GType._empVec = SDL.V2 0 0
        , GType._empFrz = frz
        }

crearEntidadZombie :: Int -> SDL.V2 Float -> GType.Entidad
crearEntidadZombie zId pos = GType.Entidad
    { GType._entBox = crearBoxZombie                zId pos
    , GType._entMov = crearStatsMovimientoZombie    zId
    , GType._entVid = crearStatsVidaZombie          zId
    , GType._entEmp = crearEmpujeZombie             zId
    , GType._entBuf = []
    , GType._entInv = []
    , GType._entHnd = GType.itemVacio
    }

crearZombie :: Int -> SDL.V2 Float -> PType.Zombie
crearZombie zId pos = 
    let (radVis, dano, teamR) =
            if      zId == idZmbBasico
            then (1000.0, 20.0, 1.0)
            else if zId == idZmbCorredor
            then (2000.0, 15.0, 0.8)
            else if zId == idZmbTanque
            then (800.0, 35.0, 1.3)
            else error "crearZombieTipo: id de zombie desconocido"
    in PType.Zombie
        { PType._zmbEnt    = crearEntidadZombie zId pos
        , PType._zmbRadVis = radVis
        , PType._zmbVerJug = False
        , PType._zmbDamage = dano
        , PType._zmbTeamRd = teamR
        , PType._zmdId     = zId
        }

moverZombie :: PType.Zombie -> [GType.Box] -> PType.Jugador -> PType.Zombie
moverZombie zombieActual mapObstaculos player = 
    let 
        entidadInicial = zombieActual   LMi.^. PType.zmbEnt
        rangoVis       = zombieActual   LMi.^. PType.zmbRadVis
        posZombie      = entidadInicial LMi.^. GType.entBox . GType.boxPos
        posJugador     = player         LMi.^. PType.jugEnt . GType.entBox . GType.boxPos

        detectado = IA.enRangoDeVision posZombie rangoVis posJugador
        entidadFinal = CMS.execState (IA.rutinaPersecucion posJugador detectado mapObstaculos) entidadInicial
    in
        zombieActual 
            LMi.& PType.zmbEnt LMi..~ entidadFinal
            LMi.& PType.zmbVerJug LMi..~ detectado

updateEnemies :: PType.Jugador -> [PType.Zombie] -> [GType.Box] -> [PType.Zombie]
updateEnemies jug enemigos mapa =
    let zombiesMovidos = map (\enemy -> moverZombie enemy mapa jug) enemigos
    in resolverColisionesEntreZombies zombiesMovidos

limpiarZombiesMuertos :: [PType.Zombie] -> [PType.Zombie]
limpiarZombiesMuertos = filter (\z -> (z LMi.^. PType.zmbEnt . GType.entVid . GType.vidAct) > 0)

gestionarCombate :: [OType.Particula] -> [PType.Zombie] -> ([OType.Particula], [PType.Zombie])
gestionarCombate balas zombies =
    let 
        (balasRestantes, zombiesDañados) = resolverImpactos balas zombies
        zombiesVivos = limpiarZombiesMuertos zombiesDañados
    in 
        (balasRestantes, zombiesVivos)

resolverColisionesEntreZombies :: [PType.Zombie] -> [PType.Zombie]
resolverColisionesEntreZombies zombies =
    let entidadesGenericas = map (LMi.^. PType.zmbEnt) zombies
    in map (\z -> z LMi.& PType.zmbEnt LMi.%~ (FC.separarEntidades (z LMi.& PType._zmbTeamRd) entidadesGenericas)) zombies

dañarZombieEnIndice :: Int -> Float -> [PType.Zombie] -> [PType.Zombie]
dañarZombieEnIndice idx dano zombies =
    take idx zombies ++ 
    [zombies !! idx LMi.& PType.zmbEnt . GType.entVid . GType.vidAct LMi.%~ (\v -> v - dano)] ++ 
    drop (idx + 1) zombies

resolverImpactos :: [OType.Particula] -> [PType.Zombie] -> ([OType.Particula], [PType.Zombie])
resolverImpactos balas enemigos = 
    foldl procesarBala ([], enemigos) balas
  where
    procesarBala (balasVivas, zombiesActuales) bala =
        let 
            entBala = bala LMi.^. OType.parEnt
            boxBala = entBala LMi.^. GType.entBox
            angBala = entBala LMi.^. GType.entBox . GType.boxAng
            parID = determinarIdParticula entBala
            
            impacto = checkImpactoUnico boxBala zombiesActuales
        in case impacto of
            Nothing -> (bala : balasVivas, zombiesActuales) 
            Just (zombieIndex, _) -> 
                let zombiesDañados = aplicarImpactoZombie 
                                        zombieIndex 
                                        parID
                                        angBala 
                                        zombiesActuales
                in (balasVivas, zombiesDañados)

determinarIdParticula :: GType.Entidad -> Int
determinarIdParticula ent =
    let vel = ent LMi.^. GType.entMov . GType.movVel
        vidaMax = ent LMi.^. GType.entVid . GType.vidMax
    in if vel == OPart.velocidadBala && vidaMax == OPart.vidaBala 
       then OPart.idParBala
       else OPart.idParFuego

aplicarImpactoZombie :: Int -> Int -> Float -> [PType.Zombie] -> [PType.Zombie]
aplicarImpactoZombie idxZombie parId anguloBala zombies =
    let 
        (dano, fuerzaEmpuje) = case parId of
            _ | parId == OPart.idParBala -> (25.0, OPart.fuerzaBala)
            _ | parId == OPart.idParFuego -> (5.0, OPart.fuerzaFuego)
            _ -> (0.0, 0.0)

        zombieTarget = zombies !! idxZombie
        angleRad = anguloBala * (pi / 180.0)
        dirEmpuje = SDL.V2 (cos angleRad) (sin angleRad)
        vecFuerza = dirEmpuje LV.^* fuerzaEmpuje

        zombieModificado = zombieTarget
            LMi.& PType.zmbEnt . GType.entVid . GType.vidAct LMi.%~ (\v -> v - dano)
            LMi.& PType.zmbEnt . GType.entEmp . GType.empVec LMi.%~ (+ vecFuerza)
            LMi.& PType.zmbVerJug LMi..~ True 

    in 
        take idxZombie zombies ++ [zombieModificado] ++ drop (idxZombie + 1) zombies

checkImpactoUnico :: GType.Box -> [PType.Zombie] -> Maybe (Int, PType.Zombie)
checkImpactoUnico boxBala zombies = 
    go 0 zombies
  where
    go _ [] = Nothing
    go idx (z:zs) =
        let boxZ = z LMi.^. PType.zmbEnt . GType.entBox
        in case FC.mtvBoxes boxBala boxZ of
            Just _  -> Just (idx, z)
            Nothing -> go (idx + 1) zs

colorZombie :: Int -> SDL.V4 DW.Word8
colorZombie zId
    | zId == idZmbBasico   = SDL.V4 200  30  30 255
    | zId == idZmbCorredor = SDL.V4  30 200  30 255
    | zId == idZmbTanque   = SDL.V4  60  60 200 255
    | otherwise            = SDL.V4 255 255 255 255

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> PType.Zombie -> IO ()
dibujar renderer skinTexture camPos zoom enem = do
    let posE = enem LMi.^. PType.zmbEnt . GType.entBox . GType.boxPos
    let tamE = enem LMi.^. PType.zmbEnt . GType.entBox . GType.boxTam
    let angE = enem LMi.^. PType.zmbEnt . GType.entBox . GType.boxAng
    let col  = colorZombie (enem LMi.^. PType.zmdId)

    GD.dibujarTextura renderer skinTexture camPos zoom posE tamE angE col