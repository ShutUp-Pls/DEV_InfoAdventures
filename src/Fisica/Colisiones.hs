module Fisica.Colisiones where

-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric  as LMe
import qualified Linear.Vector  as LV
import qualified Lens.Micro     as LMi

-- Módulos propios
import qualified Personajes.Types as PType
import qualified Globals.Types    as GType
import qualified Objetos.Types    as OType
import qualified Fisica.SAT       as FS

danoBaseEnemigo :: Float
danoBaseEnemigo = 30.0   
anguloNulo :: Float
anguloNulo = 0.0
calcularRadioAprox :: SDL.V2 Float -> Float
calcularRadioAprox (SDL.V2 w h) = (w + h) / 2

resolverCombate :: PType.Jugador -> [PType.Zombie] -> (PType.Jugador, [PType.Zombie])
resolverCombate jug enemigosList = 
    foldr logicaColision (jug, []) enemigosList

logicaColision :: PType.Zombie -> (PType.Jugador, [PType.Zombie]) -> (PType.Jugador, [PType.Zombie])
logicaColision enem (jActual, enemigosProcesados) =
    let posJ = jActual LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
        tamJ = jActual LMi.^. PType.jugEnt . GType.entBox . GType.boxTam
        
        posE = enem    LMi.^. PType.eneEnt . GType.entBox . GType.boxPos
        tamE = enem    LMi.^. PType.eneEnt . GType.entBox . GType.boxTam

    in case FS.satCollision posJ tamJ anguloNulo posE tamE anguloNulo of
        Just mtv ->
            let
                dir = LMe.normalize mtv 

                fuerzaEmpujeJ = jActual LMi.^. PType.jugEnt . GType.entEmp . GType.empFrz
                fuerzaEmpujeE = enem    LMi.^. PType.eneEnt . GType.entEmp . GType.empFrz

                nuevoVelJ = dir LV.^* fuerzaEmpujeE
                nuevoVelE = (dir LV.^* (-1)) LV.^* fuerzaEmpujeJ

                jDañado = jActual 
                    LMi.& PType.jugEnt . GType.entVid . GType.vidAct LMi.%~ (\v -> v - danoBaseEnemigo)
                    LMi.& PType.jugEnt . GType.entEmp . GType.empVec LMi..~ nuevoVelJ

                enemGolpeado = enem 
                    LMi.& PType.eneEnt . GType.entEmp . GType.empVec LMi..~ nuevoVelE
            in
                (jDañado, enemGolpeado : enemigosProcesados)

        Nothing ->
            (jActual, enem : enemigosProcesados)

resolverColisionesEnemigos :: [PType.Zombie] -> [PType.Zombie]
resolverColisionesEnemigos listaEnemigos =
    map (aplicarSeparacion listaEnemigos) listaEnemigos

aplicarSeparacion :: [PType.Zombie] -> PType.Zombie -> PType.Zombie
aplicarSeparacion todos enemigo =
    let 
        miPos   = enemigo LMi.^. PType.eneEnt . GType.entBox . GType.boxPos
        miTam   = enemigo LMi.^. PType.eneEnt . GType.entBox . GType.boxTam

        miRadFactor = 1.0
        rechazo     = 0.2
        
        miRadio = calcularRadioAprox miTam * miRadFactor

        empujeTotal = foldr (\otroEnemigo acc -> 
            let otroPos = otroEnemigo LMi.^. PType.eneEnt . GType.entBox . GType.boxPos
            in if otroPos == miPos
            then acc 
            else
                let 
                    otroTam   = otroEnemigo LMi.^. PType.eneEnt . GType.entBox . GType.boxTam
                    otroRadio = calcularRadioAprox otroTam * miRadFactor
                    
                    distanciaMinima = miRadio + otroRadio
                    vectorDif = miPos - otroPos
                    distancia = LMe.norm vectorDif
                in
                    if distancia < distanciaMinima && distancia > 0
                    then 
                        let 
                            penetracion = distanciaMinima - distancia
                            direccion = LMe.normalize vectorDif
                        in 
                            acc + (direccion LV.^* penetracion)
                    else acc
            ) (SDL.V2 0 0) todos
    in 
        enemigo LMi.& PType.eneEnt . GType.entBox . GType.boxPos LMi.%~ (+ (empujeTotal LV.^* rechazo))

checkColision :: GType.Box -> [GType.Box] -> Maybe (SDL.V2 Float)
checkColision boxEntidad obstaculos =
    let colisiones = map getMTV obstaculos
        validos = filter (/= Nothing) colisiones
    in case validos of
        (Just mtv : _) -> Just mtv
        _              -> Nothing
  where
    posE = boxEntidad LMi.^. GType.boxPos
    tamE = boxEntidad LMi.^. GType.boxTam
    angE = boxEntidad LMi.^. GType.boxAng

    getMTV boxObs =
        FS.satCollision posE tamE angE 
                        (boxObs LMi.^. GType.boxPos) 
                        (boxObs LMi.^. GType.boxTam) 
                        (boxObs LMi.^. GType.boxAng)

checkColisionsItems :: GType.Box -> [OType.ItemBuff] -> [OType.ItemBuff]
checkColisionsItems boxEntidad listaItems =
    filter estaTocando listaItems
  where
    posE = boxEntidad LMi.^. GType.boxPos
    tamE = boxEntidad LMi.^. GType.boxTam
    angE = boxEntidad LMi.^. GType.boxAng
    
    estaTocando :: OType.ItemBuff -> Bool
    estaTocando it = 
        let 
            boxIt = it LMi.^. OType.iteBox
            posI = boxIt LMi.^. GType.boxPos
            tamI = boxIt LMi.^. GType.boxTam
            angI = boxIt LMi.^. GType.boxAng
        in 
            case FS.satCollision posE tamE angE posI tamI angI of
                Just _  -> True
                Nothing -> False