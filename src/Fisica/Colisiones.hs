module Fisica.Colisiones where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric          as LMe
import qualified Linear.Vector          as LV
import qualified Lens.Micro             as LMi
-- Módulos propios
import qualified Globals.Types          as GType
import qualified Fisica.SAT             as FS  

obtenerRadio :: GType.Box -> Float
obtenerRadio box = 
    let r = box LMi.^. GType.boxRad
    in if r > 0 
       then r 
       else LMe.norm (box LMi.^. GType.boxTam) * 0.5

mtvBoxes :: GType.Box -> GType.Box -> Maybe (SDL.V2 Float)
mtvBoxes boxA boxB =
    let 
        halfA   = (boxA LMi.^. GType.boxTam) LV.^* 0.5
        halfB   = (boxB LMi.^. GType.boxTam) LV.^* 0.5
        centerA = (boxA LMi.^. GType.boxPos) + halfA
        centerB = (boxB LMi.^. GType.boxPos) + halfB

        radA = obtenerRadio boxA
        radB = obtenerRadio boxB
        
        distCuadrada = LMe.quadrance (centerA - centerB)
        radiosSum    = radA + radB
        
    in 
    if distCuadrada > (radiosSum * radiosSum) 
        then Nothing
        else 
            let posA = boxA LMi.^. GType.boxPos
                tamA = boxA LMi.^. GType.boxTam
                angA = boxA LMi.^. GType.boxAng
                
                posB = boxB LMi.^. GType.boxPos
                tamB = boxB LMi.^. GType.boxTam
                angB = boxB LMi.^. GType.boxAng
            in FS.satCollision posA tamA angA posB tamB angB

separarEntidades :: Float -> [GType.Entidad] -> GType.Entidad -> GType.Entidad
separarEntidades rechazo obstaculos entidad =
    let 
        boxEnt      = entidad LMi.^. GType.entBox
        halfEnt     = (boxEnt LMi.^. GType.boxTam) LV.^* 0.5
        miPosCentro = (boxEnt LMi.^. GType.boxPos) + halfEnt
        miRadio     = obtenerRadio boxEnt
        
        empujeTotal = foldr (calcEmpujeIndividual miPosCentro miRadio) (SDL.V2 0 0) obstaculos
    in 
        entidad LMi.& GType.entBox . GType.boxPos LMi.%~ (+ (empujeTotal LV.^* rechazo))
  
calcEmpujeIndividual :: SDL.V2 Float -> Float -> GType.Entidad -> SDL.V2 Float -> SDL.V2 Float
calcEmpujeIndividual miPos miRadio otroEnemigo acc =
    let 
        boxOtro   = otroEnemigo LMi.^. GType.entBox
        halfOtro  = (boxOtro LMi.^. GType.boxTam) LV.^* 0.5
        otroPos   = (boxOtro LMi.^. GType.boxPos) + halfOtro
    in if otroPos == miPos then acc else
        let
            otroRadio = obtenerRadio boxOtro
            vectorDif = miPos - otroPos
            
            distCuadrada    = LMe.quadrance vectorDif
            minDist         = miRadio + otroRadio
            minDistCuadrada = minDist * minDist
        in
            if distCuadrada < minDistCuadrada && distCuadrada > 0.0001
            then
                let 
                    distancia   = sqrt distCuadrada 
                    penetracion = minDist - distancia
                    direccion   = vectorDif LV.^* (1.0 / distancia)
                in acc + (direccion LV.^* penetracion)
            else acc

checkColision :: GType.Box -> [GType.Box] -> Maybe (SDL.V2 Float)
checkColision boxEntidad obstaculos =
    case [ mtv | Just mtv <- map (mtvBoxes boxEntidad) obstaculos ] of
        (mtv : _) -> Just mtv
        _         -> Nothing

checkColisionsItems :: GType.Box -> [GType.Item] -> [GType.Item]
checkColisionsItems boxEntidad listaItems = filter estaTocando listaItems
  where
    estaTocando :: GType.Item -> Bool
    estaTocando it = 
        let boxIt = it LMi.^. GType.iteBox
        in case mtvBoxes boxEntidad boxIt of
            Just _  -> True
            Nothing -> False