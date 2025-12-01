module Fisica.Colisiones where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric          as LMe
import qualified Linear.Vector          as LV
import qualified Lens.Micro             as LMi
-- Módulos propios
import qualified Globals.Types          as GType
import qualified Fisica.SAT             as FS  

calcularRadioAprox :: SDL.V2 Float -> Float
calcularRadioAprox (SDL.V2 w h) = (w + h) / 2

mtvBoxes :: GType.Box -> GType.Box -> Maybe (SDL.V2 Float)
mtvBoxes boxA boxB =
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
        miPos       = entidad LMi.^. GType.entBox . GType.boxPos
        miRadio     = entidad LMi.^. GType.entBox . GType.boxRad
        
        empujeTotal = foldr (calcEmpujeIndividual miPos miRadio) (SDL.V2 0 0) obstaculos
    in 
        entidad LMi.& GType.entBox . GType.boxPos LMi.%~ (+ (empujeTotal LV.^* rechazo))
  
calcEmpujeIndividual :: SDL.V2 Float -> Float -> GType.Entidad -> SDL.V2 Float -> SDL.V2 Float
calcEmpujeIndividual miPos miRadio otroEnemigo acc =
    let otroPos = otroEnemigo LMi.^. GType.entBox . GType.boxPos
    in if otroPos == miPos then acc else
        let
            otroRadio = otroEnemigo LMi.^. GType.entBox . GType.boxRad
            
            distanciaMinima = miRadio + otroRadio
            vectorDif       = miPos - otroPos
            distancia       = LMe.norm vectorDif
        in
            if distancia < distanciaMinima && distancia > 0.001
            then
                let penetracion = distanciaMinima - distancia
                    direccion   = LMe.normalize vectorDif
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