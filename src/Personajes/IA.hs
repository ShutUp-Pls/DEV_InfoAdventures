module Personajes.IA where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric  as LMe
import qualified Linear.Vector  as LV
import qualified Lens.Micro     as LMi
-- Módulos propios
import qualified Fisica.Angulos     as FA
import qualified Fisica.MovEntidad  as FMW
import qualified Globals.Types      as GType

rutinaPersecucion :: SDL.V2 Float -> Bool -> [GType.Box] -> GType.Entidad -> GType.Entidad
rutinaPersecucion posObjetivo detectado mapObstaculos entidad =
    let 
        entidadOrientada = girarHaciaObjetivo posObjetivo detectado entidad
        velIntencion     = calcularIntencionMovimiento detectado entidadOrientada
    in  FMW.moverEntidad velIntencion mapObstaculos entidadOrientada

girarHaciaObjetivo :: SDL.V2 Float -> Bool -> GType.Entidad -> GType.Entidad
girarHaciaObjetivo posObjetivo detectado entidad =
    let 
        posEnt    = entidad LMi.^. GType.entBox . GType.boxPos
        angActual = entidad LMi.^. GType.entBox . GType.boxAng
        
        nuevoAngulo = 
            if detectado
            then 
                let rotSpeed  = entidad LMi.^. GType.entMov . GType.movRot
                    targetAng = FA.calcularAnguloHacia posEnt posObjetivo
                in FA.suavizarAngulo angActual targetAng rotSpeed
            else angActual
    in entidad LMi.& GType.entBox . GType.boxAng LMi..~ nuevoAngulo

calcularIntencionMovimiento :: Bool -> GType.Entidad -> SDL.V2 Float
calcularIntencionMovimiento detectado entidad =
    if detectado
    then 
        let 
            angulo   = entidad LMi.^. GType.entBox . GType.boxAng
            velBase  = entidad LMi.^. GType.entMov . GType.movVel
            facBase  = entidad LMi.^. GType.entMov . GType.movFac
            magnitud = velBase * facBase
            vectorDir = FA.anguloAVector angulo
        in  vectorDir LV.^* magnitud
    else SDL.V2 0 0