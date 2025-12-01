module Personajes.IA where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric  as LMe
import qualified Linear.Vector  as LV
import qualified Lens.Micro     as LMi
import qualified Control.Monad.State as CMS
-- Módulos propios
import qualified Fisica.Angulos     as FA
import qualified Fisica.MovEntidad  as FMW
import qualified Globals.Types      as GType

enRangoDeVision :: SDL.V2 Float -> Float -> SDL.V2 Float -> Bool
enRangoDeVision posObservador rangoVision posObjetivo =
    LMe.distance posObservador posObjetivo < rangoVision

rutinaPersecucion :: SDL.V2 Float -> Bool -> [GType.Box] -> CMS.State GType.Entidad ()
rutinaPersecucion posObjetivo detectado mapObstaculos = do
    entidad <- CMS.get
    let posEnt    = entidad LMi.^. GType.entBox . GType.boxPos
    let angActual = entidad LMi.^. GType.entBox . GType.boxAng
    let rotSpeed  = entidad LMi.^. GType.entMov . GType.movRot
    
    let targetAng = FA.calcularAnguloHacia posEnt posObjetivo
    
    let nuevoAngulo = if detectado
                      then FA.suavizarAngulo angActual targetAng rotSpeed
                      else angActual

    CMS.modify $ \e -> e LMi.& GType.entBox . GType.boxAng LMi..~ nuevoAngulo

    let velBase = entidad LMi.^. GType.entMov . GType.movVel
    let facBase = entidad LMi.^. GType.entMov . GType.movFac

    let magnitud = if detectado
                   then velBase * facBase
                   else 0
    
    let forwardDir = FA.anguloAVector nuevoAngulo
    let velocidadIntencion = forwardDir LV.^* magnitud

    FMW.moverEntidad velocidadIntencion mapObstaculos