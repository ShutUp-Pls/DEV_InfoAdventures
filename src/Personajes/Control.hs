module Personajes.Control where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric       as LMe
import qualified Linear.Vector       as LV
import qualified Control.Monad.State as CMS
import qualified Lens.Micro          as LMi
-- Módulos propios
import qualified Types              as Input
import qualified Globals.Types      as GType
import qualified Fisica.MovEntidad  as FME
import qualified Fisica.Angulos     as FA

rutinaControl :: Input.Input -> Float -> [GType.Box] -> CMS.State GType.Entidad ()
rutinaControl input bonusCorrer mapObstaculos = do
    entidad <- CMS.get

    let dirX = (if input LMi.^. Input.derecha then 1 else 0) - (if input LMi.^. Input.izquierda then 1 else 0) :: Int
    let dirY = (if input LMi.^. Input.abajo   then 1 else 0) - (if input LMi.^. Input.arriba    then 1 else 0) :: Int
    let vecDireccion = SDL.V2 (fromIntegral dirX) (fromIntegral dirY)

    let velBase  = entidad LMi.^. GType.entMov . GType.movVel
    let facBase  = entidad LMi.^. GType.entMov . GType.movFac
    let facInput = if input LMi.^. Input.shift then bonusCorrer else 0.0
    
    let factorTotal         = facBase + facInput
    let velocidadMagnitud   = velBase * factorTotal

    let velocidadIntencion  = if vecDireccion == SDL.V2 0 0 
                                then SDL.V2 0 0 
                                else LMe.normalize vecDireccion LV.^* velocidadMagnitud

    let anguloActual = entidad LMi.^. GType.entBox . GType.boxAng
    let velRotBase   = entidad LMi.^. GType.entMov . GType.movRot
    
    let nuevoAngulo = if vecDireccion == SDL.V2 0 0
                      then anguloActual
                      else 
                          let rads = atan2 (fromIntegral dirY) (fromIntegral dirX)
                              targetAng = rads * (180 / pi)
                              diff = 180 - abs (abs (anguloActual - targetAng) - 180)
                              multiplicador = 1.0 + (diff / 180.0) * 3.0 
                              velRotFinal   = velRotBase * multiplicador

                          in FA.suavizarAngulo anguloActual targetAng velRotFinal

    CMS.modify $ \e -> e LMi.& GType.entBox . GType.boxAng LMi..~ nuevoAngulo
    FME.moverEntidad velocidadIntencion mapObstaculos