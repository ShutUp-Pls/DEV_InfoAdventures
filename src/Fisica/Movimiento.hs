module Fisica.Movimiento where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric       as LMe
import qualified Linear.Vector       as LV
import qualified Control.Monad.State as CMS
import qualified Lens.Micro          as LMi
-- Módulos propios
import qualified Globals.Types      as GType
import qualified Fisica.Colisiones  as FC

-- Constantes físicas
factorFriccion :: Float
factorFriccion = 0.90      
umbralParada :: Float
umbralParada = 0.5         
margenAntiAtasco :: Float
margenAntiAtasco = 0.01
maxPasoPixel :: Float
maxPasoPixel = 5.0

moverEntidad :: SDL.V2 Float -> [GType.Box] -> CMS.State GType.Entidad ()
moverEntidad velocidadIntencion mapObstaculos = do
    entidad <- CMS.get
    let empujeActual = entidad LMi.^. GType.entEmp . GType.empVec
        esEmpujado   = LMe.norm empujeActual > umbralParada
        velocidadFisica = if esEmpujado then empujeActual else velocidadIntencion

    let magnitud = LMe.norm velocidadFisica
        numPasos = if magnitud > 0 
                   then ceiling (magnitud / maxPasoPixel) 
                   else 1

        velPorPaso = velocidadFisica LV.^* (1.0 / fromIntegral numPasos)
        boxActual = entidad LMi.^. GType.entBox
        posInicial = boxActual LMi.^. GType.boxPos

    let (posFinal, velFinal) = simularPasos numPasos posInicial velPorPaso mapObstaculos boxActual
    let nuevoEmpuje = if esEmpujado 
                      then velFinal LV.^* factorFriccion 
                      else SDL.V2 0 0

    CMS.modify $ \e -> e 
        LMi.& GType.entBox . GType.boxPos LMi..~ posFinal
        LMi.& GType.entEmp . GType.empVec LMi..~ nuevoEmpuje
        LMi.& GType.entMov . GType.movAct LMi..~ LMe.norm velFinal

simularPasos :: Int -> SDL.V2 Float -> SDL.V2 Float -> [GType.Box] -> GType.Box -> (SDL.V2 Float, SDL.V2 Float)
simularPasos 0 posActual velActual _ _ = (posActual, velActual)
simularPasos pasosRestantes posActual velPaso mapObstaculos boxBase =
    let 
        posTentativa = posActual + velPaso
        boxTentativa = boxBase LMi.& GType.boxPos LMi..~ posTentativa
        
        maybeMTV = FC.checkColision boxTentativa mapObstaculos
    in
        case maybeMTV of
            Nothing -> 
                simularPasos (pasosRestantes - 1) posTentativa velPaso mapObstaculos boxBase
            
            Just mtv -> 
                let 
                    mtvCorregido = mtv + (LMe.normalize mtv LV.^* margenAntiAtasco)
                    posCorregida = posTentativa + mtvCorregido
                    normal = LMe.normalize mtv
                    velRestanteTotal = velPaso LV.^* fromIntegral pasosRestantes
                    proyeccion = velRestanteTotal `LMe.dot` normal
                    velRebotada = if proyeccion < 0 
                                  then velRestanteTotal - (normal LV.^* proyeccion)
                                  else velRestanteTotal

                    nuevoVelPaso = velRebotada LV.^* (1.0 / fromIntegral pasosRestantes)
                in
                    simularPasos (pasosRestantes - 1) posCorregida nuevoVelPaso mapObstaculos boxBase