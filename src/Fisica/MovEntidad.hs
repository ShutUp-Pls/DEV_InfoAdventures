module Fisica.MovEntidad where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric      as LMe
import qualified Linear.Vector      as LV
import qualified Lens.Micro         as LMi
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType

import qualified Fisica.Colisiones  as FCol
import qualified Fisica.Angulos     as FAng

factorFriccion, umbralParada, maxPasoPixel, margenAntiAtasco :: Float
factorFriccion = 0.90      
umbralParada = 0.5         
maxPasoPixel = 5.0
margenAntiAtasco = 0.01

moverEntidad :: SDL.V2 Float -> [GType.Box] -> GType.Entidad -> GType.Entidad
moverEntidad velIntencion mapa entidad =
    let empujeActual              = entidad LMi.^. GType.entEmp . GType.empVec
        boxActual                 = entidad LMi.^. GType.entBox
        (velFisica, esEmpujado)   = calcularVelocidadFisica velIntencion empujeActual
        (numPasos, velPorPaso)    = calcularPasos velFisica
        (posFinal, velFinal)      = simularMovimiento numPasos velPorPaso mapa boxActual

        nuevoEmpuje =
            if esEmpujado
            then velFinal LV.^* factorFriccion
            else SDL.V2 0 0

        entidadFinal =
            entidad
              LMi.& GType.entBox . GType.boxPos LMi..~ posFinal
              LMi.& GType.entEmp . GType.empVec LMi..~ nuevoEmpuje
              LMi.& GType.entMov . GType.movAct LMi..~ LMe.norm velFinal
    in entidadFinal

calcularVelocidadFisica :: SDL.V2 Float  -> SDL.V2 Float -> (SDL.V2 Float, Bool)
calcularVelocidadFisica velIntencion empujeActual =
    let esEmpujado = LMe.norm empujeActual > umbralParada
        velFisica  = if esEmpujado then empujeActual else velIntencion
    in (velFisica, esEmpujado)

calcularPasos :: SDL.V2 Float -> (Int, SDL.V2 Float)
calcularPasos velFisica =
    let magnitud   = LMe.norm velFisica
        numPasos   = if magnitud > 0
                     then ceiling (magnitud / maxPasoPixel)
                     else 1
        velPorPaso = velFisica LV.^* (1.0 / fromIntegral numPasos)
    in (numPasos, velPorPaso)

simularMovimiento :: Int -> SDL.V2 Float -> [GType.Box] -> GType.Box -> (SDL.V2 Float, SDL.V2 Float)
simularMovimiento numPasos velPorPaso mapa boxActual =
    let posInicial = boxActual LMi.^. GType.boxPos
    in  simularPasos numPasos posInicial velPorPaso mapa boxActual

simularPasos :: Int -> SDL.V2 Float -> SDL.V2 Float -> [GType.Box] -> GType.Box -> (SDL.V2 Float, SDL.V2 Float)
simularPasos 0 posActual velActual _ _ = (posActual, velActual)
simularPasos pasosRestantes posActual velPaso mapa boxBase =
    let posTentativa = posActual + velPaso
        boxTentativa = boxBase LMi.& GType.boxPos LMi..~ posTentativa
    in case FCol.checkColision boxTentativa mapa of
        Nothing -> simularPasos (pasosRestantes - 1) posTentativa velPaso mapa boxBase
        Just mtv ->
            let (posCorregida, nuevoVelPaso) = resolverColision pasosRestantes posTentativa velPaso mtv
            in simularPasos (pasosRestantes - 1) posCorregida nuevoVelPaso mapa boxBase    

resolverColision :: Int -> SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float -> (SDL.V2 Float, SDL.V2 Float) 
resolverColision pasosRestantes posTentativa velPaso mtv =
    let mtvCorregido      = mtv + (LMe.normalize mtv LV.^* margenAntiAtasco)
        posCorregida      = posTentativa + mtvCorregido

        normal            = LMe.normalize mtv
        velRestanteTotal  = velPaso LV.^* fromIntegral pasosRestantes
        velRebotada       = rebotarVelocidad normal velRestanteTotal

        pasosRestantes'   = pasosRestantes - 1
        nuevoVelPaso      = velRebotada LV.^* (1.0 / fromIntegral (max 1 pasosRestantes'))
    in (posCorregida, nuevoVelPaso)
    

rebotarVelocidad :: SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float
rebotarVelocidad normal velRestanteTotal =
    let proyeccion = velRestanteTotal `LMe.dot` normal
    in if proyeccion < 0
       then velRestanteTotal - (normal LV.^* proyeccion)
       else velRestanteTotal

rutinaPersecucion :: SDL.V2 Float -> Bool -> [GType.Box] -> GType.Entidad -> GType.Entidad
rutinaPersecucion posObjetivo detectado mapObstaculos entidad =
    let 
        entidadOrientada = girarHaciaObjetivo posObjetivo detectado entidad
        velIntencion     = calcularIntencionMovimiento detectado entidadOrientada
    in  moverEntidad velIntencion mapObstaculos entidadOrientada

girarHaciaObjetivo :: SDL.V2 Float -> Bool -> GType.Entidad -> GType.Entidad
girarHaciaObjetivo posObjetivo detectado entidad =
    let 
        posEnt    = entidad LMi.^. GType.entBox . GType.boxPos
        angActual = entidad LMi.^. GType.entBox . GType.boxAng
        
        nuevoAngulo = 
            if detectado
            then 
                let rotSpeed  = entidad LMi.^. GType.entMov . GType.movRot
                    targetAng = FAng.calcularAnguloHacia posEnt posObjetivo
                in FAng.suavizarAngulo angActual targetAng rotSpeed
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
            vectorDir = FAng.anguloAVector angulo
        in  vectorDir LV.^* magnitud
    else SDL.V2 0 0

girarEntidadPorTeclado :: Types.Input -> GType.Entidad -> GType.Entidad
girarEntidadPorTeclado input entidad =
    let dirX = (if input LMi.^. Types.derecha then 1 else 0) - (if input LMi.^. Types.izquierda then 1 else 0) :: Int
        dirY = (if input LMi.^. Types.abajo   then 1 else 0) - (if input LMi.^. Types.arriba  then 1 else 0) :: Int
        vecDireccion = SDL.V2 (fromIntegral dirX) (fromIntegral dirY) :: SDL.V2 Float

        anguloActual = entidad LMi.^. GType.entBox . GType.boxAng
        velRotBase   = entidad LMi.^. GType.entMov . GType.movRot
        nuevoAngulo =
            if vecDireccion == SDL.V2 0 0
            then anguloActual
            else
                let rads        = atan2 (fromIntegral dirY) (fromIntegral dirX)
                    targetAng   = rads * (180 / pi)
                    diff        = FAng.diferenciaAngular anguloActual targetAng
                    multiplicador = 1.0 + (diff / 180.0) * 3.0
                    velRotFinal   = velRotBase * multiplicador
                in FAng.suavizarAngulo anguloActual targetAng velRotFinal
    in
    entidad LMi.& GType.entBox . GType.boxAng LMi..~ nuevoAngulo