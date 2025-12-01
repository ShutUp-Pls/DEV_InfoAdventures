module Fisica.Vectores where
-- Módulos del Sistema
import qualified SDL
import qualified Linear.Vector  as LV
import qualified Linear.Metric  as LM
import qualified Lens.Micro     as LMi
-- Módulos propios
import qualified Types

import qualified Fisica.Angulos as FAng

vectorInput :: Types.Input -> Float -> Float -> SDL.V2 Float
vectorInput input velocidad factor =
    let 
        isRight = input LMi.^. Types.derecha
        isLeft  = input LMi.^. Types.izquierda
        isDown  = input LMi.^. Types.abajo
        isUp    = input LMi.^. Types.arriba

        dirX = (if isRight then 1 else 0) - (if isLeft then 1 else 0)
        dirY = (if isDown  then 1 else 0) - (if isUp   then 1 else 0)
        direccion = SDL.V2 dirX dirY
    in 
        if direccion == SDL.V2 0 0 
            then SDL.V2 0 0 
            else LM.normalize direccion LV.^* (velocidad * factor)

vectorHacia :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float
vectorHacia posOrigen posDestino magnitud =
    let direccion = posDestino - posOrigen
    in if direccion == SDL.V2 0 0 
       then SDL.V2 0 0
       else LM.normalize direccion LV.^* magnitud

magnitudPorTeclado :: Types.Input -> Float -> Float -> Float -> Float
magnitudPorTeclado input velBase runFactor anguloActual =
    let 
        dirX = (if input LMi.^. Types.derecha then 1 else 0) - (if input LMi.^. Types.izquierda then 1 else 0) :: Int
        dirY = (if input LMi.^. Types.abajo   then 1 else 0) - (if input LMi.^. Types.arriba  then 1 else 0) :: Int
        
        hayMovimiento = dirX /= 0 || dirY /= 0
        estaCorriendo = input LMi.^. Types.shift
        multCorrer    = if estaCorriendo then runFactor else 1.0

        ventanaTolerancia = 45
        
        factorAlineacion = 
            if not hayMovimiento 
            then 0.0 
            else
                if estaCorriendo
                then 1.0
                else
                    let 
                        rads      = atan2 (fromIntegral dirY) (fromIntegral dirX)
                        targetAng = rads * (180 / pi)

                        diff      = FAng.diferenciaAngular anguloActual targetAng
                    in
                        if diff >= ventanaTolerancia
                        then 0.0
                        else (ventanaTolerancia - diff) / ventanaTolerancia

    in velBase * multCorrer * factorAlineacion