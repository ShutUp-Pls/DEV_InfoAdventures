module Fisica.Vectores where
-- Módulos del Sistema
import qualified SDL
import qualified Linear.Vector  as LV
import qualified Linear.Metric  as LM
import qualified Lens.Micro     as LMi
-- Módulos propios
import qualified Types

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