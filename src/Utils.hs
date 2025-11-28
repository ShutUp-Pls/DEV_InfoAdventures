module Utils where

-- Módulos del sistema
import qualified SDL
import qualified Foreign.C.Types as FCT
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Módulos propios
import qualified Types

-- Convertir coordenadas de Float a Rectangulo CInt
toSDLRect :: SDL.V2 Float -> SDL.V2 Float -> SDL.Rectangle FCT.CInt
toSDLRect (SDL.V2 x y) (SDL.V2 w h) = 
    SDL.Rectangle (SDL.P (SDL.V2 (round x) (round y))) (SDL.V2 (round w) (round h))

-- Calculamos un vector normalizado a partir de los input del teclado
vectorInput :: Types.Input -> Float -> Float -> SDL.V2 Float
vectorInput input velocidad factor =
    let 
        -- Obtenemos la dirección pura (solo -1, 0 o 1)
        dirX = (if Types.derecha input then 1 else 0) - (if Types.izquierda input then 1 else 0)
        dirY = (if Types.abajo input then 1 else 0)  - (if Types.arriba input then 1 else 0)
        direccion = SDL.V2 dirX dirY
    in 
        -- Verificamos si hay movimiento para evitar dividir por cero al normalizar
        if direccion == SDL.V2 0 0 
            then SDL.V2 0 0 
            else LM.normalize direccion LV.^* (velocidad*factor)

vectorHacia :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float
vectorHacia posOrigen posDestino magnitud =
    let direccion = posDestino - posOrigen
    in if direccion == SDL.V2 0 0 -- Evitar división por cero si ya llegó
       then SDL.V2 0 0
       else LM.normalize direccion LV.^* magnitud