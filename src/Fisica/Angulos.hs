module Fisica.Angulos where
-- Módulos del sistema
import qualified SDL
import qualified Data.Fixed as DF

degToRad :: Float -> Float
degToRad d = d * pi / 180.0

anguloAVector :: Float -> SDL.V2 Float
anguloAVector grados = 
    let radianes = degToRad grados
    in SDL.V2 (cos radianes) (sin radianes)

suavizarAngulo :: Float -> Float -> Float -> Float
suavizarAngulo actual objetivo velocidad =
    let 
        diff = objetivo - actual
        delta = (diff + 180) `DF.mod'` 360 - 180
    in 
        if abs delta < velocidad
        then objetivo
        else actual + (signum delta * velocidad)

calcularAnguloHacia :: SDL.V2 Float -> SDL.V2 Float -> Float
calcularAnguloHacia origen destino =
    let direccion = destino - origen
        (SDL.V2 dx dy) = direccion
    in atan2 dy dx * (180 / pi)