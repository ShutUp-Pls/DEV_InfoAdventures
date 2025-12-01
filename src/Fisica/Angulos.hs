module Fisica.Angulos where
-- Módulos del sistema
import qualified SDL
import qualified Data.Fixed as DF

anguloAVector :: Float -> SDL.V2 Float
anguloAVector grados = 
    let radianes = grados * pi / 180.0
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

diferenciaAngular :: Float -> Float -> Float
diferenciaAngular a1 a2 =
    let 
        norm a = a - (fromIntegral (floor (a / 360.0) :: Int) * 360.0)
        a1' = norm a1
        a2' = norm a2
        diff = abs (a1' - a2')
    in  if diff > 180 then 360 - diff else diff