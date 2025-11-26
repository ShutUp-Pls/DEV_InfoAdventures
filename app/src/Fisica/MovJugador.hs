module Fisica.MovJugador where

import Linear.V2 (V2(..))
import Linear.Metric (normalize, norm)
import Linear.Vector ((^*))

import Types
import Fisica.Colisiones(checkColision)

-- Velocidades del jugador
friccion :: Float
friccion = 0.90

umbralParada :: Float
umbralParada = 0.5

-- Función para calcular el vector de desplazamiento
calcularDirJugador :: Input -> Float -> Float -> V2 Float
calcularDirJugador input velocidad factor =
    let 
        -- Obtenemos la dirección pura (solo -1, 0 o 1)
        dirX = (if derecha input then 1 else 0) - (if izquierda input then 1 else 0)
        dirY = (if abajo input then 1 else 0)  - (if arriba input then 1 else 0)
        direccion = V2 dirX dirY
    in 
        -- Verificamos si hay movimiento para evitar dividir por cero al normalizar
        if direccion == V2 0 0 
            then V2 0 0 
            else normalize direccion ^* (velocidad*factor)

-- Mover al jugador 
moverJugador :: Input -> Jugador -> [Obstaculo] -> Jugador
moverJugador input jugadorIni mapObstaculos = 
    let 
        -- Calculamos la velocidad que tendrá el jugador
        velCor = velCorrerJ jugadorIni
        velCam = velCaminarJ jugadorIni
        factor = velFactorJ jugadorIni
        rapidez = (if shift input then velCor else velCam) * factor

        -- Resolvemos la velocidad del empuje si corresponde
        esEmpujado = norm (velGolpeJ jugadorIni) > umbralParada
        velocidad = if esEmpujado
                    then velGolpeJ jugadorIni
                    else if shift input
                         then calcularDirJugador input velCor factor 
                         else calcularDirJugador input velCam factor
        (V2 dx dy) = velocidad

        -- Verificamos si choca en X (Resolvemos posición en X)
        posIniJ = posJugador jugadorIni
        posIniJX = posIniJ + V2 dx 0
        jugadorMovX = jugadorIni { posJugador = posIniJX }
        jugadorMid = if checkColision jugadorMovX mapObstaculos 
                     then jugadorIni 
                     else jugadorMovX

        -- Verificamos si choca en Y [A partir de X Resuelto] (Resolvemos posición en Y)
        posMidJ = posJugador jugadorMid
        posMidJY = posMidJ + V2 0 dy
        jugadorMovY = jugadorMid { posJugador = posMidJY }
        jugadorFin = if checkColision jugadorMovY mapObstaculos
                     then jugadorMid
                     else jugadorMovY

        nuevoVelGolpe = if esEmpujado
                        then velGolpeJ jugadorIni ^* friccion
                        else V2 0 0
        
    in jugadorFin 
        { velGolpeJ = nuevoVelGolpe
        , velJugador = if esEmpujado 
                       then norm nuevoVelGolpe
                       else rapidez
        }