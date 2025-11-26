module Fisica.MovJugador where

import Linear.V2 (V2(..))
import Linear.Metric (normalize)
import Linear.Vector ((^*))

import Types
import Fisica.Colisiones(checkColision)

-- Velocidades del jugador
velCaminar :: Float
velCaminar = 3.0

velCorrer :: Float
velCorrer = 6.0

-- Función para calcular el vector de desplazamiento
calcularDirJugador :: Input -> Float -> V2 Float
calcularDirJugador input velocidad =
    let 
        -- Obtenemos la dirección pura (solo -1, 0 o 1)
        dirX = (if derecha input then 1 else 0) - (if izquierda input then 1 else 0)
        dirY = (if abajo input then 1 else 0)  - (if arriba input then 1 else 0)
        direccion = V2 dirX dirY
    in 
        -- Verificamos si hay movimiento para evitar dividir por cero al normalizar
        if direccion == V2 0 0 
            then V2 0 0 
            else normalize direccion ^* velocidad

-- Mover al jugador 
moverJugador :: Input -> Jugador -> Mapa -> Jugador
moverJugador input jugadorIni mapObstaculos = 
    let 
        -- Determinamos la velocidad según este corriendo o no
        velocidad = if shift input 
                    then velCorrer
                    else velCaminar
        (V2 dx dy) = calcularDirJugador input velocidad

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
        
    in jugadorFin