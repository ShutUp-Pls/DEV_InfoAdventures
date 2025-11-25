module Personajes.Jugador where

import Types
import Linear.V2 (V2(..))
import Linear.Metric (normalize)
import Linear.Vector ((^*))

-- Velocidades constantes
velocidadCaminar :: Float
velocidadCaminar = 3.0

velocidadCorrer :: Float
velocidadCorrer = 6.0

-- Crea un jugador inicial
nuevoJugador :: Jugador
nuevoJugador = Jugador
    { posJugador = V2 400 300 
    , velocidad  = velocidadCaminar
    }

-- Función para calcular el vector de desplazamiento
calcularDelta :: Input -> Float -> V2 Float
calcularDelta input vel =
    let 
        -- Obtenemos la dirección pura (solo -1, 0 o 1)
        dirX = (if right input then 1 else 0) - (if left input then 1 else 0)
        dirY = (if down input then 1 else 0)  - (if up input then 1 else 0)
        direction = V2 dirX dirY
    in 
        -- Verificamos si hay movimiento para evitar dividir por cero al normalizar
        if direction == V2 0 0 
            then V2 0 0 
            else normalize direction ^* vel

-- Movimiento caminando
moverJugadorCaminar :: Input -> Jugador -> Jugador
moverJugadorCaminar input p = p { posJugador = posJugador p + delta }
  where
    delta = calcularDelta input velocidadCaminar

-- Movimiento corriendo
moverJugadorCorrer :: Input -> Jugador -> Jugador
moverJugadorCorrer input p = p { posJugador = posJugador p + delta }
  where
    delta = calcularDelta input velocidadCorrer