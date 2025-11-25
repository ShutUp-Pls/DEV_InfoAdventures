module Personajes.Jugador where

import Types
import Linear.V2 (V2(..))

-- Crea un jugador inicial en el centro (o donde quieras)
nuevoJugador :: Jugador
nuevoJugador = Jugador
    { posJugador = V2 400 300 -- Asumiendo una pantalla de 800x600 definida en el Main.hs
    , velocidad  = 5
    }

-- Recibe un input y un jugador, devuelve un nuevo jugador
moverJugador :: Input -> Jugador -> Jugador
moverJugador input p = p { posJugador = posJugador p + delta }
  where
    vel = velocidad p
    dx = (if right input then vel else 0) - (if left input then vel else 0)
    dy = (if down input then vel else 0)  - (if up input then vel else 0)
    delta = V2 dx dy