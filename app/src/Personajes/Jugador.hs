module Personajes.Jugador where

import Types
import Linear.V2 (V2(..))
import Fisica.MovJugador (velCaminar)

-- Crea un jugador inicial
nuevoJugador :: Jugador
nuevoJugador = Jugador
    { posJugador = V2 400 300
    , velJugador  = velCaminar
    , tamJugador = V2 30.0 30.0
    }