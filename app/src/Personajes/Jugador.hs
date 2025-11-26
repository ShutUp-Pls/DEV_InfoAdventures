module Personajes.Jugador where

import Types
import Linear.V2 (V2(..))

-- Crea un jugador inicial
nuevoJugador :: Jugador
nuevoJugador = Jugador
    { posJugador = V2 400 300
    , velJugador = 3.0
    , velCaminarJ = 3.0
    , velCorrerJ  = 6.0
    , velFactorJ = 1.0
    , vidJugador = 100.0
    , tamJugador = V2 30.0 30.0
    , empujeJ    = 10.0
    , velGolpeJ  = V2 0 0
    }