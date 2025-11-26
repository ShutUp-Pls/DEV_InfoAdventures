module Personajes.Enemigo where

import Linear.V2 (V2(..))
import Types

-- Crea un enemigo inicial
nuevoEnemigo :: Enemigo
nuevoEnemigo = Enemigo
    { posEnemigo = V2 600 200
    , velEnemigo = 1.5
    , tamEnemigo = V2 30 30
    , rangoVision = 200.0
    }