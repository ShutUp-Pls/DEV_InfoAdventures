module Personajes.Enemigo where

import Linear.V2 (V2(..))
import Types

-- Crea un enemigo inicial
crearEnemigo :: V2 Float -> Enemigo
crearEnemigo pos = Enemigo
    { posEnemigo = pos
    , velEnemigo = 1.5
    , vidEnemigo = 100.0
    , tamEnemigo = V2 30 30
    , rangoVision = 200.0
    , empujeE     = 5.0
    , velGolpeE   = V2 0 0
    }

enemigosIniciales :: [Enemigo]
enemigosIniciales = 
    [ crearEnemigo (V2 600 200)
    , crearEnemigo (V2 100 100)
    , crearEnemigo (V2 700 500)
    , crearEnemigo (V2 200 450)
    ]