module Personajes.Enemigo where

-- Modulos del sitema
import qualified SDL

-- Modulos propios
import qualified Types

-- Crea un enemigo inicial
crearEnemigo :: SDL.V2 Float -> Types.Enemigo
crearEnemigo pos = Types.Enemigo
    { Types.posEnemigo  = pos
    , Types.velEnemigo  = 1.5
    , Types.vidEnemigo  = 100.0
    , Types.tamEnemigo  = SDL.V2 30 30
    , Types.rangoVision = 200.0
    , Types.empujeE     = 5.0
    , Types.velGolpeE   = SDL.V2 0 0
    }