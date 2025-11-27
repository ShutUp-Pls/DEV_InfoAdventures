module Personajes.Enemigo where

-- Modulos del sitema
import qualified SDL
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Fisica.MovEnemigo as FME

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

moverEnemigo :: Types.Enemigo -> SDL.V2 Float -> [Types.Obstaculo] -> Types.Enemigo
moverEnemigo enemigoIni delta mapObstaculos = 
    CMS.execState (FME.actFisicasMovEnemigo delta mapObstaculos) enemigoIni