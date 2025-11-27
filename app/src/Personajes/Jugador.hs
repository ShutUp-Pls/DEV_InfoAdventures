module Personajes.Jugador where

-- Módulos de Sistema
import qualified Control.Monad.State as CMS

-- Módulos propios
import qualified Types
import qualified Fisica.MovJugador as FMJ

moverJugador :: Types.Input -> Types.Jugador -> [Types.Obstaculo] -> Types.Jugador
moverJugador input jugadorIni mapObstaculos = 
    CMS.execState (FMJ.actFisicasMovJugador input mapObstaculos) jugadorIni