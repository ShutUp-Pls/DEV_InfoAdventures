module Personajes.Jugador where

-- Modulos del sistema
import SDL
import qualified Linear.Metric as LM
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Fisica.Movimiento as FM

actFisicasMovJugador :: Types.Input -> [Types.Obstaculo] -> CMS.State Types.Jugador ()
actFisicasMovJugador input mapObstaculos = do
    jugador <- CMS.get
    -- Calculamos input específico del jugador
    let velCor = Types.velCorrerJ jugador
        velCam = Types.velCaminarJ jugador
        factor = Types.velFactorJ jugador
        velocidadInput = if Types.shift input
                         then Utils.vectorInput input velCor factor 
                         else Utils.vectorInput input velCam factor
    -- Delegamos la física dura a la monadeState de movimiento [Esto está interesante explicarlo]
    velFinal <- FM.resolverFisica velocidadInput mapObstaculos

    -- Si el jugador se mueve, actualizamos su ángulo. Si no, mantiene el último.
    let nuevoAngulo = if velFinal == SDL.V2 0 0 
                      then Types.angJugador jugador 
                      else (atan2 (gettingY velFinal) (gettingX velFinal)) * (180 / pi)

    CMS.modify $ \s -> s { Types.velJugador = LM.norm velFinal
                         , Types.angJugador = nuevoAngulo 
                         }
  where
    gettingX (SDL.V2 x _) = x
    gettingY (SDL.V2 _ y) = y

moverJugador :: Types.Input -> Types.Jugador -> [Types.Obstaculo] -> Types.Jugador
moverJugador input jugadorIni mapObstaculos = 
    CMS.execState (actFisicasMovJugador input mapObstaculos) jugadorIni