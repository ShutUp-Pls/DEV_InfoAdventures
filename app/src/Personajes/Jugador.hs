module Personajes.Jugador where

-- Modulos del sistema
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
    -- Actualizamos cosas específicas del jugador
    CMS.modify $ \s -> s { Types.velJugador = LM.norm velFinal }

moverJugador :: Types.Input -> Types.Jugador -> [Types.Obstaculo] -> Types.Jugador
moverJugador input jugadorIni mapObstaculos = 
    CMS.execState (actFisicasMovJugador input mapObstaculos) jugadorIni