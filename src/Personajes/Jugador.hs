module Personajes.Jugador where

-- Modulos del sistema
import SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Control.Monad.State as CMS
import Data.Fixed (mod')

-- Modulos propios
import qualified Types
import qualified Fisica.Movimiento as FM

-- Función auxiliar para interpolar ángulos suavemente
suavizarAngulo :: Float -> Float -> Float -> Float
suavizarAngulo actual objetivo velocidad =
    let 
        diff = objetivo - actual
        delta = (diff + 180) `mod'` 360 - 180
    in 
        if abs delta < velocidad
        then objetivo
        else actual + (signum delta * velocidad)

actFisicasMovJugador :: Types.Input -> [Types.Obstaculo] -> CMS.State Types.Jugador ()
actFisicasMovJugador input mapObstaculos = do
    jugador <- CMS.get

    let dirX = (if Types.derecha input then 1 else 0) - (if Types.izquierda input then 1 else 0)
    let dirY = (if Types.abajo input then 1 else 0)  - (if Types.arriba input then 1 else 0)
    let vecDireccion = SDL.V2 dirX dirY

    let velCor = Types.velCorrerJ jugador
        velCam = Types.velCaminarJ jugador
        velRot = Types.velRotacion jugador
        factor = Types.velFactorJ jugador
        velocidadBase = if Types.shift input then velCor else velCam

    let velocidadInput = if vecDireccion == SDL.V2 0 0 
                         then SDL.V2 0 0 
                         else LM.normalize vecDireccion LV.^* (velocidadBase * factor)

    velFinal <- FM.resolverFisica velocidadInput mapObstaculos
    let anguloActual = Types.angJugador jugador
    let nuevoAngulo = if vecDireccion == SDL.V2 0 0
                      then anguloActual
                      else 
                          let 
                              rads = atan2 dirY dirX
                              targetAng = rads * (180 / pi)
                          in 
                              suavizarAngulo anguloActual targetAng velRot

    CMS.modify $ \s -> s { Types.velJugador = LM.norm velFinal
                         , Types.angJugador = nuevoAngulo 
                         }

moverJugador :: Types.Input -> Types.Jugador -> [Types.Obstaculo] -> Types.Jugador
moverJugador input jugadorIni mapObstaculos = 
    CMS.execState (actFisicasMovJugador input mapObstaculos) jugadorIni