module Fisica.MovJugador where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Fisica.Colisiones as FC

friccion :: Float
friccion = 0.90

umbralParada :: Float
umbralParada = 0.5

-- Las fisicas son, basicamente, pasar de un estado a otro
-- Una tarea ideal para una (monadeState) que controle esto
actFisicasMovJugador :: Types.Input -> [Types.Obstaculo] -> CMS.State Types.Jugador ()
actFisicasMovJugador input mapObstaculos = do
    jugador <- CMS.get 
    let velCor = Types.velCorrerJ jugador
        velCam = Types.velCaminarJ jugador
        factor = Types.velFactorJ jugador
        rapidez = (if Types.shift input then velCor else velCam) * factor
    
        -- Calculamos la velocidad del movimiento según las condiciones de empuje
        esEmpujado = LM.norm (Types.velGolpeJ jugador) > umbralParada
        velocidad = if esEmpujado
                    then Types.velGolpeJ jugador
                    else if Types.shift input
                         then Utils.vectorInput input velCor factor 
                         else Utils.vectorInput input velCam factor
        (SDL.V2 dx dy) = velocidad

        -- Actualizamos velocidad final
        nuevoVelGolpe = if esEmpujado
                        then Types.velGolpeJ jugador LV.^* friccion
                        else SDL.V2 0 0

    -- Resolvemos X (Intentamos mover y revertimos si hay choque)
    posOriginal <- CMS.gets Types.posJugador
    CMS.modify $ \s -> s { Types.posJugador = posOriginal + SDL.V2 dx 0 }
    chocaX <- CMS.gets (`FC.checkColision` mapObstaculos)
    CMS.when chocaX $ do CMS.modify $ \s -> s { Types.posJugador = posOriginal }

    -- Resolvemos Y (A partir de donde quedamos en X)
    posPostX <- CMS.gets Types.posJugador
    CMS.modify $ \s -> s { Types.posJugador = posPostX + SDL.V2 0 dy }
    chocaY <- CMS.gets (`FC.checkColision` mapObstaculos)
    CMS.when chocaY $ do CMS.modify $ \s -> s { Types.posJugador = posPostX }

    -- Aplicamos las modificaciones
    CMS.modify $ \s -> s { 
        Types.velGolpeJ = nuevoVelGolpe,
        Types.velJugador = if esEmpujado 
                           then LM.norm nuevoVelGolpe 
                           else rapidez
    }