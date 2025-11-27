module Personajes.Enemigo where

-- Modulos del sitema
import qualified SDL
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Fisica.Movimiento as FM
import qualified Linear.Metric as LM

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

actFisicasMovEnemigo :: SDL.V2 Float -> [Types.Obstaculo] -> CMS.State Types.Enemigo ()
actFisicasMovEnemigo delta mapObstaculos = do
    -- El enemigo no tiene lógica extra post-movimiento, así que solo ejecutamos y descartamos el resultado
    _ <- FM.resolverFisica delta mapObstaculos
    return ()

moverEnemigo :: Types.Enemigo -> SDL.V2 Float -> [Types.Obstaculo] -> Types.Enemigo
moverEnemigo enemigoIni delta mapObstaculos = 
    CMS.execState (actFisicasMovEnemigo delta mapObstaculos) enemigoIni

calcularDirEnemigo :: Types.Enemigo -> Types.Jugador -> SDL.V2 Float
calcularDirEnemigo enemigo player =
    let posE = Types.posEnemigo enemigo
        posJ = Types.posJugador player
        dist = LM.distance posE posJ
        rango = Types.rangoVision enemigo
    in if dist < rango
       then Utils.vectorHacia posE posJ (Types.velEnemigo enemigo)
       else SDL.V2 0 0