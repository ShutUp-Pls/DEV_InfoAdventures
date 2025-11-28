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
    { Types.posEnemigo      = pos
    , Types.tamEnemigo      = SDL.V2 30 30
    , Types.velGolpeE       = SDL.V2 0 0
    , Types.empujeE         = 5.0
    , Types.velEnemigo      = 1.5
    , Types.angEnemigo      = 0.0
    , Types.vidEnemigo      = 100.0
    , Types.rangoVision     = 200.0
    , Types.radInterno      = 1.0
    , Types.rechazoE        = 0.2
    }

actFisicasMovEnemigo :: SDL.V2 Float -> [Types.Obstaculo] -> CMS.State Types.Enemigo ()
actFisicasMovEnemigo delta mapObstaculos = do
    -- El enemigo no tiene lógica extra post-movimiento, así que solo ejecutamos y descartamos el resultado
    _ <- FM.resolverFisica delta mapObstaculos
    return ()

calcularDirEnemigo :: Types.Enemigo -> Types.Jugador -> SDL.V2 Float
calcularDirEnemigo enemigo player =
    let posE = Types.posEnemigo enemigo
        posJ = Types.posJugador player
        dist = LM.distance posE posJ
        rango = Types.rangoVision enemigo
    in if dist < rango
       then Utils.vectorHacia posE posJ (Types.velEnemigo enemigo)
       else SDL.V2 0 0

calcularAnguloHacia :: SDL.V2 Float -> SDL.V2 Float -> Float
calcularAnguloHacia origen destino =
    let direccion = destino - origen
        (SDL.V2 dx dy) = direccion
    in atan2 dy dx * (180 / pi) -- Retorna grados

moverEnemigo :: Types.Enemigo -> SDL.V2 Float -> [Types.Obstaculo] -> Types.Jugador -> Types.Enemigo
moverEnemigo enemigoIni delta mapObstaculos player = 
    let 
        -- 1. Resolvemos física (posición)
        enemigoMovido = CMS.execState (actFisicasMovEnemigo delta mapObstaculos) enemigoIni
        
        -- 2. Resolvemos rotación (Mirar al jugador)
        nuevoAngulo = calcularAnguloHacia (Types.posEnemigo enemigoMovido) (Types.posJugador player)
    in
        enemigoMovido { Types.angEnemigo = nuevoAngulo }