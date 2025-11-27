module Fisica.Colisiones where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Fisica.SAT as FS

class Hitbox a where
    getPos :: a -> SDL.V2 Float
    getTam :: a -> SDL.V2 Float
    getAng :: a -> Float

instance Hitbox Types.Jugador where
    getPos = Types.posJugador
    getTam = Types.tamJugador
    getAng _ = 0

instance Hitbox Types.Enemigo where
    getPos = Types.posEnemigo
    getTam = Types.tamEnemigo
    getAng _ = 0

instance Hitbox Types.Item where
    getPos = Types.posItem
    getTam = Types.tamItem
    getAng _ = 0

instance Hitbox Types.Obstaculo where
    getPos (Types.Obstaculo p _ _) = p
    getTam (Types.Obstaculo _ s _) = s
    getAng (Types.Obstaculo _ _ a) = a

-- Verifica si algo con Hitbox choca contra algún obstáculo de la lista
checkColision :: Hitbox a => a -> [Types.Obstaculo] -> Bool
checkColision entidad obstaculos =
    any chocaCon obstaculos
  where
    posE = getPos entidad
    tamE = getTam entidad
    angE = getAng entidad
    
    chocaCon (Types.Obstaculo posObj tamObj angObj) =
        FS.satCollision posE tamE angE posObj tamObj angObj

-- Verifica si algo con Hitbox choca contra algún item de la lista
checkColisionsItems :: Hitbox a => a -> [Types.Item] -> [Types.Item]
checkColisionsItems entidad listaItems =
    filter estaTocando listaItems
  where
    posE = getPos entidad
    tamE = getTam entidad
    angE = getAng entidad
    
    estaTocando :: Types.Item -> Bool
    estaTocando it = 
        -- Solo detectamos colisión si el item está "activo"
        FS.satCollision posE tamE angE (Types.posItem it) (Types.tamItem it) 0

-- Esta función acumula los enemigos procesados y actualiza al jugador acumulado
logicaColision :: Types.Enemigo -> (Types.Jugador, [Types.Enemigo]) -> (Types.Jugador, [Types.Enemigo])
logicaColision enem (jActual, enemigosProcesados) =
    if FS.satCollision (Types.posJugador jActual) (Types.tamJugador jActual) 0 (Types.posEnemigo enem) (Types.tamEnemigo enem) 0
    then
        let
            diff = Types.posJugador jActual - Types.posEnemigo enem
            dir  = if diff == SDL.V2 0 0 then SDL.V2 1 0 else LM.normalize diff
            nuevoVelJ = dir LV.^* Types.empujeE enem
            nuevoVelE = (dir LV.^* (-1)) LV.^* Types.empujeJ jActual
            jDañado = jActual { Types.vidJugador = Types.vidJugador jActual - 5.0
                              , Types.velGolpeJ  = nuevoVelJ
                              }
            enemGolpeado = enem { Types.velGolpeE = nuevoVelE }
        in
            (jDañado, enemGolpeado : enemigosProcesados)
    else
        (jActual, enem : enemigosProcesados)

resolverCombate :: Types.Jugador -> [Types.Enemigo] -> (Types.Jugador, [Types.Enemigo])
resolverCombate jug enemigosList = 
    foldr logicaColision (jug, []) enemigosList