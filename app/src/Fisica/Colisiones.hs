module Fisica.Colisiones where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types

class Hitbox a where
    getPos :: a -> SDL.V2 Float
    getTam :: a -> SDL.V2 Float

instance Hitbox Types.Jugador where
    getPos = Types.posJugador
    getTam = Types.tamJugador

instance Hitbox Types.Enemigo where
    getPos = Types.posEnemigo
    getTam = Types.tamEnemigo

instance Hitbox Types.Item where
    getPos = Types.posItem
    getTam = Types.tamItem

-- Verifica si dos rectángulos (pos1, size1) y (pos2, size2) se superponen
haySolapamiento :: SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float -> Bool
haySolapamiento (SDL.V2 x1 y1) (SDL.V2 w1 h1) (SDL.V2 x2 y2) (SDL.V2 w2 h2) =
    x1 < x2 + w2 &&
    x1 + w1 > x2 &&
    y1 < y2 + h2 &&
    y1 + h1 > y2

-- Verifica si algo con Hitbox choca contra algún obstáculo de la lista
checkColision :: Hitbox a => a -> [Types.Obstaculo] -> Bool
checkColision entidad obstaculos =
    any chocaCon obstaculos
  where
    posE = getPos entidad
    tamE = getTam entidad
    chocaCon (Types.Obstaculo posObj tamObj) =
        haySolapamiento posE tamE posObj tamObj

-- Verifica si algo con Hitbox choca contra algún item de la lista
checkColisionsItems :: Hitbox a => a -> [Types.Item] -> [Types.Item]
checkColisionsItems entidad listaItems =
    filter estaTocando listaItems
  where
    posE = getPos entidad
    tamE = getTam entidad
    
    estaTocando :: Types.Item -> Bool
    estaTocando it = 
        -- Solo detectamos colisión si el item está "activo"
        haySolapamiento posE tamE (Types.posItem it) (Types.tamItem it)

-- Esta función acumula los enemigos procesados y actualiza al jugador acumulado
logicaColision :: Types.Enemigo -> (Types.Jugador, [Types.Enemigo]) -> (Types.Jugador, [Types.Enemigo])
logicaColision enem (jActual, enemigosProcesados) =
    if haySolapamiento (Types.posJugador jActual) (Types.tamJugador jActual) (Types.posEnemigo enem) (Types.tamEnemigo enem)
    then
        let
            -- Calcular dirección del impacto (desde el enemigo hacia el jugador)
            diff = Types.posJugador jActual - Types.posEnemigo enem
            -- Evitamos division por cero si spawnearon en el mismo pixel exacto
            dir  = if diff == SDL.V2 0 0 then SDL.V2 1 0 else LM.normalize diff
            -- Aplicar "Empuje" instantáneo
            nuevoVelJ = dir LV.^* Types.empujeE enem
            -- El enemigo sale disparado en dirección contraria con fuerza del jugador
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