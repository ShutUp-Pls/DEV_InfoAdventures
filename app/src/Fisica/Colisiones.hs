module Fisica.Colisiones where

import Linear.V2 (V2(..))
import Types (Jugador(..), Enemigo(..), Obstaculo(..))

class Hitbox a where
    getPos :: a -> V2 Float
    getTam :: a -> V2 Float

instance Hitbox Jugador where
    getPos = posJugador
    getTam = tamJugador

instance Hitbox Enemigo where
    getPos = posEnemigo
    getTam = tamEnemigo

-- Verifica si dos rectángulos (pos1, size1) y (pos2, size2) se superponen
haySolapamiento :: V2 Float -> V2 Float -> V2 Float -> V2 Float -> Bool
haySolapamiento (V2 x1 y1) (V2 w1 h1) (V2 x2 y2) (V2 w2 h2) =
    x1 < x2 + w2 &&
    x1 + w1 > x2 &&
    y1 < y2 + h2 &&
    y1 + h1 > y2

-- Verifica si el jugador choca contra algún obstáculo de la lista
checkColision :: Hitbox a => a -> [Obstaculo] -> Bool
checkColision entidad obstaculos =
    any chocaCon obstaculos
  where
    posE = getPos entidad
    tamE = getTam entidad
    chocaCon (Obstaculo posObj tamObj) =
        haySolapamiento posE tamE posObj tamObj
