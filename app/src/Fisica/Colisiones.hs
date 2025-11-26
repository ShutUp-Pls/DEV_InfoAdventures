module Fisica.Colisiones where

import Linear.V2 (V2(..))
import Types (Jugador(..), Enemigo(..), Obstaculo(..))

-- Verifica si dos rectángulos (pos1, size1) y (pos2, size2) se superponen
haySolapamiento :: V2 Float -> V2 Float -> V2 Float -> V2 Float -> Bool
haySolapamiento (V2 x1 y1) (V2 w1 h1) (V2 x2 y2) (V2 w2 h2) =
    x1 < x2 + w2 &&
    x1 + w1 > x2 &&
    y1 < y2 + h2 &&
    y1 + h1 > y2

-- Verifica si el jugador choca contra algún obstáculo de la lista
checkColision :: Jugador -> [Obstaculo] -> Bool
checkColision jugador listaObstaculos = 
    any chocaCon listaObstaculos
  where
    playerSize = tamJugador jugador 
    playerPos  = posJugador jugador
    
    chocaCon :: Obstaculo -> Bool
    chocaCon (Obstaculo oPos oSize) = 
        haySolapamiento playerPos playerSize oPos oSize

  -- Verifica si el enemigo choca contra algún obstáculo de la lista
checkColisionEnemigo :: Enemigo -> [Obstaculo] -> Bool
checkColisionEnemigo enemigo listaObstaculos = 
    any chocaCon listaObstaculos
  where
    eTam = tamEnemigo enemigo 
    ePos  = posEnemigo enemigo
    
    chocaCon :: Obstaculo -> Bool
    chocaCon (Obstaculo oPos oSize) = 
        haySolapamiento ePos eTam oPos oSize