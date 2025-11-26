module Personajes.Enemigo where

import Linear.V2 (V2(..))
import Linear.Metric (normalize, distance)
import Linear.Vector ((^*))
import Types
import Fisica.Colisiones (checkColisionEnemigo)

-- Constructor
nuevoEnemigo :: Enemigo
nuevoEnemigo = Enemigo
    { posEnemigo = V2 600 200  -- Empieza en otro lado
    , velEnemigo = 1.5         -- Un poco más lento que el jugador
    , tamEnemigo = V2 30 30
    , rangoVision = 200.0
    }

-- Lógica de IA: Decide hacia dónde moverse
-- Recibe al Jugador para saber dónde está
calcularDireccion :: Enemigo -> Jugador -> V2 Float
calcularDireccion enemy player =
    let dist = distance (posEnemigo enemy) (posJugador player)
    in if dist < rangoVision enemy
       then 
           -- Si está cerca, perseguir
           let diff = posJugador player - posEnemigo enemy
           in normalize diff ^* velEnemigo enemy
       else 
           -- Si está lejos, quedarse quieto (o patrullar)
           V2 0 0

-- Lógica de Física: Aplica el movimiento respetando colisiones
moverEnemigo :: Enemigo -> V2 Float -> Mapa -> Enemigo
moverEnemigo enemy delta mapObstaculos = 
    let 
        (V2 dx dy) = delta
        currentPos = posEnemigo enemy
        
        -- Mover en X
        posX = currentPos + V2 dx 0
        enemyX = enemy { posEnemigo = posX }
        enemyResueltoX = if checkColisionEnemigo enemyX mapObstaculos 
                         then enemy 
                         else enemyX
        
        -- Mover en Y
        posResueltaX = posEnemigo enemyResueltoX
        posY = posResueltaX + V2 0 dy
        enemyY = enemyResueltoX { posEnemigo = posY }
        
    in if checkColisionEnemigo enemyY mapObstaculos
       then enemyResueltoX
       else enemyY