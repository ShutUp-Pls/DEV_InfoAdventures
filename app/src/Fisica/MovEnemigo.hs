module Fisica.MovEnemigo where

import Linear.V2 (V2(..))
import Linear.Metric (normalize, distance)
import Linear.Vector ((^*))

import Types
import Fisica.Colisiones (checkColision)

calcularDirEnemigo :: Enemigo -> Jugador -> V2 Float
calcularDirEnemigo enemigoIni player =
    let dist = distance (posEnemigo enemigoIni) (posJugador player)
    in if dist < rangoVision enemigoIni
       then 
           -- Si está cerca, perseguir
           let diff = posJugador player - posEnemigo enemigoIni
           in normalize diff ^* velEnemigo enemigoIni
       else 
           -- Si está lejos, quedarse quieto (o patrullar)
           V2 0 0

-- Mover al enemigo
moverEnemigo :: Enemigo -> V2 Float -> Mapa -> Enemigo
moverEnemigo enemigoIni delta mapObstaculos = 
    let 
        (V2 dx dy) = delta
        currentPos = posEnemigo enemigoIni
        
        -- Mover en X
        posX = currentPos + V2 dx 0
        enemyX = enemigoIni { posEnemigo = posX }
        enemyResueltoX = if checkColision enemyX mapObstaculos 
                         then enemigoIni 
                         else enemyX
        
        -- Mover en Y
        posResueltaX = posEnemigo enemyResueltoX
        posY = posResueltaX + V2 0 dy
        enemyY = enemyResueltoX { posEnemigo = posY }
        
    in if checkColision enemyY mapObstaculos
       then enemyResueltoX
       else enemyY