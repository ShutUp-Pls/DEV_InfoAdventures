module Fisica.MovEnemigo where

import Linear.V2 (V2(..))
import Linear.Metric (normalize, distance, norm)
import Linear.Vector ((^*))

import Types
import Fisica.Colisiones (checkColision)

friccion :: Float
friccion = 0.90

umbralParada :: Float
umbralParada = 0.5

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
moverEnemigo :: Enemigo -> V2 Float -> [Obstaculo] -> Enemigo
moverEnemigo enemigoIni delta mapObstaculos = 
    let 
        esEmpujado = norm (velGolpeE enemigoIni) > umbralParada
        
        (V2 dx dy) = if esEmpujado 
                     then velGolpeE enemigoIni
                     else delta
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

        enemyFin = if checkColision enemyY mapObstaculos
                      then enemyResueltoX
                      else enemyY

        nuevoVelGolpe = if esEmpujado
                        then velGolpeE enemigoIni ^* friccion
                        else V2 0 0
        
    in enemyFin { velGolpeE = nuevoVelGolpe }