module Fisica.MovEnemigo where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Fisica.Colisiones as FC

friccion :: Float
friccion = 0.90

umbralParada :: Float
umbralParada = 0.5

calcularDirEnemigo :: Types.Enemigo -> Types.Jugador -> SDL.V2 Float
calcularDirEnemigo enemigoIni player =
    let dist = LM.distance (Types.posEnemigo enemigoIni) (Types.posJugador player)
    in if dist < Types.rangoVision enemigoIni
       then 
           -- Si está cerca, perseguir
           let diff = Types.posJugador player - Types.posEnemigo enemigoIni
           in LM.normalize diff LV.^* Types.velEnemigo enemigoIni
       else 
           -- Si está lejos, quedarse quieto (o patrullar)
           SDL.V2 0 0

-- Mover al enemigo
moverEnemigo :: Types.Enemigo -> SDL.V2 Float -> [Types.Obstaculo] -> Types.Enemigo
moverEnemigo enemigoIni delta mapObstaculos = 
    let 
        esEmpujado = LM.norm (Types.velGolpeE enemigoIni) > umbralParada
        
        (SDL.V2 dx dy) = if esEmpujado 
                     then Types.velGolpeE enemigoIni
                     else delta
        currentPos = Types.posEnemigo enemigoIni
        
        -- Mover en X
        posX = currentPos + SDL.V2 dx 0
        enemyX = enemigoIni { Types.posEnemigo = posX }
        enemyResueltoX = if FC.checkColision enemyX mapObstaculos 
                         then enemigoIni 
                         else enemyX
        
        -- Mover en Y
        posResueltaX = Types.posEnemigo enemyResueltoX
        posY = posResueltaX + SDL.V2 0 dy
        enemyY = enemyResueltoX { Types.posEnemigo = posY }

        enemyFin = if FC.checkColision enemyY mapObstaculos
                      then enemyResueltoX
                      else enemyY

        nuevoVelGolpe = if esEmpujado
                        then Types.velGolpeE enemigoIni LV.^* friccion
                        else SDL.V2 0 0
        
    in enemyFin { Types.velGolpeE = nuevoVelGolpe }