module Camara where

import Linear.V2 (V2(..))
import Types

-- Función pura para actualizar el valor de la camara en el monadeState
actualizarCamara :: Input -> Jugador -> Camara -> Camara
actualizarCamara input target camActual = camActual { posCamara = finalCamPos, deadzoneSize = finalDZ }
  where
    currentDZ = deadzoneSize camActual
    cPos      = posCamara camActual
    pPos      = posJugador target
    pSize     = tamJugador target
    
    -- Cambio de tamaño de la Deadzone (Ajuste dinamico mediante 'O' y 'P')
    changeSpeed = 5.0
    dzCandidata 
        | increaseDZ input = currentDZ + V2 changeSpeed changeSpeed
        | decreaseDZ input = currentDZ - V2 changeSpeed changeSpeed
        | otherwise        = currentDZ

    -- Limites para que la Deadzone no sea más pequeña que el jugador ni más grande que un valor arbitrario
    (V2 pAW pAH) = pSize
    (V2 w h)     = dzCandidata
    finalW       = max pAW (min 500 w)
    finalH       = max pAH (min 500 h)
    finalDZ      = V2 finalW finalH
    
    -- Calculamos la distancia entre el Jugador y el Centro de la Cámara
    (V2 diffX diffY) = pPos - cPos
    (V2 dzW dzH)     = finalDZ
    limitX = dzW / 2
    limitY = dzH / 2
    
    -- Calculamos nueva X
    -- Si el jugador se sale por la derecha (diffX + anchoJugador > limite)
    -- O si se sale por la izquierda (diffX < -limite)
    (V2 pW pH) = pSize
    newCamX 
        | (diffX + pW) > limitX = cPos + V2 ((diffX + pW) - limitX) 0
        | diffX < -limitX       = cPos + V2 (diffX + limitX) 0
        | otherwise             = cPos

    -- Calculamos nueva Y basándonos en la nueva X ya calculada
    finalCamPos 
        | (diffY + pH) > limitY = newCamX + V2 0 ((diffY + pH) - limitY)
        | diffY < -limitY       = newCamX + V2 0 (diffY + limitY)
        | otherwise             = newCamX