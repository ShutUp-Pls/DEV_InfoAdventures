module Objetos.Camara where

-- Modulos del sistema
import qualified SDL

-- Modulos propios
import qualified Types

-- Función pura para actualizar el valor de la camara en el monadeState
actualizarCamara :: Types.Input -> Types.Jugador -> Types.Camara -> Types.Camara
actualizarCamara input target camActual = camActual { Types.posCamara = finalCamPos, Types.deadzoneSize = finalDZ }
  where
    currentDZ = Types.deadzoneSize camActual
    cPos      = Types.posCamara camActual
    pPos      = Types.posJugador target
    pSize     = Types.tamJugador target
    
    -- Cambio de tamaño de la Deadzone (Ajuste dinamico mediante 'O' y 'P')
    changeSpeed = 5.0
    dzCandidata 
        | Types.increaseDZ input = currentDZ + SDL.V2 changeSpeed changeSpeed
        | Types.decreaseDZ input = currentDZ - SDL.V2 changeSpeed changeSpeed
        | otherwise              = currentDZ

    -- Limites para que la Deadzone no sea más pequeña que el jugador ni más grande que un valor arbitrario
    (SDL.V2 pAW pAH) = pSize
    (SDL.V2 w h)     = dzCandidata
    finalW           = max pAW (min 500 w)
    finalH           = max pAH (min 500 h)
    finalDZ          = SDL.V2 finalW finalH
    
    -- Calculamos la distancia entre el Jugador y el Centro de la Cámara
    (SDL.V2 diffX diffY) = pPos - cPos
    (SDL.V2 dzW dzH)     = finalDZ
    limitX = dzW / 2
    limitY = dzH / 2
    
    -- Calculamos nueva X
    -- Si el jugador se sale por la derecha (diffX + anchoJugador > limite)
    -- O si se sale por la izquierda (diffX < -limite)
    (SDL.V2 pW pH) = pSize
    newCamX 
        | (diffX + pW) > limitX = cPos + SDL.V2 ((diffX + pW) - limitX) 0
        | diffX < -limitX       = cPos + SDL.V2 (diffX + limitX) 0
        | otherwise             = cPos

    -- Calculamos nueva Y basándonos en la nueva X ya calculada
    finalCamPos 
        | (diffY + pH) > limitY = newCamX + SDL.V2 0 ((diffY + pH) - limitY)
        | diffY < -limitY       = newCamX + SDL.V2 0 (diffY + limitY)
        | otherwise             = newCamX