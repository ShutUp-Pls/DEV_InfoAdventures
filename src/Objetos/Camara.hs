module Objetos.Camara where

-- Modulos del sistema
import qualified SDL

-- Modulos propios
import qualified Types
import qualified Graficos.Dibujado as GD

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

dibujarDeadzone :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> IO ()
dibujarDeadzone renderer texture (SDL.V2 w h) = do
    let x = (fromIntegral GD.screenWidth - w) / 2
    let y = (fromIntegral GD.screenHeight - h) / 2
    
    -- Definimos las 4 esquinas en coordenadas de pantalla
    let tl = SDL.V2 x y
    let tr = SDL.V2 (x + w) y
    let br = SDL.V2 (x + w) (y + h)
    let bl = SDL.V2 x (y + h)
    
    let color = SDL.V3 255 0 0 -- Rojo
    let grosor = 2
    let uiCam = GD.screenCenter 

    GD.dibujarLinea renderer texture uiCam tl tr grosor color
    GD.dibujarLinea renderer texture uiCam tr br grosor color
    GD.dibujarLinea renderer texture uiCam br bl grosor color
    GD.dibujarLinea renderer texture uiCam bl tl grosor color