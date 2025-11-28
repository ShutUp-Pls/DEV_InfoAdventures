module Objetos.Camara where

-- Módulos del sistema
import qualified SDL
import qualified Data.Word as DW

-- Módulos propios
import qualified Types
import qualified Graficos.Dibujado as GD

-- Cuánto cambia el zoom por frame al pulsar +/-
velZoomManual :: Float
velZoomManual = 0.02
-- El zoom más alejado permitido
zoomMinimo :: Float
zoomMinimo = 0.5
-- El zoom más cercano permitido manualmente
zoomMaxUsuario :: Float
zoomMaxUsuario = 3.0       
-- Cuanto zoom extra se añade cuando te ven
zoomBoostPeligro :: Float
zoomBoostPeligro = 0.2     
-- Tope máximo total (Manual + Peligro)
zoomMaxAbsoluto :: Float
zoomMaxAbsoluto = 3.5      
-- Velocidad de interpolación (5% de la distancia por frame)
factorSuavizado :: Float
factorSuavizado = 0.05     
-- Si la diferencia es menor a esto, el zoom se fija
umbralEstabilidad :: Float
umbralEstabilidad = 0.001  

-- Velocidad al cambiar el tamaño de la deadzone con O/P
velAjusteDeadzone :: Float
velAjusteDeadzone = 5.0    
-- Tamaño máximo en píxeles de la deadzone
maxTamanoDeadzone :: Float
maxTamanoDeadzone = 500.0  

colorDeadzone :: SDL.V3 DW.Word8
colorDeadzone = SDL.V3 255 0 0
grosorLineaDeadzone :: Float
grosorLineaDeadzone = 2.0


-- Función pura para actualizar el valor de la camara en el monadeState
actualizarCamara :: Types.Input -> Types.Jugador -> Bool -> Types.Camara -> Types.Camara
actualizarCamara input target hayPeligro camActual = camActual
        { Types.posCamara    = finalCamPos
        , Types.deadzoneSize = finalDZ 
        , Types.zoomLevel    = finalZoom -- Zoom visual suavizado
        , Types.zoomBase     = newBase   -- Zoom configurado por usuario
        }
  where
    -- Gestionar el Zoom Base
    currentBase = Types.zoomBase camActual
    
    newBase 
        | Types.zoomIn input  = min zoomMaxUsuario (currentBase + velZoomManual)
        | Types.zoomOut input = max zoomMinimo (currentBase - velZoomManual)
        | otherwise           = currentBase

    -- Si hay peligro, añadimos el boost, respetando el máximo absoluto
    targetZoom = if hayPeligro 
                 then min zoomMaxAbsoluto (newBase + zoomBoostPeligro) 
                 else newBase

    -- Suavizado (Interpolación) hacia el target
    currentZoom = Types.zoomLevel camActual
    diffZoom = targetZoom - currentZoom
    
    finalZoom 
        | abs diffZoom < umbralEstabilidad = targetZoom
        | otherwise                        = currentZoom + (diffZoom * factorSuavizado)


    -- Actualización de la Deadzone y Posición
    currentDZ = Types.deadzoneSize camActual
    cPos      = Types.posCamara camActual
    pPos      = Types.posJugador target
    pSize     = Types.tamJugador target
    
    dzCandidata 
        | Types.increaseDZ input = currentDZ + SDL.V2 velAjusteDeadzone velAjusteDeadzone
        | Types.decreaseDZ input = currentDZ - SDL.V2 velAjusteDeadzone velAjusteDeadzone
        | otherwise              = currentDZ

    (SDL.V2 pAW pAH) = pSize
    (SDL.V2 w h)     = dzCandidata
    
    finalW           = max pAW (min maxTamanoDeadzone w)
    finalH           = max pAH (min maxTamanoDeadzone h)
    finalDZ          = SDL.V2 finalW finalH
    
    (SDL.V2 diffX diffY) = pPos - cPos
    (SDL.V2 dzW dzH)     = finalDZ
    limitX = dzW / 2
    limitY = dzH / 2
    
    (SDL.V2 pW pH) = pSize
    
    -- Calculo de posición X
    newCamX 
        | (diffX + pW) > limitX = cPos + SDL.V2 ((diffX + pW) - limitX) 0
        | diffX < -limitX       = cPos + SDL.V2 (diffX + limitX) 0
        | otherwise             = cPos

    -- Calculo de posición Y
    finalCamPos 
        | (diffY + pH) > limitY = newCamX + SDL.V2 0 ((diffY + pH) - limitY)
        | diffY < -limitY       = newCamX + SDL.V2 0 (diffY + limitY)
        | otherwise             = newCamX

dibujarDeadzone :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> IO ()
dibujarDeadzone renderer texture (SDL.V2 w h) zoom = do
    let x = (fromIntegral GD.screenWidth - w) / 2
    let y = (fromIntegral GD.screenHeight - h) / 2
    
    -- Definimos las 4 esquinas en coordenadas de pantalla
    let tl = SDL.V2 x y
    let tr = SDL.V2 (x + w) y
    let br = SDL.V2 (x + w) (y + h)
    let bl = SDL.V2 x (y + h)
    
    -- Usamos un centro de cámara "falso" (el centro de la pantalla) porque
    -- la deadzone es un elemento de UI, fijo relativo a la pantalla.
    let uiCam = GD.screenCenter 

    GD.dibujarLinea renderer texture uiCam zoom tl tr grosorLineaDeadzone colorDeadzone
    GD.dibujarLinea renderer texture uiCam zoom tr br grosorLineaDeadzone colorDeadzone
    GD.dibujarLinea renderer texture uiCam zoom br bl grosorLineaDeadzone colorDeadzone
    GD.dibujarLinea renderer texture uiCam zoom bl tl grosorLineaDeadzone colorDeadzone