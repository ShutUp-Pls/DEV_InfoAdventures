module Globals.Camara where

-- Módulos del sistema
import qualified SDL
import qualified Data.Word  as DW
import qualified Lens.Micro as LMi
import qualified Linear.Metric as LMe -- Necesario para calcular distancias
-- Módulos propios
import qualified Types
import qualified Personajes.Types   as PType
import qualified Objetos.Types      as OType
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD

-- Cuánto cambia el zoom por frame al pulsar +/-
velZoomManual :: Float
velZoomManual = 0.02
-- El zoom más alejado permitido
zoomMinimo :: Float
zoomMinimo = 0.25
-- El zoom más cercano permitido manualmente
zoomMaxUsuario :: Float
zoomMaxUsuario = 3.0       
-- Cuanto zoom extra se añade MÁXIMO cuando te ven
zoomBoostPeligro :: Float
zoomBoostPeligro = 0.4     -- Aumentado ligeramente para notar más el efecto
-- Tope máximo total (Manual + Peligro)
zoomMaxAbsoluto :: Float
zoomMaxAbsoluto = 5.0      
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
-- Distancia en píxeles (mundo) donde la cámara empieza a cerrarse
radioPeligro :: Float
radioPeligro = 500.0 

colorDeadzone :: SDL.V4 DW.Word8
colorDeadzone = SDL.V4 255 0 0 255
grosorLineaDeadzone :: Float
grosorLineaDeadzone = 2.0

actualizarCamara :: Types.Input -> PType.Jugador -> [PType.Zombie] -> OType.Camara -> OType.Camara
actualizarCamara input target enemigos camActual = camActual
        LMi.& OType.posCamara    LMi..~ finalCamPos
        LMi.& OType.deadzoneSize LMi..~ finalDZ 
        LMi.& OType.zoomLevel    LMi..~ finalZoom
        LMi.& OType.zoomBase     LMi..~ newBase
  where
    currentBase = camActual LMi.^. OType.zoomBase
    newBase 
        | input LMi.^. Types.zoomIn  = min zoomMaxUsuario (currentBase + velZoomManual)
        | input LMi.^. Types.zoomOut = max zoomMinimo (currentBase - velZoomManual)
        | otherwise              = currentBase

    pPos      = target LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    calcAmenaza :: PType.Zombie -> Float
    calcAmenaza zombie = 
        let zPos = zombie LMi.^. PType.zmbEnt . GType.entBox . GType.boxPos
            dist = LMe.distance pPos zPos
        in if dist < radioPeligro 
           then 1.0 - (dist / radioPeligro)
           else 0.0

    maxAmenaza = if null enemigos 
                 then 0.0 
                 else maximum (map calcAmenaza enemigos)

    targetZoom = min zoomMaxAbsoluto (newBase + (zoomBoostPeligro * maxAmenaza))

    currentZoom = camActual LMi.^. OType.zoomLevel
    diffZoom = targetZoom - currentZoom
    finalZoom 
        | abs diffZoom < umbralEstabilidad = targetZoom
        | otherwise                        = currentZoom + (diffZoom * factorSuavizado)

    currentDZ = camActual LMi.^. OType.deadzoneSize
    cPos      = camActual LMi.^. OType.posCamara
    pSize     = target LMi.^. PType.jugEnt . GType.entBox . GType.boxTam
    (SDL.V2 diffX diffY) = pPos - cPos
    
    dzCandidata 
        | input LMi.^. Types.increaseDZ = currentDZ + SDL.V2 velAjusteDeadzone velAjusteDeadzone
        | input LMi.^. Types.decreaseDZ = currentDZ - SDL.V2 velAjusteDeadzone velAjusteDeadzone
        | otherwise                 = currentDZ

    (SDL.V2 pAW pAH) = pSize
    (SDL.V2 w h)     = dzCandidata
    (SDL.V2 pW pH)   = pSize

    finalW           = max pAW (min maxTamanoDeadzone w)
    finalH           = max pAH (min maxTamanoDeadzone h)
    finalDZ          = SDL.V2 finalW finalH
    (SDL.V2 dzW dzH) = finalDZ

    limitX = dzW / 2
    limitY = dzH / 2
    newCamX 
        | (diffX + pW) > limitX = cPos + SDL.V2 ((diffX + pW) - limitX) 0
        | diffX < -limitX       = cPos + SDL.V2 (diffX + limitX) 0
        | otherwise             = cPos
    finalCamPos 
        | (diffY + pH) > limitY = newCamX + SDL.V2 0 ((diffY + pH) - limitY)
        | diffY < -limitY       = newCamX + SDL.V2 0 (diffY + limitY)
        | otherwise             = newCamX

dibujarDeadzone :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> IO ()
dibujarDeadzone renderer texture (SDL.V2 w h) zoom = do
    let x = (fromIntegral GD.screenWidth - w) / 2
    let y = (fromIntegral GD.screenHeight - h) / 2
    let tl = SDL.V2 x y
    let tr = SDL.V2 (x + w) y
    let br = SDL.V2 (x + w) (y + h)
    let bl = SDL.V2 x (y + h)
    let uiCam = GD.screenCenter 

    GD.dibujarLinea renderer texture uiCam zoom tl tr grosorLineaDeadzone colorDeadzone
    GD.dibujarLinea renderer texture uiCam zoom tr br grosorLineaDeadzone colorDeadzone
    GD.dibujarLinea renderer texture uiCam zoom br bl grosorLineaDeadzone colorDeadzone
    GD.dibujarLinea renderer texture uiCam zoom bl tl grosorLineaDeadzone colorDeadzone