module Personajes.Jugador where

-- Modulos del sistema
import SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Graficos.Dibujado as GD
import qualified Fisica.Movimiento as FM
import qualified Objetos.Cono as OC 

actFisicasMovJugador :: Types.Input -> [Types.Obstaculo] -> CMS.State Types.Jugador ()
actFisicasMovJugador input mapObstaculos = do
    jugador <- CMS.get

    let dirX = (if Types.derecha input then 1 else 0) - (if Types.izquierda input then 1 else 0)
    let dirY = (if Types.abajo input then 1 else 0)  - (if Types.arriba input then 1 else 0)
    let vecDireccion = SDL.V2 dirX dirY

    let velCor = Types.velCorrerJ jugador
        velCam = Types.velCaminarJ jugador
        velRot = Types.velRotacion jugador
        factor = Types.velFactorJ jugador
        velocidadBase = if Types.shift input then velCor else velCam

    let velocidadInput = if vecDireccion == SDL.V2 0 0 
                         then SDL.V2 0 0 
                         else LM.normalize vecDireccion LV.^* (velocidadBase * factor)

    velFinal <- FM.resolverFisica velocidadInput mapObstaculos
    let anguloActual = Types.angJugador jugador
    let nuevoAngulo = if vecDireccion == SDL.V2 0 0
                      then anguloActual
                      else 
                          let 
                              rads = atan2 dirY dirX
                              targetAng = rads * (180 / pi)
                          in 
                              Utils.suavizarAngulo anguloActual targetAng velRot

    CMS.modify $ \s -> s { Types.velJugador = LM.norm velFinal
                         , Types.angJugador = nuevoAngulo 
                         }

moverJugador :: Types.Input -> Types.Jugador -> [Types.Obstaculo] -> Types.Jugador
moverJugador input jugadorIni mapObstaculos = 
    CMS.execState (actFisicasMovJugador input mapObstaculos) jugadorIni

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Types.Jugador -> IO ()
dibujar renderer skinTexture camPos player = do
    let posJ = Types.posJugador player
    let tamJ = Types.tamJugador player
    let angJ = Types.angJugador player
    
    -- Dibujamos la textura base (Blanco)
    GD.dibujarTextura renderer skinTexture camPos posJ tamJ angJ (SDL.V3 255 255 255)

    -- Debug outline (Visión)
    let centroJ = posJ + (tamJ LV.^* 0.5)
    OC.dibujarConoOutline renderer skinTexture camPos centroJ angJ 60 30