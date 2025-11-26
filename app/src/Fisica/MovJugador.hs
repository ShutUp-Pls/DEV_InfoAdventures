module Fisica.MovJugador where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Fisica.Colisiones as FC

-- Velocidades del jugador
friccion :: Float
friccion = 0.90

umbralParada :: Float
umbralParada = 0.5

-- Función para calcular el vector de desplazamiento
calcularDirJugador :: Types.Input -> Float -> Float -> SDL.V2 Float
calcularDirJugador input velocidad factor =
    let 
        -- Obtenemos la dirección pura (solo -1, 0 o 1)
        dirX = (if Types.derecha input then 1 else 0) - (if Types.izquierda input then 1 else 0)
        dirY = (if Types.abajo input then 1 else 0)  - (if Types.arriba input then 1 else 0)
        direccion = SDL.V2 dirX dirY
    in 
        -- Verificamos si hay movimiento para evitar dividir por cero al normalizar
        if direccion == SDL.V2 0 0 
            then SDL.V2 0 0 
            else LM.normalize direccion LV.^* (velocidad*factor)

-- Mover al jugador 
moverJugador :: Types.Input -> Types.Jugador -> [Types.Obstaculo] -> Types.Jugador
moverJugador input jugadorIni mapObstaculos = 
    let 
        -- Calculamos la velocidad que tendrá el jugador
        velCor = Types.velCorrerJ jugadorIni
        velCam = Types.velCaminarJ jugadorIni
        factor = Types.velFactorJ jugadorIni
        rapidez = (if Types.shift input then velCor else velCam) * factor

        -- Resolvemos la velocidad del empuje si corresponde
        esEmpujado = LM.norm (Types.velGolpeJ jugadorIni) > umbralParada
        velocidad = if esEmpujado
                    then Types.velGolpeJ jugadorIni
                    else if Types.shift input
                         then calcularDirJugador input velCor factor 
                         else calcularDirJugador input velCam factor
        (SDL.V2 dx dy) = velocidad

        -- Verificamos si choca en X (Resolvemos posición en X)
        posIniJ = Types.posJugador jugadorIni
        posIniJX = posIniJ + SDL.V2 dx 0
        jugadorMovX = jugadorIni { Types.posJugador = posIniJX }
        jugadorMid = if FC.checkColision jugadorMovX mapObstaculos 
                     then jugadorIni 
                     else jugadorMovX

        -- Verificamos si choca en Y [A partir de X Resuelto] (Resolvemos posición en Y)
        posMidJ = Types.posJugador jugadorMid
        posMidJY = posMidJ + SDL.V2 0 dy
        jugadorMovY = jugadorMid { Types.posJugador = posMidJY }
        jugadorFin = if FC.checkColision jugadorMovY mapObstaculos
                     then jugadorMid
                     else jugadorMovY

        nuevoVelGolpe = if esEmpujado
                        then Types.velGolpeJ jugadorIni LV.^* friccion
                        else SDL.V2 0 0
        
    in jugadorFin 
        { Types.velGolpeJ = nuevoVelGolpe
        , Types.velJugador = if esEmpujado 
                       then LM.norm nuevoVelGolpe
                       else rapidez
        }