module Juego where

import Linear.V2 (V2(..))
import Control.Monad.State
import Types
import Personajes.Jugador
import Personajes.Enemigo
import Mapas.Mapa
import Fisica.Colisiones (checkColision)

-- Estado inicial del juego
estadoInicial :: GameState
estadoInicial = GameState
    { jugador = nuevoJugador
    , enemigo = nuevoEnemigo
    , mapa    = cargarMapa
    , camaraPos    = V2 400 300
    , deadzoneSize = V2 100 100
    }

-- La función principal de lógica usando monadState
-- Modifica el estado del juego basado en el input.
updateGame :: Input -> State GameState ()
updateGame input = do
    -- Obtener el estado actual del juego
    gameState <- get

    -- Extraer el jugador y mapa actuales
    let jugadorActual = jugador gameState
    let enemigoActual  = enemigo gameState
    let mapaActual    = mapa gameState

    -- Calcula el factor de cambio por frame del Deadzone
    let currentDZ = deadzoneSize gameState
    let changeSpeed = 5.0
    
    let dzCandidata = if increaseDZ input 
                      then currentDZ + V2 changeSpeed changeSpeed
                      else if decreaseDZ input
                           then currentDZ - V2 changeSpeed changeSpeed
                           else currentDZ
    
    -- Limite para que la Deadzone no sea más pequeño que el jugador ni más grande que la pantalla
    let pASize = tamJugador jugadorActual
    let (V2 pAW pAH) = pASize

    let (V2 w h) = dzCandidata
    let finalW = max pAW (min 700 w)
    let finalH = max pAH (min 500 h)
    let finalDZ = V2 finalW finalH

    -- Obtenemos el jugador en su nueva posición según corresponda
    let jugadorCandidato = if shift input
                           then moverJugadorCorrer input jugadorActual
                           else moverJugadorCaminar input jugadorActual

    -- Calculamos la diferencia de posición para el 'slicing' en diagonal
    let deltaTotal = posJugador jugadorCandidato - posJugador jugadorActual
    let (V2 dx dy) = deltaTotal

    -- Si se puede mover en X, lo movemos, sino, pues no XD (X Resuelto)
    let movX = jugadorActual { posJugador = posJugador jugadorActual + V2 dx 0 }
    let movXr = if checkColision movX mapaActual 
                then jugadorActual 
                else movX

    -- Si, a partir del X resuelto, se puede mover en Y, lo movemos
    let movY = movX { posJugador = posJugador movXr + V2 0 dy }
    let jugadorFinal = if checkColision movY mapaActual 
                       then movXr
                       else movY

    let deltaE = calcularDireccion enemigoActual jugadorActual
    let enemigoFinal = moverEnemigo enemigoActual deltaE mapaActual

    -- Calculamos el posición de la camara según la DeadZone
    let cPos = camaraPos gameState
    let playerSize = tamJugador jugadorFinal
    let (V2 pW pH) = playerSize
    
    -- Calculamos la distancia entre el Jugador y el Centro de la Cámara
    let (V2 diffX diffY) = posJugador jugadorFinal - cPos
    let (V2 dzW dzH) = finalDZ
    
    -- Limites: La mitad del ancho/alto de la deadzone
    let limitX = dzW / 2
    let limitY = dzH / 2

    -- Si la distancia supera el límite, movemos la cámara la diferencia justa
    let newCamX = if (diffX + pW) > limitX
                  then cPos + V2 ((diffX + pW) - limitX) 0
                  else if diffX < -limitX
                       then cPos + V2 (diffX + limitX) 0
                       else cPos

    -- Lo mismo para Y pero usando el actual de X
    let finalCamPos = if (diffY + pH) > limitY
                      then newCamX + V2 0 ((diffY + pH) - limitY) 
                      else if diffY < -limitY
                           then newCamX + V2 0 (diffY + limitY) 
                           else newCamX

    -- Actualizar el estado con el nuevo estado del jugador, la camara y la deadzone
    put $ gameState 
        {
          jugador = jugadorFinal
        , enemigo = enemigoFinal
        , camaraPos = finalCamPos
        , deadzoneSize = finalDZ
        }
    return ()