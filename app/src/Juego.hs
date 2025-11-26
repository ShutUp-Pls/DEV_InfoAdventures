module Juego where

import Linear.V2 (V2(..))
import Control.Monad.State
import Types
import Personajes.Jugador
import Mapas.Mapa
import Fisica.Colisiones (checkColision)

-- Estado inicial del juego
estadoInicial :: GameState
estadoInicial = GameState
    { jugador = nuevoJugador
    , mapa    = cargarMapa
    }

-- La función principal de lógica usando monadState
-- Modifica el estado del juego basado en el input.
updateGame :: Input -> State GameState ()
updateGame input = do
    -- Obtener el estado actual del juego
    gameState <- get
    -- Extraer el jugador y mapa actuales
    let jugadorActual = jugador gameState
    let mapaActual    = mapa gameState

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

    -- Actualizar el estado con el nuevo jugador
    put $ gameState { jugador = jugadorFinal }
    return ()