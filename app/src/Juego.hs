module Juego where

import Control.Monad.State
import Types
import Personajes.Jugador
import Mapas.Mapa

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
    -- Obtener el estado actual
    gameState <- get
    -- Extraer el jugador actual
    let jugadorActual = jugador gameState
    -- Calcular el nuevo jugador
    let jugadorNuevo = if shift input
                    then moverJugadorCorrer input jugadorActual
                    else moverJugadorCaminar input jugadorActual
    -- Actualizar el estado con el nuevo jugador
    put $ gameState { jugador = jugadorNuevo }

    return ()