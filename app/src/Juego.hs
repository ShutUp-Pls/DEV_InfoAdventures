module Juego where

import Control.Monad.State
import Types
import Personajes.Jugador
import Mapas.Mapa

-- | Estado inicial del juego
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
    let currentPlayer = jugador gameState
    -- Calcular el nuevo jugador usando lógica pura
    let newPlayer = moverJugador input currentPlayer
    -- Actualizar el estado con el nuevo jugador
    put $ gameState { jugador = newPlayer }

    return ()