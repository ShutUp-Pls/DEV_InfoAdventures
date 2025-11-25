module Juego where

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

    -- Calcular el movimiento del nuevo jugador
    let jugadorCandidato = if shift input
                       then moverJugadorCorrer input jugadorActual
                       else moverJugadorCaminar input jugadorActual

    -- Verificamos si el nuevo jugador colisiona con el mapa
    let hayChoque = checkColision jugadorCandidato mapaActual
    let jugadorFinal = if hayChoque
                       then jugadorActual
                       else jugadorCandidato

    -- Actualizar el estado con el nuevo jugador
    put $ gameState { jugador = jugadorFinal }

    return ()