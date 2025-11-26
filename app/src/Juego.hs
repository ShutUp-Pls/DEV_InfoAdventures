module Juego where

import Linear.V2 (V2(..))
import Control.Monad.State

import Types
import Camara
import Mapas.Mapa

import Personajes.Jugador
import Personajes.Enemigo

import Fisica.MovJugador
import Fisica.MovEnemigo

-- Estado inicial del juego
estadoInicial :: GameState
estadoInicial = GameState
    { jugador = nuevoJugador
    , enemigo = nuevoEnemigo
    , mapa    = cargarMapa
    , camara  = Camara
        { posCamara    = V2 400 300
        , deadzoneSize = V2 100 100
        }
    }

-- La función principal de lógica usando monadState
-- Modifica el estado del juego basado en el input.
updateGame :: Input -> State GameState ()
updateGame input = do
    -- Obtener el estado actual del juego
    gameState <- get

    -- Extraer el jugador y mapa actuales
    let jugadorActual = jugador gameState
    let enemigoActual = enemigo gameState
    let mapaActual    = mapa gameState
    let camaraActual  = camara gameState

    -- Actualizamos posicion del jugador
    let jugadorFin = moverJugador input jugadorActual mapaActual

    -- Actualizamos posicion del enemigo
    let deltaE = calcularDirEnemigo enemigoActual jugadorActual
    let enemigoFin = moverEnemigo enemigoActual deltaE mapaActual

    -- Actualizamos la camara a partir del jugador final
    let camaraFin = actualizarCamara input jugadorFin camaraActual

    -- Se implementan los cambios
    put $ gameState 
        {
          jugador = jugadorFin
        , enemigo = enemigoFin
        , camara = camaraFin
        }
    return ()