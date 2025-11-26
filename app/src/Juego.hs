module Juego where

import Linear.V2 (V2(..))
import Control.Monad.State

import Linear.Metric (normalize)
import Linear.Vector ((^*))

import Types
import Camara
import Items

import Mapas.Mapa

import Personajes.Jugador
import Personajes.Enemigo

import Fisica.Colisiones
import Fisica.MovJugador
import Fisica.MovEnemigo

-- Items iniciales
itemsIniciales :: [Item]
itemsIniciales = 
    [ Item (V2 300 300) (V2 20 20) (Vida 10) True
    , Item (V2 500 100) (V2 20 20) (Velocidad 3) True
    ]

-- Estado inicial del juego
estadoInicial :: GameState
estadoInicial = GameState
    { jugador  = nuevoJugador
    , enemigos = enemigosIniciales
    , items    = itemsIniciales
    , mapa     = cargarMapa
    , camara   = Camara
        { posCamara    = V2 400 300
        , deadzoneSize = V2 100 100
        }
    }

resolverCombate :: Jugador -> [Enemigo] -> (Jugador, [Enemigo])
resolverCombate jug enemigosList = 
    foldr logicaColision (jug, []) enemigosList
  where
    -- Esta función acumula los enemigos procesados y actualiza al jugador acumulado
    logicaColision :: Enemigo -> (Jugador, [Enemigo]) -> (Jugador, [Enemigo])
    logicaColision enem (jActual, enemigosProcesados) =
        if haySolapamiento (posJugador jActual) (tamJugador jActual) (posEnemigo enem) (tamEnemigo enem)
        then
            let
                -- 1. Calcular dirección del impacto (desde el enemigo hacia el jugador)
                diff = posJugador jActual - posEnemigo enem
                -- Evitamos division por cero si spawnearon en el mismo pixel exacto
                dir  = if diff == V2 0 0 then V2 1 0 else normalize diff
                
                -- 2. Aplicar "Empuje" instantáneo (Kickback)
                -- El jugador sale disparado con la fuerza del enemigo
                nuevoVelJ = dir ^* empujeE enem
                
                -- El enemigo sale disparado en dirección contraria con fuerza del jugador
                nuevoVelE = (dir ^* (-1)) ^* empujeJ jActual
                
                jDañado = jActual { vidJugador = vidJugador jActual - 5.0
                                  , velGolpeJ  = nuevoVelJ -- Usamos velGolpeJ
                                  }
                
                enemGolpeado = enem { velGolpeE = nuevoVelE }
            in
                (jDañado, enemGolpeado : enemigosProcesados)
        else
            (jActual, enem : enemigosProcesados)

-- La función principal de lógica usando monadState
-- Modifica el estado del juego basado en el input.
updateGame :: Input -> State GameState ()
updateGame input = do
    -- Obtener el estado actual del juego
    gameState <- get

    -- Extraer el jugador y mapa actuales
    let jugadorActual = jugador gameState
    let enemigosList   = enemigos gameState
    let mapaActual    = mapa gameState
    let camaraActual  = camara gameState

    let (jugadorGolpeado, enemigosGolpeados) = resolverCombate jugadorActual enemigosList

    -- Actualizamos posicion del jugador
    let jugadorFin = moverJugador input jugadorGolpeado mapaActual

    let itemsTocados = checkColisionsItems jugadorFin (items gameState)
    let itemsRestantes = filter (\it -> not (it `elem` itemsTocados)) (items gameState)
    let jugadorConPowerUps = foldr (\it jug -> aplicarEfecto (tipoItem it) jug) jugadorFin itemsTocados

    -- Actualizamos posicion del enemigo
    let enemigosFin = map (\enemigo -> 
            let delta = calcularDirEnemigo enemigo jugadorConPowerUps
            in moverEnemigo enemigo delta mapaActual
         ) enemigosGolpeados

    -- Actualizamos la camara a partir del jugador final
    let camaraFin = actualizarCamara input jugadorConPowerUps camaraActual

    -- Se implementan los cambios
    put $ gameState 
        { jugador  = jugadorConPowerUps
        , items    = itemsRestantes
        , enemigos = enemigosFin
        , camara   = camaraFin
        }
    return ()