module Juego where

-- Módulos del sistema
import qualified Control.Monad.State as CMS

-- Módulos propios
import qualified Types

import qualified Objetos.Camara as OC
import qualified Objetos.Items as OI

import qualified Fisica.Colisiones as FC
import qualified Fisica.MovEnemigo as FME

import qualified Personajes.Jugador as PJ
import qualified Personajes.Enemigo as PE

-- La función principal de lógica usando monadState
-- Modifica el estado del juego basado en el input.
updateGame :: Types.Input -> CMS.State Types.GameState ()
updateGame input = do
    -- Obtener el estado actual del juego
    gameState <- CMS.get
    let dt = 0.016

    let jugadorConBuffs = OI.procesarBuffs dt (Types.jugador gameState)

    -- Extraer el jugador y mapa actuales
    let enemigosList = Types.enemigos gameState
    let mapaActual   = Types.mapa gameState
    let camaraActual = Types.camara gameState

    let (jugadorGolpeado, enemigosGolpeados) = FC.resolverCombate jugadorConBuffs enemigosList

    -- Actualizamos posicion del jugador
    let jugadorFin = PJ.moverJugador input jugadorGolpeado mapaActual

    let itemsTocados   = FC.checkColisionsItems jugadorFin (Types.items gameState)
    let itemsRestantes = filter (\it -> not (it `elem` itemsTocados)) (Types.items gameState)
    let jugadorConPowerUps = foldr (\it jug -> OI.aplicarEfecto (Types.tipoItem it) jug) jugadorFin itemsTocados

    -- Actualizamos posicion del enemigo
    let enemigosFin = map (\enemigo -> 
            let delta = FME.calcularDirEnemigo enemigo jugadorConPowerUps
            in PE.moverEnemigo enemigo delta mapaActual
         ) enemigosGolpeados

    -- Actualizamos la camara a partir del jugador final
    let camaraFin = OC.actualizarCamara input jugadorConPowerUps camaraActual

    -- Se implementan los cambios
    CMS.put $ gameState 
        { Types.jugador  = jugadorConPowerUps
        , Types.items    = itemsRestantes
        , Types.enemigos = enemigosFin
        , Types.camara   = camaraFin
        }
    return ()