{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Font as Font
import Control.Monad (unless)
import Control.Monad.State (execState)

import Types
import Juego
import Render (renderGame, screenWidth, screenHeight)

main :: IO ()
main = do
    -- Inicializar SDL
    SDL.initializeAll
    Font.initialize

    -- Crear la ventana usando las dimensiones definidas en Render
    window <- SDL.createWindow "Juego full Haskell" SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
    
    -- Crear renderizador y cargar fuente
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    font <- Font.load "assets/font.ttf" 24

    -- Iniciar el bucle del juego con el estado inicial
    loop renderer font estadoInicial

    -- Limpiar al salir
    Font.free font
    Font.quit
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

-- El bucle principal (Game Loop)
loop :: SDL.Renderer -> Font.Font -> GameState -> IO ()
loop renderer font currentState = do

    -- Manejo de Eventos (Inputs)
    events <- SDL.pollEvents
    let eventPayloads = map SDL.eventPayload events
    let quitEvent = any (== SDL.QuitEvent) eventPayloads
    
    -- Mapear teclado al Input
    keyboardState <- SDL.getKeyboardState
    let input = Input
          { arriba     = keyboardState SDL.ScancodeW
          , abajo      = keyboardState SDL.ScancodeS
          , izquierda  = keyboardState SDL.ScancodeA
          , derecha    = keyboardState SDL.ScancodeD
          , shift      = keyboardState SDL.ScancodeLShift
          , decreaseDZ = keyboardState SDL.ScancodeO
          , increaseDZ = keyboardState SDL.ScancodeP
          }

    -- Lógica del juego (Update)
    let newState = execState (updateGame input) currentState

    -- Renderizado (Draw) - Llamada al módulo externo
    renderGame renderer font newState

    -- Control de Frames y Recursión
    SDL.delay 16 -- Aproximadamente 60 FPS
    unless quitEvent (loop renderer font newState)