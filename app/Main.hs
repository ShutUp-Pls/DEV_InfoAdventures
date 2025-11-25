{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
-- import Linear (V2(..), V4(..))
import Control.Monad (unless)
import Control.Monad.State (execState)
import Foreign.C.Types (CInt)
import Data.Text (Text)

import Types
import Juego

-- | Configuración de ventana
screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600
titulo :: Text
titulo = "Juego full Haskell"

main :: IO ()
main = do
    -- Inicializar SDL
    initializeAll

    -- Crear la ventana
    window <- createWindow titulo defaultWindow
        { windowInitialSize = V2 screenWidth screenHeight }
    
    -- Crear renderizador (aceleración por hardware)
    renderer <- createRenderer window (-1) defaultRenderer

    -- Iniciar el bucle del juego con el estado inicial
    loop renderer estadoInicial

    -- Limpiar al salir
    destroyRenderer renderer
    destroyWindow window
    quit

-- El bucle principal (Game Loop)
loop :: Renderer -> GameState -> IO ()
loop renderer currentState = do

    -- Manejo de Eventos (Inputs)
    events <- pollEvents
    let eventPayloads = map eventPayload events
    let quitEvent = any (== QuitEvent) eventPayloads
    
    -- Mapear teclado al Input
    keyboardState <- getKeyboardState
    let input = Input
          { up    = keyboardState ScancodeW
          , down  = keyboardState ScancodeS
          , left  = keyboardState ScancodeA
          , right = keyboardState ScancodeD
          }

    -- Usamos execState para correr nuestra mónada de estado y obtener el nuevo estado
    let newState = execState (updateGame input) currentState

    -- Renderizado (IO)
    renderGame renderer newState

    -- Control de Frames y Recursión
    delay 16 -- Aproximadamente 60 FPS
    unless quitEvent (loop renderer newState)

-- Función de dibujado
renderGame :: Renderer -> GameState -> IO ()
renderGame renderer gs = do
    -- Limpiar pantalla (Fondo negro)
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    -- Dibujar Jugador (Cuadrado blanco)
    let (Jugador (V2 x y) _) = jugador gs
    let playerRect = Rectangle (P (V2 x y)) (V2 30 30) -- Tamaño 30x30
    
    rendererDrawColor renderer $= V4 255 255 255 255
    fillRect renderer (Just playerRect)

    -- Mostrar en pantalla
    present renderer