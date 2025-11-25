{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
-- import Linear (V2(..), V4(..)) 
import Control.Monad (unless)
import Control.Monad.State (execState)
import Foreign.C.Types (CInt)
import Data.Text (Text)

import Types
import Juego

-- Configuración de ventana
screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600
titulo :: Text
titulo = "Juego full Haskell"

main :: IO ()
main = do
    -- Inicializar SDL
    SDL.initializeAll

    -- Crear la ventana
    window <- SDL.createWindow titulo SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
    
    -- Crear renderizador (aceleración por hardware)
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    -- Iniciar el bucle del juego con el estado inicial
    loop renderer estadoInicial

    -- Limpiar al salir
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

-- El bucle principal (Game Loop)
loop :: SDL.Renderer -> GameState -> IO ()
loop renderer currentState = do

    -- Manejo de Eventos (Inputs)
    events <- SDL.pollEvents
    let eventPayloads = map SDL.eventPayload events
    let quitEvent = any (== SDL.QuitEvent) eventPayloads
    
    -- Mapear teclado al Input
    keyboardState <- SDL.getKeyboardState
    let input = Input
          { up    = keyboardState SDL.ScancodeW
          , down  = keyboardState SDL.ScancodeS
          , left  = keyboardState SDL.ScancodeA
          , right = keyboardState SDL.ScancodeD
          , shift = keyboardState SDL.ScancodeLShift
          }

    -- Usamos execState para correr nuestra mónada de estado y obtener el nuevo estado
    let newState = execState (updateGame input) currentState

    -- Renderizado (IO)
    renderGame renderer newState

    -- Control de Frames y Recursión
    SDL.delay 16 -- Aproximadamente 60 FPS
    unless quitEvent (loop renderer newState)

-- Helper necesario para convertir coordenadas (Requerido por dibujarObstaculo)
toSDLRect :: SDL.V2 Float -> SDL.V2 Float -> SDL.Rectangle CInt
toSDLRect (SDL.V2 x y) (SDL.V2 w h) = 
    SDL.Rectangle (SDL.P (SDL.V2 (round x) (round y))) (SDL.V2 (round w) (round h))

-- Función para dibujar los obstaculos de colisión
dibujarObstaculo :: SDL.Renderer -> Obstaculo -> IO ()
dibujarObstaculo renderer (Obstaculo pos size) = do
    let rect = toSDLRect pos size
    SDL.fillRect renderer (Just rect)

-- Función de dibujado
renderGame :: SDL.Renderer -> GameState -> IO ()
renderGame renderer gs = do
    let player = jugador gs
    let pos  = posJugador player
    let size = tamJugador player

    let playerSkin = toSDLRect pos size
    let listaObstaculos = mapa gs

    -- Limpiar pantalla (Fondo negro)
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.clear renderer

    -- Dibujamos los obstaculos de colisión
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 100 100 255
    mapM_ (dibujarObstaculo renderer) listaObstaculos

    -- Extraemos la posición
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    SDL.fillRect renderer (Just playerSkin)

    -- Mostrar en pantalla
    SDL.present renderer