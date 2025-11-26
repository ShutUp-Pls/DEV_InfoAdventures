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
titulo :: Text
titulo = "Juego full Haskell"

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

screenCenter :: SDL.V2 Float
screenCenter = SDL.V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

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
          { arriba     = keyboardState SDL.ScancodeW
          , abajo      = keyboardState SDL.ScancodeS
          , izquierda  = keyboardState SDL.ScancodeA
          , derecha    = keyboardState SDL.ScancodeD
          , shift      = keyboardState SDL.ScancodeLShift
          , decreaseDZ = keyboardState SDL.ScancodeO
          , increaseDZ = keyboardState SDL.ScancodeP
          }

    -- Usamos execState para correr nuestra mónada de estado y obtener el nuevo estado
    let newState = execState (updateGame input) currentState

    -- Renderizado (IO)
    renderGame renderer newState

    -- Control de Frames y Recursión
    SDL.delay 16 -- Aproximadamente 60 FPS
    unless quitEvent (loop renderer newState)

-- Helper necesario para convertir coordenadas
toSDLRect :: SDL.V2 Float -> SDL.V2 Float -> SDL.Rectangle CInt
toSDLRect (SDL.V2 x y) (SDL.V2 w h) = 
    SDL.Rectangle (SDL.P (SDL.V2 (round x) (round y))) (SDL.V2 (round w) (round h))

-- Gestiona la posición del mundo dependiendo del tipo de camara actual
worldToScreen :: SDL.V2 Float -> SDL.V2 Float -> SDL.V2 Float
worldToScreen worldPos camPos = worldPos - camPos + screenCenter

-- Para efectos de testeo, dibujamos la DeadZone
dibujarDeadzone :: SDL.Renderer -> SDL.V2 Float -> IO ()
dibujarDeadzone renderer (SDL.V2 w h) = do
    let topLeftX = (fromIntegral screenWidth - w) / 2
    let topLeftY = (fromIntegral screenHeight - h) / 2
    let rect = toSDLRect (SDL.V2 topLeftX topLeftY) (SDL.V2 w h)
    
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255 -- Rojo
    SDL.drawRect renderer (Just rect)

-- Función para dibujar los obstaculos de colisión
dibujarObstaculo :: SDL.Renderer -> SDL.V2 Float -> Obstaculo -> IO ()
dibujarObstaculo renderer camPos (Obstaculo pos size) = do
    let screenPos = worldToScreen pos camPos
    let rect = toSDLRect screenPos size
    SDL.fillRect renderer (Just rect)

-- Función de dibujado
renderGame :: SDL.Renderer -> GameState -> IO ()
renderGame renderer gs = do
    let player = jugador gs
    let posJ   = posJugador player
    let tamJ   = tamJugador player

    let enemy = enemigo gs
    let posE  = posEnemigo enemy
    let tamE  = tamEnemigo enemy

    let cam    = camara gs
    let camPos = posCamara cam
    let dzSize = deadzoneSize cam

    -- Limpiar pantalla (Fondo negro)
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
    SDL.clear renderer

    -- Dibujamos los obstaculos de colisión
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 100 100 255
    mapM_ (dibujarObstaculo renderer camPos) (mapa gs)

    -- Dibujamos al enemigo
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 255 0 255
    let enemyScreePos = worldToScreen posE camPos
    let enemySkin = toSDLRect enemyScreePos tamE
    SDL.fillRect renderer (Just enemySkin)

    -- Dibujamos al jugador
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    let playerScreenPos = worldToScreen posJ camPos
    let playerSkin = toSDLRect playerScreenPos tamJ
    SDL.fillRect renderer (Just playerSkin)
    
    -- Mostrar en pantalla
    dibujarDeadzone renderer dzSize
    SDL.present renderer