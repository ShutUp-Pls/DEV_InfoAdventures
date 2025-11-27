{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Módulos del sistema
import qualified SDL
import qualified SDL.Font as Font
import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS

-- Módulos propios
import qualified Types
import qualified Juego
import qualified Inicio
import qualified Graficos.Dibujado as RD
import qualified Graficos.Render as RR

main :: IO ()
main = do
    -- Inicializar SDL
    SDL.initializeAll
    Font.initialize

    -- Crear la ventana y cargar fuente
    window <- SDL.createWindow "Juego full Haskell" SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 RD.screenWidth RD.screenHeight }
    font <- Font.load "assets/font.ttf" 24
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    surface <- SDL.createRGBSurface (SDL.V2 1 1) SDL.RGB888
    
    -- Pintar la superficie de blanco y liberarla una vez creada la textura
    SDL.surfaceFillRect surface Nothing (SDL.V4 255 255 255 255)
    blockTexture <- SDL.createTextureFromSurface renderer surface
    skinTexture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface

    -- Crear renderizado e iniciar loop con el estado inicial del juego
    loop renderer font blockTexture skinTexture Inicio.estadoInicial

    -- Limpiar al salir
    Font.free font
    Font.quit
    SDL.destroyTexture blockTexture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

-- El bucle principal
loop :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.GameState -> IO ()
loop renderer font blockTexture skinTexture currentState = do

    -- Manejo de Eventos
    events <- SDL.pollEvents
    let eventPayloads = map SDL.eventPayload events
    let quitEvent = SDL.QuitEvent `elem` eventPayloads

    -- Mapear teclado al Input
    keyboardState <- SDL.getKeyboardState
    let input = Types.Input
          { Types.arriba     = keyboardState SDL.ScancodeW
          , Types.abajo      = keyboardState SDL.ScancodeS
          , Types.izquierda  = keyboardState SDL.ScancodeA
          , Types.derecha    = keyboardState SDL.ScancodeD
          , Types.shift      = keyboardState SDL.ScancodeLShift
          , Types.decreaseDZ = keyboardState SDL.ScancodeO
          , Types.increaseDZ = keyboardState SDL.ScancodeP
          }

    -- Lógica del juego (monadeState)
    let newState = CMS.execState (Juego.updateGame input) currentState

    -- Renderizado
    RR.renderGame renderer font blockTexture skinTexture newState

    -- Control de Frames y Recursión [1000ms/60fps=16.66...delay]
    SDL.delay 16
    CM.unless quitEvent (loop renderer font blockTexture skinTexture newState)