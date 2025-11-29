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
    
    -- Crear una textura blanca de 1x1 para usar como placeholder
    surface <- SDL.createRGBSurface (SDL.V2 1 1) SDL.RGB888
    SDL.surfaceFillRect surface Nothing (SDL.V4 255 255 255 255)
    blockTexture <- SDL.createTextureFromSurface renderer surface
    skinTexture  <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface

    -- Crear renderizado e iniciar loop con el estado inicial del juego
    estadoJuego <- Inicio.estadoInicial
    loop renderer font blockTexture skinTexture estadoJuego

    -- Limpiar al salir
    Font.free font
    Font.quit
    SDL.destroyTexture skinTexture
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
    -- NOTA: Al usar Lenses en Types.hs, los campos reales tienen guion bajo (_).
    -- Debemos usar esos nombres (_arriba, _abajo) para construir el registro.
    keyboardState <- SDL.getKeyboardState
    let input = Types.Input
          { Types._arriba       = keyboardState SDL.ScancodeW
          , Types._abajo        = keyboardState SDL.ScancodeS
          , Types._izquierda    = keyboardState SDL.ScancodeA
          , Types._derecha      = keyboardState SDL.ScancodeD
          , Types._shift        = keyboardState SDL.ScancodeLShift
          , Types._decreaseDZ   = keyboardState SDL.ScancodeO
          , Types._increaseDZ   = keyboardState SDL.ScancodeP
          , Types._zoomOut      = keyboardState SDL.ScancodeSlash
          , Types._zoomIn       = keyboardState SDL.ScancodeRightBracket
          , Types._teclaRespawn = keyboardState SDL.ScancodeY
          }

    -- Lógica del juego (ejecutamos el State Monad sobre el estado actual)
    -- Juego.updateGame ahora usa Lenses internamente como definimos antes
    let newState = CMS.execState (Juego.updateGame input) currentState

    -- Renderizado
    RR.renderGame renderer font blockTexture skinTexture newState

    -- Control de Frames y Recursión [1000ms/60fps ≈ 16ms delay]
    SDL.delay 16
    CM.unless quitEvent (loop renderer font blockTexture skinTexture newState)