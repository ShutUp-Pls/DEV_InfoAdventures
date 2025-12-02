{-# LANGUAGE OverloadedStrings #-}
module Main where
-- Módulos del sistema
import qualified SDL
import qualified SDL.Font               as Font
import qualified Control.Monad.State    as CMS
import qualified Lens.Micro             as LMi
-- Módulos propios
import qualified Juego
import qualified Inicio
import qualified Launcher
import qualified Types
import qualified Personajes.Types       as PType
import qualified Globals.Types          as GType
import qualified Graficos.Dibujado      as RD
import qualified Graficos.Render        as RR

main :: IO ()
main = do
    SDL.initializeAll
    Font.initialize
    window <- SDL.createWindow "Juego full Haskell" SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 RD.screenWidth RD.screenHeight }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    appFlow renderer window True
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit

appFlow :: SDL.Renderer -> SDL.Window -> Bool -> IO ()
appFlow renderer window lastTutorialState = do
    decision <- Launcher.runLauncher window renderer lastTutorialState
    case decision of
        Launcher.ActionExit -> putStrLn "Saliendo de la aplicación..." 
        Launcher.ActionPlay modoTutorialElegido -> do
            putStrLn $ "Iniciando juego... (Tutorial: " ++ show modoTutorialElegido ++ ")"        
            correrJuego renderer modoTutorialElegido
            appFlow renderer window modoTutorialElegido

correrJuego :: SDL.Renderer -> Bool -> IO ()
correrJuego renderer isTutorial = do
    font <- Font.load "assets/font.ttf" 24
    
    surface <- SDL.createRGBSurface (SDL.V2 1 1) SDL.RGB888
    SDL.surfaceFillRect surface Nothing (SDL.V4 255 255 255 255)
    blockTexture <- SDL.createTextureFromSurface renderer surface
    skinTexture  <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    
    estadoJuego <- Inicio.estadoInicial isTutorial
    
    gameLoop renderer font blockTexture skinTexture estadoJuego

    SDL.destroyTexture skinTexture
    SDL.destroyTexture blockTexture
    Font.free font

gameLoop :: SDL.Renderer -> Font.Font -> SDL.Texture -> SDL.Texture -> Types.GameState -> IO ()
gameLoop renderer font blockTexture skinTexture currentState = do
    events <- SDL.pollEvents
    let eventPayloads = map SDL.eventPayload events
    let userQuit = SDL.QuitEvent `elem` eventPayloads

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
          , Types._teclaSalir   = keyboardState SDL.ScancodeX
          , Types._disparar     = keyboardState SDL.ScancodeJ
          , Types._prevWeapon   = keyboardState SDL.ScancodeK
          , Types._nextWeapon   = keyboardState SDL.ScancodeL
          , Types._espacio      = keyboardState SDL.ScancodeSpace
          }

    let newState = CMS.execState (Juego.actualizarJuego input) currentState

    let vidaActual               = newState LMi.^. Types.jugador LMi.^. PType.jugEnt . GType.entVid . GType.vidAct
    let tiempoRestante           = newState LMi.^. Types.tiempoJuego
    let estaMuerto = vidaActual <= 0 || tiempoRestante <= 0

    let returnToMenu = (estaMuerto && Types._teclaSalir input) || (userQuit)

    RR.renderGame renderer font blockTexture skinTexture input newState
    SDL.delay 16

    if returnToMenu
        then putStrLn "Juego terminado. Volviendo al menú..."
        else gameLoop renderer font blockTexture skinTexture newState