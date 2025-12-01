{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Launcher (LauncherAction(..), runLauncher) where

import qualified SDL
import qualified SDL.Image              as IMG
import qualified SDL.Font               as Font
import           SDL                    (($=))
import           Control.Monad          as CM
import           Foreign.C.Types        (CInt)
import           Data.Text              (Text)

data LauncherAction
    = ActionPlay Bool 
    | ActionExit
    deriving (Show, Eq)

data ButtonId
    = BtnPlay
    | BtnMap
    | BtnTutorial
    | BtnExit
    deriving (Show, Eq)

data LauncherState = LauncherState
    { finalAction  :: Maybe LauncherAction
    , mousePos     :: (CInt, CInt)
    , tutorialMode :: Bool
    } deriving (Show)

data ButtonDef = ButtonDef
    { btnId    :: ButtonId
    , btnRect  :: SDL.Rectangle CInt
    , btnLabel :: Text
    }

btnW, btnH, btnX, startY, gapY :: CInt
btnW    = 300
btnH    = 60
btnX    = 100
startY  = 250
gapY    = 20

mkRect :: CInt -> CInt -> CInt -> CInt -> SDL.Rectangle CInt
mkRect x y w h = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)

construirBotones :: Bool -> [ButtonDef]
construirBotones isTutorial =
    let textoTutorial = if isTutorial then "Tutorial Activado" else "Tutorial Desactivado"
    in
    [ ButtonDef BtnPlay     (mkRect btnX (startY + 0 * (btnH + gapY)) btnW btnH) "Jugar"
    , ButtonDef BtnMap      (mkRect btnX (startY + 1 * (btnH + gapY)) btnW btnH) "Elegir Mapa"
    , ButtonDef BtnTutorial (mkRect btnX (startY + 2 * (btnH + gapY)) btnW btnH) textoTutorial
    , ButtonDef BtnExit     (mkRect btnX (startY + 3 * (btnH + gapY)) btnW btnH) "Salir"
    ]

pointInRect :: (CInt, CInt) -> SDL.Rectangle CInt -> Bool
pointInRect (mx, my) (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)) =
    mx >= x && mx <= x + w && my >= y && my <= y + h

-- Necesitamos pasar el estado para saber qué botones existen
buttonAt :: (CInt, CInt) -> Bool -> Maybe ButtonId
buttonAt mPos isTutorial = 
    case [ btnId b | b <- construirBotones isTutorial, pointInRect mPos (btnRect b) ] of
        (bid:_) -> Just bid
        []      -> Nothing

handleClick :: LauncherState -> IO LauncherState
handleClick st@LauncherState{..} =
    case buttonAt mousePos tutorialMode of

        Just BtnPlay     -> return st { finalAction = Just (ActionPlay tutorialMode) }
        
        Just BtnMap      -> do
            putStrLn "TODO: Implementar selector de mapas"
            return st
            
        Just BtnTutorial -> return st { tutorialMode = not tutorialMode }
        
        Just BtnExit     -> return st { finalAction = Just ActionExit }
        Nothing          -> return st

renderTextButton :: SDL.Renderer -> Font.Font -> ButtonDef -> Bool -> IO ()
renderTextButton renderer font ButtonDef{..} isHover = do
    let bgColor = if isHover then SDL.V4 100 100 220 255 else SDL.V4 60 60 180 255
    let borderColor = SDL.V4 255 255 255 255
    let textColor = SDL.V4 255 255 255 255

    SDL.rendererDrawColor renderer $= bgColor
    SDL.fillRect renderer (Just btnRect)
    
    SDL.rendererDrawColor renderer $= borderColor
    SDL.drawRect renderer (Just btnRect)

    textSurface <- Font.solid font textColor btnLabel 
    textTexture <- SDL.createTextureFromSurface renderer textSurface
    
    ti <- SDL.queryTexture textTexture
    let textW = SDL.textureWidth ti
    let textH = SDL.textureHeight ti
    let (SDL.Rectangle (SDL.P (SDL.V2 bx by)) (SDL.V2 bw bh)) = btnRect
    let textX = bx + (bw `div` 2) - (textW `div` 2)
    let textY = by + (bh `div` 2) - (textH `div` 2)
    
    SDL.copy renderer textTexture Nothing (Just (mkRect textX textY textW textH))
    
    SDL.destroyTexture textTexture
    SDL.freeSurface textSurface

renderLauncher :: SDL.Renderer -> SDL.Texture -> Font.Font -> LauncherState -> IO ()
renderLauncher renderer bgTexture font st = do
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
    SDL.clear renderer

    SDL.copy renderer bgTexture Nothing Nothing 
    let currentButtons = construirBotones (tutorialMode st)

    mapM_ (\btn -> renderTextButton renderer font btn (pointInRect (mousePos st) (btnRect btn))) currentButtons

    SDL.present renderer

handleEvent :: LauncherState -> SDL.Event -> IO LauncherState
handleEvent st ev =
    case SDL.eventPayload ev of
        SDL.QuitEvent -> return st { finalAction = Just ActionExit }
        
        SDL.KeyboardEvent kEv
            | SDL.keyboardEventKeyMotion kEv == SDL.Pressed
            , SDL.keysymKeycode (SDL.keyboardEventKeysym kEv) == SDL.KeycodeEscape ->
                return st { finalAction = Just ActionExit }
        
        SDL.MouseMotionEvent mEv ->
            let SDL.P (SDL.V2 mx my) = SDL.mouseMotionEventPos mEv
            in return st { mousePos = (fromIntegral mx, fromIntegral my) }

        SDL.MouseButtonEvent mbEv
            | SDL.mouseButtonEventMotion mbEv == SDL.Pressed
            , SDL.mouseButtonEventButton mbEv == SDL.ButtonLeft ->
                handleClick st

        _ -> return st

launcherLoop :: SDL.Renderer -> SDL.Texture -> Font.Font -> LauncherState -> IO LauncherAction
launcherLoop renderer bgTexture font st = do
    events <- SDL.pollEvents
    st' <- foldM handleEvent st events
    
    renderLauncher renderer bgTexture font st'
    
    case finalAction st' of
        Just action -> return action
        Nothing     -> do
            SDL.delay 16 
            launcherLoop renderer bgTexture font st'

runLauncher :: SDL.Renderer -> IO LauncherAction
runLauncher renderer = do
    font <- Font.load "assets/font.ttf" 24
    bgSurface <- IMG.load "assets/fondo-launcher.png" 
    bgTexture <- SDL.createTextureFromSurface renderer bgSurface
    SDL.freeSurface bgSurface

    -- Inicializamos con el tutorial activado por defecto (True) o desactivado (False)
    let initialState = LauncherState 
            { finalAction = Nothing
            , mousePos = (0,0)
            , tutorialMode = True 
            }
    
    result <- launcherLoop renderer bgTexture font initialState
    
    SDL.destroyTexture bgTexture
    Font.free font
    
    return result