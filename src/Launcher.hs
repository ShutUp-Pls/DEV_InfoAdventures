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
import qualified Data.Word              as DW

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
    , showNotImpl  :: Bool
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
    let textoTutorial = if isTutorial then "Tutorial: ACTIVADO" else "Tutorial: DESACTIVADO"
    in
    [ ButtonDef BtnPlay     (mkRect btnX (startY + 0 * (btnH + gapY)) btnW btnH) "Jugar"
    , ButtonDef BtnMap      (mkRect btnX (startY + 1 * (btnH + gapY)) btnW btnH) "Elegir Mapa"
    , ButtonDef BtnTutorial (mkRect btnX (startY + 2 * (btnH + gapY)) btnW btnH) textoTutorial
    , ButtonDef BtnExit     (mkRect btnX (startY + 3 * (btnH + gapY)) btnW btnH) "Salir"
    ]

pointInRect :: (CInt, CInt) -> SDL.Rectangle CInt -> Bool
pointInRect (mx, my) (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)) =
    mx >= x && mx <= x + w && my >= y && my <= y + h

buttonAt :: (CInt, CInt) -> Bool -> Maybe ButtonId
buttonAt mPos isTutorial = 
    case [ btnId b | b <- construirBotones isTutorial, pointInRect mPos (btnRect b) ] of
        (bid:_) -> Just bid
        []      -> Nothing

handleClick :: LauncherState -> IO LauncherState
handleClick st@LauncherState{..} =
    if showNotImpl 
    then return st { showNotImpl = False }
    else case buttonAt mousePos tutorialMode of

        Just BtnPlay     -> return st { finalAction = Just (ActionPlay tutorialMode) }
        
        Just BtnMap      -> do
            return st { showNotImpl = True }
            
        Just BtnTutorial -> return st { tutorialMode = not tutorialMode }
        
        Just BtnExit     -> return st { finalAction = Just ActionExit }
        Nothing          -> return st

obtenerColorBoton :: ButtonId -> Bool -> Bool -> SDL.V4 DW.Word8
obtenerColorBoton bId isTutorial isHover = 
    case bId of
        BtnTutorial -> 
            if isTutorial
            then if isHover then SDL.V4 50 200 50 255 else SDL.V4 20 160 20 255 
            else if isHover then SDL.V4 200 50 50 255 else SDL.V4 160 20 20 255 
        _ -> 
            if isHover then SDL.V4 100 100 220 255 else SDL.V4 60 60 180 255 

renderTextButton :: SDL.Renderer -> Font.Font -> ButtonDef -> Bool -> Bool -> IO ()
renderTextButton renderer font ButtonDef{..} isHover isTutorial = do
    let bgColor = obtenerColorBoton btnId isTutorial isHover
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

renderPopup :: SDL.Renderer -> Font.Font -> IO ()
renderPopup renderer font = do
    let popupRect = mkRect 200 250 400 150

    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 100
    SDL.fillRect renderer Nothing

    SDL.rendererDrawColor renderer $= SDL.V4 50 50 50 255
    SDL.fillRect renderer (Just popupRect)

    SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
    SDL.drawRect renderer (Just popupRect)

    let msg1 = "Funcion no implementada"
    let msg2 = "(Click para cerrar)"
    
    let drawText t yOffset color = do
            s <- Font.solid font color t
            tex <- SDL.createTextureFromSurface renderer s
            info <- SDL.queryTexture tex
            let w = SDL.textureWidth info
            let h = SDL.textureHeight info
            let x = 200 + (400 `div` 2) - (w `div` 2)
            let y = 250 + yOffset
            SDL.copy renderer tex Nothing (Just (mkRect x y w h))
            SDL.destroyTexture tex
            SDL.freeSurface s

    drawText msg1 40 (SDL.V4 255 100 100 255)
    drawText msg2 80 (SDL.V4 200 200 200 255)


renderLauncher :: SDL.Window -> SDL.Renderer -> SDL.Texture -> Font.Font -> LauncherState -> IO ()
renderLauncher window renderer bgTexture font st = do
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
    SDL.clear renderer

    tInfo <- SDL.queryTexture bgTexture
    let tW = SDL.textureWidth tInfo
    let tH = SDL.textureHeight tInfo

    windowSize <- SDL.get (SDL.windowSize window)
    let SDL.V2 wW_c wH_c = windowSize

    let wWf = fromIntegral (wW_c :: CInt) :: Double
    let wHf = fromIntegral (wH_c :: CInt) :: Double
    let tHf = fromIntegral (tH :: CInt)   :: Double

    let scale = if tHf == 0 then 1 else wHf / tHf

    let srcWf = wWf / scale
    let srcW_c = fromIntegral (max 1 (floor srcWf :: Int)) :: CInt 

    let srcX_c = max 0 (tW - srcW_c)
    let srcY_c = 0
    let srcH_c = tH

    let srcRect = SDL.Rectangle (SDL.P (SDL.V2 srcX_c srcY_c)) (SDL.V2 srcW_c srcH_c)
    let scaledW_c = fromIntegral (floor (fromIntegral srcW_c * scale) :: Integer) :: CInt
    let scaledH_c = wH_c

    let dstX_c = wW_c - scaledW_c
    let dstY_c = 0

    let dstRect = SDL.Rectangle (SDL.P (SDL.V2 dstX_c dstY_c)) (SDL.V2 scaledW_c scaledH_c)

    SDL.copy renderer bgTexture (Just srcRect) (Just dstRect)


    let currentButtons = construirBotones (tutorialMode st)
    mapM_ (\btn -> renderTextButton renderer font btn (pointInRect (mousePos st) (btnRect btn)) (tutorialMode st)) currentButtons
    if showNotImpl st 
        then renderPopup renderer font
        else return ()

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

launcherLoop :: SDL.Window -> SDL.Renderer -> SDL.Texture -> Font.Font -> LauncherState -> IO LauncherAction
launcherLoop window renderer bgTexture font st = do
    events <- SDL.pollEvents
    st' <- foldM handleEvent st events
    
    renderLauncher window renderer bgTexture font st'
    
    case finalAction st' of
        Just action -> return action
        Nothing     -> do
            SDL.delay 16 
            launcherLoop window renderer bgTexture font st'

runLauncher :: SDL.Window -> SDL.Renderer -> Bool -> IO LauncherAction
runLauncher window renderer initialTutorialState = do
    font <- Font.load "assets/font.ttf" 24
    bgSurface <- IMG.load "assets/fondo-launcher.png" 
    bgTexture <- SDL.createTextureFromSurface renderer bgSurface
    SDL.freeSurface bgSurface

    let initialState = LauncherState 
            { finalAction = Nothing
            , mousePos = (0,0)
            , tutorialMode = initialTutorialState 
            , showNotImpl = False
            }
    
    result <- launcherLoop window renderer bgTexture font initialState
    
    SDL.destroyTexture bgTexture
    Font.free font
    
    return result