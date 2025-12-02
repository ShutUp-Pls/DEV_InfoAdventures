{-# LANGUAGE OverloadedStrings #-}
module Graficos.HUD where
-- Módulos del sistema
import qualified SDL
import qualified SDL.Font           as Font
import qualified Data.Text          as DT
import qualified Data.List          as DL
import qualified Data.Function      as DF
import qualified Foreign.C.Types    as FCT
import qualified Lens.Micro         as LMi
import qualified Text.Printf        as TP
-- Módulos propios
import qualified Personajes.Types   as PType
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD
import qualified Objetos.Arma       as OArma
import qualified Objetos.Buff       as OBuff
import qualified Types

dibujarFondosHUD :: SDL.Renderer -> SDL.Texture -> (Int, Int) -> IO ()
dibujarFondosHUD renderer texture (winW, _) = do
    let colorFondo = SDL.V4 20 20 20 200

    let fondoIzqPos  = SDL.V2 5 5              :: SDL.V2 Float
    let fondoIzqSize = SDL.V2 220 50           :: SDL.V2 Float
    GD.dibujarOverlay renderer texture fondoIzqPos fondoIzqSize colorFondo

    let anchoDer     = 220                     :: Float
    let fondoDerPosX = fromIntegral winW - anchoDer
    let fondoDerPos  = SDL.V2 fondoDerPosX 5
    let fondoDerSize = SDL.V2 anchoDer 90
    GD.dibujarOverlay renderer texture fondoDerPos fondoDerSize colorFondo

dibujarHUD :: SDL.Renderer -> Font.Font -> SDL.Texture -> PType.Jugador -> Float -> (Int, Int) -> IO ()
dibujarHUD renderer font texture jugador tiempoRestante (winW, _) = do
    let vida    = jugador LMi.^. PType.jugEnt . GType.entVid . GType.vidAct
    let velocidadReal = jugador LMi.^. PType.jugEnt . GType.entMov . GType.movAct
    let (SDL.V2 pX pY) = jugador LMi.^. PType.jugEnt . GType.entBox . GType.boxPos

    GD.dibujarOverlay renderer texture (SDL.V2 10 10) (SDL.V2 200 20) (SDL.V4 200 127 127 255)
    let vidaClamp = max 0 (min 100 vida)
    let anchoVida = (vidaClamp / 100.0) * 200 
    GD.dibujarOverlay renderer texture (SDL.V2 10 10) (SDL.V2 anchoVida 20) (SDL.V4 200 0 0 255)

    let colorTexto = SDL.V4 255 255 255 255
    let txtVida = "HP: " <> DT.pack (show (round vida :: Int))
    GD.dibujarTexto renderer font txtVida (SDL.V2 15 6) colorTexto 

    let txtCoords = "Pos: " <> DT.pack (show (round pX :: Int)) <> ", " <> DT.pack (show (round pY :: Int))
    let marginX = 210
    let coordXDisplay = fromIntegral winW - marginX :: Int
    let posTextoCoords = SDL.V2 (fromIntegral coordXDisplay :: FCT.CInt) 5 
    let posTextoVel = SDL.V2 (fromIntegral coordXDisplay :: FCT.CInt) 35
    let posTextoTmp = SDL.V2 (fromIntegral coordXDisplay :: FCT.CInt) 65
    GD.dibujarTexto renderer font txtCoords posTextoCoords colorTexto
    let velRedondeada = (fromIntegral (round (velocidadReal * 100) :: Int) / 100) :: Float
    let txtVel = "Vel: " <> DT.pack (show velRedondeada)
    GD.dibujarTexto renderer font txtVel posTextoVel colorTexto
    let tiempoInt = round tiempoRestante :: Int
    let txtTiempo = "TIEMPO: " <> DT.pack (show tiempoInt)
    let colorTiempo = if tiempoInt < 10 then SDL.V4 255 0 0 255 else SDL.V4 0 255 0 255
    GD.dibujarTexto renderer font txtTiempo posTextoTmp colorTiempo

dibujarBuffs :: SDL.Renderer -> Font.Font -> SDL.Texture -> [GType.Buff] -> Int -> IO ()
dibujarBuffs renderer font texture buffs winW = loopDraw buffs startY
  where
    iconSize = 40
    padding  = 5
    centerX  = fromIntegral winW / 2
    startY   = 10 :: FCT.CInt 
    
    colorTexto = SDL.V4 255 255 255 255

    loopDraw [] _ = return ()
    loopDraw (b:bs) y = do
        let bID = b LMi.^. GType.bufID
        let bloqueX = centerX - 160
        
        let posIcono = SDL.V2 bloqueX (fromIntegral y)
        let sizeIconoV = SDL.V2 (fromIntegral iconSize) (fromIntegral iconSize)

        OBuff.dibujarIcono renderer texture posIcono sizeIconoV bID

        let tiempoVal = b LMi.^. GType.bufTmp
        let tiempoStr = DT.pack (show ((fromIntegral (round (tiempoVal * 10) :: Int) / 10.0) :: Float))
        let nombre = b LMi.^. GType.bufNom 

        let texto  = nombre <> ": " <> tiempoStr <> "s"
        GD.dibujarTexto renderer font texto (SDL.V2 (round (bloqueX + fromIntegral iconSize + 10)) (y + 5)) colorTexto

        loopDraw bs (y + iconSize + padding)

dibujarMuerte :: SDL.Renderer -> Font.Font -> SDL.Texture -> Float -> IO ()
dibujarMuerte renderer font texture tiempoTotalSobrevivido = do
    let bgPos  = SDL.V2 200 200
    let bgSize = SDL.V2 400 200
    let bgColor = SDL.V4 20 20 20 200
    
    GD.dibujarOverlay renderer texture bgPos bgSize bgColor

    let mensajePrincipal = "GAME OVER"

    let tiempoStr = DT.pack $ TP.printf "%.2f s" tiempoTotalSobrevivido
    let msgTiempo = "Sobreviviste: " <> tiempoStr

    GD.dibujarTextoCentrado renderer font mensajePrincipal  (SDL.V2 400 250) (SDL.V4 255 0 0 255)
    GD.dibujarTextoCentrado renderer font msgTiempo         (SDL.V2 400 300) (SDL.V4 255 255 0 255)
    GD.dibujarTextoCentrado renderer font "Presiona 'X' para Salir" (SDL.V2 400 350) (SDL.V4 200 200 200 255)

dibujarHUDAtasco :: SDL.Renderer -> Font.Font -> SDL.Texture -> PType.Jugador -> IO ()
dibujarHUDAtasco renderer font texture jugador = do
    let maybeArma = jugador LMi.^? PType.jugEnt . GType.entHnd . GType.iteTipo . GType._EsArma
    
    case maybeArma of
        Nothing -> return ()
        Just arma -> do
            let heat      = arma LMi.^. GType.eaHeat
            let jammed    = arma LMi.^. GType.eaJammed
            let maxH      = arma LMi.^. GType.eaMaxHeat
            
            let posX = 10.0
            let posY = 35.0
            let anchoTotal = 200.0
            let altoBarra  = 10.0 

            let porcentaje = if maxH > 0 then heat / maxH else 0
            let anchoActual = anchoTotal * porcentaje

            let colorFondo = SDL.V4 50 50 50 200
            let colorBarra = if jammed 
                             then SDL.V4 255 0 0 255   
                             else SDL.V4 0 255 255 255 

            GD.dibujarOverlay renderer texture 
                              (SDL.V2 posX posY) 
                              (SDL.V2 anchoTotal altoBarra) 
                              colorFondo

            GD.dibujarOverlay renderer texture 
                              (SDL.V2 posX posY) 
                              (SDL.V2 anchoActual altoBarra) 
                              colorBarra

            if jammed then do
                let texto = "! RECALENTADA !"
                let textPos = SDL.V2 (round (posX)) (round (posY + 10))
                GD.dibujarTexto renderer font texto textPos (SDL.V4 255 100 100 255)
            else return ()

dibujarInventario :: SDL.Renderer -> Font.Font -> SDL.Texture -> PType.Jugador -> IO ()
dibujarInventario renderer font texture jugador = loopSlots (0 :: Int)
  where
    mano = jugador LMi.^. PType.jugEnt . GType.entHnd
    inv  = jugador LMi.^. PType.jugEnt . GType.entInv

    allItems = filter (\i -> (i LMi.^. GType.iteId) /= 0) (mano : inv)
    listaOrdenada = DL.sortBy (compare `DF.on` (\i -> i LMi.^. GType.iteBox . GType.boxPos . SDL._x)) allItems
    
    idEnMano = mano LMi.^. GType.iteId

    startPos = SDL.V2 20 70
    boxSize  = SDL.V2 50 50
    margin   = 10
    
    maxSlots = 3
    posTextoFijo = fmap round (startPos + SDL.V2 0 55)

    safeGet _ [] = Nothing
    safeGet 0 (x:_) = Just x
    safeGet n (_:xs) = safeGet (n-1) xs

    loopSlots i = if i >= maxSlots then return () else do
            let offset    = fromIntegral i * (60 + margin)
            let posActual = startPos + SDL.V2 offset 0
            
            let maybeItem = safeGet i listaOrdenada
            
            case maybeItem of
                Nothing -> do
                    GD.dibujarOverlay renderer texture posActual boxSize (SDL.V4 30 30 30 100)
                
                Just arma -> do
                    let armaId = arma LMi.^. GType.iteId
                    let esEquipada = armaId == idEnMano
                    let colorFondo = if esEquipada 
                                     then SDL.V4 0 180 0 180
                                     else SDL.V4 60 60 60 180

                    GD.dibujarOverlay renderer texture posActual boxSize colorFondo
                    
                    case arma LMi.^. GType.iteTipo of
                         GType.EsArma armData -> do
                             let realArmId = armData LMi.^. GType.armID
                             OArma.dibujarIcono renderer texture posActual boxSize realArmId
                         _ -> return ()
                    
                    if esEquipada then do
                        let nombre = arma LMi.^. GType.iteNom
                        GD.dibujarTexto renderer font nombre posTextoFijo (SDL.V4 255 255 255 255)
                    else return ()

            loopSlots (i + 1)

dibujarPromptEspacio :: SDL.Renderer -> Font.Font -> SDL.Texture -> (Int, Int) -> IO ()
dibujarPromptEspacio renderer font texture (_, h) = do
    let msg = "Espacio para continuar..."
    let (tw, th) = (300, 30) :: (Int, Int)
    let margin = 20
    let x = margin
    let y = h - margin - th
    
    let bgPos  = SDL.V2 (fromIntegral x - 5) (fromIntegral y - 5)       :: SDL.V2 Float
    let bgSize = SDL.V2 (fromIntegral tw + 10) (fromIntegral th + 10)   :: SDL.V2 Float
    GD.dibujarOverlay renderer texture bgPos bgSize (SDL.V4 0 0 0 200)

    let txtPos = SDL.V2 (fromIntegral x) (fromIntegral y)
    GD.dibujarTexto renderer font msg txtPos (SDL.V4 200 200 200 255)

dibujarTutorialOverlay :: SDL.Renderer -> Font.Font -> SDL.Texture -> Types.TutorialFase -> (Int, Int) -> IO ()
dibujarTutorialOverlay renderer font texture fase (w, h) = do
    let wf = fromIntegral w :: Float
    let hf = fromIntegral h :: Float
    let center = SDL.V2 (div (fromIntegral w) 2) (div (fromIntegral h) 2)

    case fase of
        Types.FaseIntro -> do
            GD.dibujarOverlay renderer texture (SDL.V2 0 0) (SDL.V2 wf hf) (SDL.V4 0 0 0 50)
            
            let msg = "El juego termina cuando PIERDES toda la VIDA"
            let msg2 = "o cuando el TIEMPO llega a 0."
            GD.dibujarTextoCentrado renderer font msg center (SDL.V4 255 255 255 255)
            GD.dibujarTextoCentrado renderer font msg2 (center + SDL.V2 0 30) (SDL.V4 255 255 255 255)

            GD.dibujarTexto renderer font "<- VIDA" (SDL.V2 220 5) (SDL.V4 255 0 0 255)
            GD.dibujarTexto renderer font "<- ENFRIAMIENTO" (SDL.V2 220 25) (SDL.V4 0 255 255 255)
            GD.dibujarTexto renderer font "TIEMPO ->" (SDL.V2 (fromIntegral w - 350) 68) (SDL.V4 0 255 0 255)
            GD.dibujarTexto renderer font "INVENTARIO" (SDL.V2 40 120) (SDL.V4 255 255 0 255)

            dibujarPromptEspacio renderer font texture (w, h)

        Types.FaseArmas -> do
            let boxPos = SDL.V2 10 (hf - 160)
            let boxSize = SDL.V2 400 100
            GD.dibujarOverlay renderer texture boxPos boxSize (SDL.V4 0 0 0 180)
            
            GD.dibujarTexto renderer font "'J' para disparar" (fmap round (boxPos + SDL.V2 20 5)) (SDL.V4 255 255 255 255)
            GD.dibujarTexto renderer font "'K' y 'L' para cambiar de arma" (fmap round (boxPos + SDL.V2 20 35)) (SDL.V4 255 255 255 255)
            GD.dibujarTexto renderer font "Cuidado: ¡Se sobrecalientan!" (fmap round (boxPos + SDL.V2 20 65)) (SDL.V4 255 100 100 255)

            dibujarPromptEspacio renderer font texture (w, h)

        Types.FaseCamara -> do
            let boxPos = SDL.V2 (wf - 300) 100
            let boxSize = SDL.V2 300 80
            GD.dibujarOverlay renderer texture boxPos boxSize (SDL.V4 0 0 0 180)
            
            GD.dibujarTexto renderer font "'-' y '+' para ZOOM" (fmap round (boxPos + SDL.V2 10 10)) (SDL.V4 255 255 255 255)
            GD.dibujarTexto renderer font "'O' y 'P' para DEADZONE" (fmap round (boxPos + SDL.V2 10 40)) (SDL.V4 255 255 255 255)

            dibujarPromptEspacio renderer font texture (w, h)

        Types.FaseZombieMsg -> do
            let boxPos = SDL.V2 (wf - 300) 100
            let boxSize = SDL.V2 300 50
            GD.dibujarOverlay renderer texture boxPos boxSize (SDL.V4 50 0 0 200)
            GD.dibujarTexto renderer font "¡Cuidado un ZOMBIE!" (fmap round (boxPos + SDL.V2 20 10)) (SDL.V4 255 255 0 255)

        Types.FaseBuffsMsg -> do
            let boxPos = SDL.V2 (wf - 400) 100
            let boxSize = SDL.V2 400 60
            GD.dibujarOverlay renderer texture boxPos boxSize (SDL.V4 0 0 0 180)
            GD.dibujarTexto renderer font "Hay distintos ITEMS" (fmap round (boxPos + SDL.V2 10 5)) (SDL.V4 255 255 255 255)
            GD.dibujarTexto renderer font "Recogelos para adquirir efectos" (fmap round (boxPos + SDL.V2 10 30)) (SDL.V4 200 200 255 255)

            dibujarPromptEspacio renderer font texture (w, h)

        Types.FaseTiempoMsg -> do
            let boxPos = SDL.V2 (wf - 480) 100
            let boxSize = SDL.V2 480 80
            GD.dibujarOverlay renderer texture boxPos boxSize (SDL.V4 0 0 0 180)
            GD.dibujarTexto renderer font "Recoje los relojes para adquirir tiempo" (fmap round (boxPos + SDL.V2 10 10)) (SDL.V4 255 255 255 255)
            GD.dibujarTexto renderer font "¡Tu PUNTUACION es tu TIEMPO!" (fmap round (boxPos + SDL.V2 10 40)) (SDL.V4 255 255 0 255)

            dibujarPromptEspacio renderer font texture (w, h)

        Types.FaseSobrevive -> do
            GD.dibujarOverlay renderer texture (SDL.V2 0 0) (SDL.V2 wf hf) (SDL.V4 0 0 0 150)
            GD.dibujarTextoCentrado renderer font "¡SOBREVIVE!" center (SDL.V4 255 0 0 255)
            GD.dibujarTextoCentrado renderer font "Si puedes..." (center + SDL.V2 0 50) (SDL.V4 200 200 200 255)

        _ -> return ()