{-# LANGUAGE OverloadedStrings #-}
module Objetos.HUD where
-- Módulos del sistema
import qualified SDL
import qualified SDL.Font           as Font
import qualified Data.Word          as DW
import qualified Data.Text          as DT
import qualified Foreign.C.Types    as FCT
import qualified Lens.Micro         as LMi
-- Módulos propios
import qualified Personajes.Types   as PType
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD

dibujarOverlay :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> SDL.V2 Float -> SDL.V4 DW.Word8 -> IO ()
dibujarOverlay renderer texture screenPos size color = do
    GD.dibujarTextura renderer texture GD.screenCenter 1.0 screenPos size 0 color

dibujarHUD :: SDL.Renderer -> Font.Font -> SDL.Texture -> PType.Jugador -> (Int, Int) -> IO ()
dibujarHUD renderer font texture jugador (winW, _) = do
    let vida    = jugador LMi.^. PType.jugEnt . GType.entVid . GType.vidAct
    let velocidadReal = jugador LMi.^. PType.jugEnt . GType.entMov . GType.movAct
    let (SDL.V2 pX pY) = jugador LMi.^. PType.jugEnt . GType.entBox . GType.boxPos

    dibujarOverlay renderer texture (SDL.V2 10 10) (SDL.V2 200 20) (SDL.V4 200 127 127 255)
    let vidaClamp = max 0 (min 100 vida)
    let anchoVida = (vidaClamp / 100.0) * 200 
    dibujarOverlay renderer texture (SDL.V2 10 10) (SDL.V2 anchoVida 20) (SDL.V4 200 0 0 255)

    let colorTexto = SDL.V4 255 255 255 255
    let txtVida = "HP: " <> DT.pack (show (round vida :: Int))
    GD.dibujarTexto renderer font txtVida (SDL.V2 15 6) colorTexto 
    let velRedondeada = (fromIntegral (round (velocidadReal * 100) :: Int) / 100) :: Float
    let txtVel = "Vel: " <> DT.pack (show velRedondeada)
    GD.dibujarTexto renderer font txtVel (SDL.V2 10 40) colorTexto

    let txtCoords = "Pos: " <> DT.pack (show (round pX :: Int)) <> ", " <> DT.pack (show (round pY :: Int))
    let marginX = 160 
    let coordXDisplay = fromIntegral winW - marginX :: Int
    let posTextoCoords = SDL.V2 (fromIntegral coordXDisplay :: FCT.CInt) 10 
    GD.dibujarTexto renderer font txtCoords posTextoCoords colorTexto

dibujarBuffs :: SDL.Renderer -> Font.Font -> SDL.Texture -> [GType.Buff] -> IO ()
dibujarBuffs renderer font texture buffs = do
    let startX = 10 :: FCT.CInt
    let startY = 70 :: FCT.CInt
    let colorTexto = SDL.V4 255 255 255 255
    
    let loopDraw [] _ = return ()
        loopDraw (b:bs) y = do
            let posIcono = SDL.V2 (fromIntegral startX) (fromIntegral y)
            let bID = b LMi.^. GType.bufID
            let colorIcono = case bID of
                    1 -> SDL.V4 0 255 255 255
                    2 -> SDL.V4 255 100 100 255
                    _ -> SDL.V4 255 255 255 255

            dibujarOverlay renderer texture posIcono (SDL.V2 20 20) colorIcono
            let tiempoVal = b LMi.^. GType.bufTmp
            let tiempoStr = DT.pack (show ((fromIntegral (round (tiempoVal * 10) :: Int) / 10.0) :: Float))
            let nombre = b LMi.^. GType.bufNom 
            let texto  = nombre <> ": " <> tiempoStr <> "s"
            GD.dibujarTexto renderer font texto (SDL.V2 (startX + 25) y) colorTexto
            
            loopDraw bs (y + 30)

    loopDraw buffs startY

dibujarMuerte :: SDL.Renderer -> Font.Font -> SDL.Texture -> Int -> IO ()
dibujarMuerte renderer font texture muertes = do
    let bgPos  = SDL.V2 200 200
    let bgSize = SDL.V2 400 200
    let bgColor = SDL.V4 20 20 20 150
    
    dibujarOverlay renderer texture bgPos bgSize bgColor
    let msgMuertes = "Muertes: " <> DT.pack (show muertes)

    GD.dibujarTextoCentrado renderer font "HAS MUERTO"      (SDL.V2 400 250) (SDL.V4 255 0 0 255)
    GD.dibujarTextoCentrado renderer font "Reaparecer 'Y'"  (SDL.V2 400 300) (SDL.V4 255 255 255 255)
    GD.dibujarTextoCentrado renderer font msgMuertes        (SDL.V2 400 350) (SDL.V4 200 200 200 255)