{-# LANGUAGE OverloadedStrings #-}
module Objetos.HUD where

import qualified SDL
import qualified SDL.Font as Font
import qualified Data.Word as DW
import qualified Data.Text as DT
import qualified Foreign.C.Types as FCT

import Types
import Graficos.Dibujado as GD

dibujarOverlay :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> SDL.V2 Float -> SDL.V3 DW.Word8 -> IO ()
dibujarOverlay renderer texture screenPos size color = do
    dibujarTextura renderer texture screenCenter 1.0 screenPos size 0 color

dibujarHUD :: SDL.Renderer -> Font.Font -> SDL.Texture -> Float -> Float -> IO ()
dibujarHUD renderer font texture vida velocidad = do
    dibujarOverlay renderer texture (SDL.V2 10 10) (SDL.V2 200 20) (SDL.V3 200 127 127)
    
    let vidaClamp = max 0 (min 100 vida)
    let anchoVida = (vidaClamp / 100.0) * 200 
    dibujarOverlay renderer texture (SDL.V2 10 10) (SDL.V2 anchoVida 20) (SDL.V3 200 0 0)

    let color = SDL.V4 255 255 255 255
    let renderizarTexto texto x y = do
            surface <- Font.solid font color texto
            t <- SDL.createTextureFromSurface renderer surface
            SDL.freeSurface surface
            ti <- SDL.queryTexture t
            let rect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 (SDL.textureWidth ti) (SDL.textureHeight ti))
            SDL.copy renderer t Nothing (Just rect)
            SDL.destroyTexture t

    let txtVida = "HP: " <> DT.pack (show (round vida :: Int))
    renderizarTexto txtVida 15 12 

    let velRedondeada = (fromIntegral (round (velocidad * 100) :: Int) / 100) :: Float
    let txtVel = "Vel: " <> DT.pack (show velRedondeada)
    renderizarTexto txtVel 10 40

-- Buffs visuales
dibujarBuffs :: SDL.Renderer -> Font.Font -> SDL.Texture -> [Types.Buff] -> IO ()
dibujarBuffs renderer font texture buffs = do
    let startX = 10 :: FCT.CInt
    let startY = 70 :: FCT.CInt
    
    let loopDraw [] _ = return ()
        loopDraw (b:bs) y = do
            -- Convertimos a Float para dibujarOverlay
            let posIcono = SDL.V2 (fromIntegral startX) (fromIntegral y)
            
            -- Fondo del buff (Cyan)
            dibujarOverlay renderer texture posIcono (SDL.V2 20 20) (SDL.V3 0 255 255)
            
            -- Texto del buff
            let tiempoStr = DT.pack (show ((fromIntegral (round (Types.buffTiempo b * 10) :: Int) / 10.0) :: Float))
            let texto = Types.buffNombre b <> ": " <> tiempoStr <> "s"
            
            surface <- Font.solid font (SDL.V4 255 255 255 255) texto
            t <- SDL.createTextureFromSurface renderer surface
            SDL.freeSurface surface
            ti <- SDL.queryTexture t
            
            -- Texto desplazado a la derecha del icono (+25 pixeles)
            let rect = SDL.Rectangle (SDL.P (SDL.V2 (startX + 25) y)) (SDL.V2 (SDL.textureWidth ti) (SDL.textureHeight ti))
            SDL.copy renderer t Nothing (Just rect)
            SDL.destroyTexture t
            
            loopDraw bs (y + 30)

    loopDraw buffs startY