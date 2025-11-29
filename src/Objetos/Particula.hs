module Objetos.Particula  where

import qualified SDL
import qualified System.Random  as SR
import qualified Linear.Vector  as LV
import qualified Data.Word      as DW
import qualified Lens.Micro     as LMi

import qualified Objetos.Types      as OType
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD

-- Rango de velocidad base
rangoVelocidad :: (Float, Float)
rangoVelocidad = (150.0, 300.0)
-- Rango de vida de la partícula en segundos
rangoVida :: (Float, Float)
rangoVida = (0.5, 1.5)
-- Rango de tamaño de la partícula en píxeles
rangoTamano :: (Float, Float)
rangoTamano = (2.0, 8.0)
colorParticula :: SDL.V4 DW.Word8
colorParticula = SDL.V4 255 255 255 255

crearExplosionLineal :: SDL.V2 Float -> Int -> Float -> SR.StdGen -> ([OType.Particula], SR.StdGen)
crearExplosionLineal pos cantidad mult rng = 
    generarBase pos cantidad mult OType.MovimientoLineal rng

crearExplosionGradualDown :: SDL.V2 Float -> Int -> Float -> SR.StdGen -> ([OType.Particula], SR.StdGen)
crearExplosionGradualDown pos cantidad mult rng = 
    generarBase pos cantidad mult OType.MovimientoGradualDown rng

generarBase :: SDL.V2 Float -> Int -> Float -> OType.ComportamientoParticula -> SR.StdGen -> ([OType.Particula], SR.StdGen)
generarBase pos cantidad multiplicadorVel comportamiento rng = foldr generar ([], rng) [1..cantidad]
  where
    generar _ (particulas, currentRng) =
        let (angulo, rng1) = SR.uniformR (0.0, 2 * pi) currentRng :: (Float, SR.StdGen)
            (velocidadBase, rng2) = SR.uniformR rangoVelocidad rng1
            velocidadFinal = velocidadBase * multiplicadorVel
            (vida, rng3) = SR.uniformR rangoVida rng2 :: (Float, SR.StdGen)
            (size, rng4) = SR.uniformR rangoTamano rng3 :: (Float, SR.StdGen)
            
            nuevaParticula = OType.Particula
                { OType._parBox = GType.Box 
                    { GType._boxPos = pos
                    , GType._boxTam = SDL.V2 size size
                    , GType._boxAng = angulo
                    , GType._boxRad = size / 2
                    }
                , OType._parVel = GType.Movimiento
                    { GType._movVel = velocidadFinal
                    , GType._movRot = 0
                    , GType._movFac = 1.0
                    , GType._movAct = 0.0
                    }
                , OType._parVid = GType.Vida
                    { GType._vidAct = vida
                    , GType._vidMax = vida
                    , GType._vidMrt = 0
                    }
                , OType._parTip = comportamiento
                }
            
        in (nuevaParticula : particulas, rng4)

actualizarParticulas :: Float -> [OType.Particula] -> [OType.Particula]
actualizarParticulas dt particulas = 
    filter (\p -> (p LMi.^. OType.parVid . GType.vidAct) > 0) $ map (actualizarParticula dt) particulas

actualizarParticula :: Float -> OType.Particula -> OType.Particula
actualizarParticula dt p = case p LMi.^. OType.parTip of
    OType.MovimientoLineal -> 
        let 
            pos   = p LMi.^. OType.parBox . GType.boxPos
            angle = p LMi.^. OType.parBox . GType.boxAng
            speed = p LMi.^. OType.parVel . GType.movVel
            dir   = SDL.V2 (cos angle) (sin angle)
            desp  = dir LV.^* (speed * dt)
        in p 
            LMi.& OType.parBox . GType.boxPos LMi..~ (pos + desp)
            LMi.& OType.parVid . GType.vidAct LMi.%~ (\v -> v - dt)
    OType.MovimientoGradualDown ->
        let 
            vidaAct = p LMi.^. OType.parVid . GType.vidAct
            vidaNew = vidaAct - dt

            pos   = p LMi.^. OType.parBox . GType.boxPos
            angle = p LMi.^. OType.parBox . GType.boxAng
            speed = p LMi.^. OType.parVel . GType.movVel

            dir   = SDL.V2 (cos angle) (sin angle)
            desp  = dir LV.^* (speed * dt)
            
        in p 
            LMi.& OType.parBox . GType.boxPos LMi..~ (pos + desp)
            LMi.& OType.parVid . GType.vidAct LMi..~ vidaNew
            LMi.& OType.parVel . GType.movVel LMi.%~ (* 0.90)
            LMi.& OType.parBox . GType.boxTam LMi.%~ (LV.^* 0.95)

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> OType.Particula -> IO ()
dibujar renderer texture camPos zoom particula = do
    let pos = particula LMi.^. OType.parBox . GType.boxPos
    let tam = particula LMi.^. OType.parBox . GType.boxTam
    
    GD.dibujarTextura renderer texture camPos zoom pos tam 0.0 colorParticula