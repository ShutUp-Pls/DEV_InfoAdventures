module Fisica.SAT where

import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Convierte grados a radianes
degToRad :: Float -> Float
degToRad d = d * (pi / 180)

-- Rota un vector (x,y) por un ángulo en radianes
rotateV2 :: Float -> SDL.V2 Float -> SDL.V2 Float
rotateV2 rad (SDL.V2 x y) = 
    SDL.V2 (x * cos rad - y * sin rad) (x * sin rad + y * cos rad)

-- Obtiene las 4 esquinas de un rectángulo rotado en el mundo
getCorners :: SDL.V2 Float -> SDL.V2 Float -> Float -> [SDL.V2 Float]
getCorners pos size angleDeg = 
    let center = pos + (size ^* 0.5)
        rads   = degToRad angleDeg
        halfW  = size ^* 0.5
        (SDL.V2 hwX hwY) = halfW

        -- Las 4 esquinas relativas al centro (sin rotar)
        cornersRel = [ SDL.V2 (-hwX) (-hwY)
                     , SDL.V2 ( hwX) (-hwY)
                     , SDL.V2 ( hwX) ( hwY)
                     , SDL.V2 (-hwX) ( hwY)
                     ]
    in map (\c -> center + rotateV2 rads c) cornersRel
  where 
    (^*) = (LV.^*)

-- Obtiene los ejes normales (perpendiculares) para el SAT
getAxes :: [SDL.V2 Float] -> [SDL.V2 Float]
getAxes corners = 
    [ LM.normalize (SDL.V2 (-(y2-y1)) (x2-x1)) 
    | (SDL.V2 x1 y1, SDL.V2 x2 y2) <- zip corners (tail corners ++ [head corners])
    ]

-- Proyecta una figura (lista de esquinas) sobre un eje
project :: SDL.V2 Float -> [SDL.V2 Float] -> (Float, Float)
project axis corners = 
    let dots = map (LM.dot axis) corners
    in (minimum dots, maximum dots)

-- Verifica solapamiento en un eje específico
overlap :: (Float, Float) -> (Float, Float) -> Bool
overlap (minA, maxA) (minB, maxB) = 
    not (maxA < minB || maxB < minA)

-- Solapamiento SAT
satCollision :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> Bool
satCollision p1 s1 a1 p2 s2 a2 = 
    let c1 = getCorners p1 s1 a1
        c2 = getCorners p2 s2 a2
        axes = getAxes c1 ++ getAxes c2
    in all (\axis -> overlap (project axis c1) (project axis c2)) axes