module Fisica.SAT where
-- Módulos de sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as DO
-- Módulos propios
import qualified Fisica.Angulos as FA

-- Rota un vector (x,y) por un ángulo en radianes
rotateV2 :: Float -> SDL.V2 Float -> SDL.V2 Float
rotateV2 rad (SDL.V2 x y) = 
    SDL.V2 (x * cos rad - y * sin rad) (x * sin rad + y * cos rad)

-- Obtiene las 4 esquinas de un rectángulo rotado en el mundo
getCorners :: SDL.V2 Float -> SDL.V2 Float -> Float -> [SDL.V2 Float]
getCorners pos size angleDeg = 
    let 
        center = pos + (size LV.^* 0.5)
        rads   = FA.degToRad angleDeg
        (SDL.V2 hwX hwY) = size LV.^* 0.5

        cornersRel = [ SDL.V2 (-hwX) (-hwY) -- Arriba-Izquierda
                     , SDL.V2 ( hwX) (-hwY) -- Arriba-Derecha
                     , SDL.V2 ( hwX) ( hwY) -- Abajo-Derecha
                     , SDL.V2 (-hwX) ( hwY) -- Abajo-Izquierda
                     ]
    in 
        map (\c -> center + rotateV2 rads c) cornersRel

-- El SAT requiere probar los ejes perpendiculares a cada lado del polígono
getAxes :: [SDL.V2 Float] -> [SDL.V2 Float]
getAxes corners = 
    [ LM.normalize (SDL.V2 (-(y2-y1)) (x2-x1)) 
    | (SDL.V2 x1 y1, SDL.V2 x2 y2) <- zip corners (tail corners ++ [head corners])
    ]

-- Proyecta una figura (lista de esquinas) sobre un eje para obtener su "sombra" (min, max)
project :: SDL.V2 Float -> [SDL.V2 Float] -> (Float, Float)
project axis corners = 
    let dots = map (LM.dot axis) corners
    in (minimum dots, maximum dots)

-- Verifica si dos sombras se solapan en un eje específico y cuánto
overlapAmount :: (Float, Float) -> (Float, Float) -> Maybe Float
overlapAmount (minA, maxA) (minB, maxB) = 
    if maxA < minB || maxB < minA
    then Nothing
    else Just $ min (maxA - minB) (maxB - minA)

-- Teorema del Eje de Separación con Resolución por Vector de Traslación Mínima (MTV)
satCollision :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> Maybe (SDL.V2 Float)
satCollision p1 s1 a1 p2 s2 a2 = 
    let 
        c1 = getCorners p1 s1 a1
        c2 = getCorners p2 s2 a2
        axes = getAxes c1 ++ getAxes c2
        overlaps = map (\axis -> 
            case overlapAmount (project axis c1) (project axis c2) of
                Nothing  -> Nothing
                Just amt -> Just (axis, amt)
            ) axes
    in 
    if any (== Nothing) overlaps 
        then Nothing 
        else 
            let 
                validOverlaps = DM.catMaybes overlaps
                (axis, depth) = DL.minimumBy (DO.comparing snd) validOverlaps

                center1 = p1 + (s1 LV.^* 0.5)
                center2 = p2 + (s2 LV.^* 0.5)
                dir = center1 - center2
                normal = if LM.dot dir axis < 0 
                         then axis LV.^* (-1.0)
                         else axis
            in 
                Just (normal LV.^* depth)