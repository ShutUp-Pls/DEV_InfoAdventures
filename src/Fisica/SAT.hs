module Fisica.SAT where

-- Módulos de sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Data.Maybe as DM
import qualified Data.List as DL
import qualified Data.Ord as DO

-- ============================================================================
-- CONSTANTES MATEMÁTICAS
-- ============================================================================

factorGradosARadianes :: Float
factorGradosARadianes = pi / 180.0

factorMitad :: Float
factorMitad = 0.5

inversorVector :: Float
inversorVector = -1.0

-- ============================================================================
-- HELPERS DE GEOMETRÍA
-- ============================================================================

-- Convierte grados a radianes
degToRad :: Float -> Float
degToRad d = d * factorGradosARadianes

-- Rota un vector (x,y) por un ángulo en radianes
rotateV2 :: Float -> SDL.V2 Float -> SDL.V2 Float
rotateV2 rad (SDL.V2 x y) = 
    SDL.V2 (x * cos rad - y * sin rad) (x * sin rad + y * cos rad)

-- Obtiene las 4 esquinas de un rectángulo rotado en el mundo
getCorners :: SDL.V2 Float -> SDL.V2 Float -> Float -> [SDL.V2 Float]
getCorners pos size angleDeg = 
    let 
        -- Calculamos el centro usando el factor mitad (0.5)
        center = pos + (size LV.^* factorMitad)
        rads   = degToRad angleDeg
        
        -- Distancia del centro a los bordes (mitad del tamaño)
        (SDL.V2 hwX hwY) = size LV.^* factorMitad

        -- Las 4 esquinas relativas al centro (sin rotar)
        cornersRel = [ SDL.V2 (-hwX) (-hwY) -- Arriba-Izquierda
                     , SDL.V2 ( hwX) (-hwY) -- Arriba-Derecha
                     , SDL.V2 ( hwX) ( hwY) -- Abajo-Derecha
                     , SDL.V2 (-hwX) ( hwY) -- Abajo-Izquierda
                     ]
    in 
        -- Rotamos cada esquina relativa y le sumamos la posición del centro
        map (\c -> center + rotateV2 rads c) cornersRel

-- Obtiene los ejes normales (perpendiculares) para el SAT
-- El SAT requiere probar los ejes perpendiculares a cada lado del polígono
getAxes :: [SDL.V2 Float] -> [SDL.V2 Float]
getAxes corners = 
    [ LM.normalize (SDL.V2 (-(y2-y1)) (x2-x1)) 
    | (SDL.V2 x1 y1, SDL.V2 x2 y2) <- zip corners (tail corners ++ [head corners])
    ]

-- ============================================================================
-- LÓGICA CORE SAT (Separating Axis Theorem)
-- ============================================================================

-- Proyecta una figura (lista de esquinas) sobre un eje para obtener su "sombra" (min, max)
project :: SDL.V2 Float -> [SDL.V2 Float] -> (Float, Float)
project axis corners = 
    let dots = map (LM.dot axis) corners
    in (minimum dots, maximum dots)

-- Verifica si dos sombras se solapan en un eje específico y cuánto
overlapAmount :: (Float, Float) -> (Float, Float) -> Maybe Float
overlapAmount (minA, maxA) (minB, maxB) = 
    if maxA < minB || maxB < minA
    then Nothing -- Hay un hueco, no hay colisión
    else Just $ min (maxA - minB) (maxB - minA) -- Devuelve la profundidad del solapamiento

-- Teorema del Eje de Separación con Resolución por Vector de Traslación Mínima (MTV)
satCollision :: SDL.V2 Float -> SDL.V2 Float -> Float -> SDL.V2 Float -> SDL.V2 Float -> Float -> Maybe (SDL.V2 Float)
satCollision p1 s1 a1 p2 s2 a2 = 
    let 
        c1 = getCorners p1 s1 a1
        c2 = getCorners p2 s2 a2
        
        -- Probamos todos los ejes únicos de ambas figuras
        axes = getAxes c1 ++ getAxes c2
        
        -- Mapeamos cada eje a su cantidad de solapamiento (o Nothing si hay hueco)
        overlaps = map (\axis -> 
            case overlapAmount (project axis c1) (project axis c2) of
                Nothing  -> Nothing
                Just amt -> Just (axis, amt)
            ) axes
    in 
    -- Si encontramos AL MENOS UN eje donde no hay solapamiento (Nothing), 
    -- entonces las figuras están separadas (Teorema SAT).
    if any (== Nothing) overlaps 
        then Nothing 
        else 
            let 
                -- Si hay colisión en todos los ejes, buscamos la "penetración mínima"
                validOverlaps = DM.catMaybes overlaps
                (axis, depth) = DL.minimumBy (DO.comparing snd) validOverlaps
                
                -- Dirección del centro de P2 al centro de P1
                dir = p1 - p2
                
                -- Nos aseguramos que el vector normal apunte hacia afuera (hacia P1)
                -- Si apunta al revés, lo invertimos.
                normal = if LM.dot dir axis < 0 
                         then axis LV.^* inversorVector 
                         else axis
            in 
                -- Devolvemos el vector de corrección (MTV)
                Just (normal LV.^* depth)