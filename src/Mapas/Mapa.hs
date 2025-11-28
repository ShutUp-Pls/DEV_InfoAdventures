module Mapas.Mapa where

-- Modulos del sistema
import qualified SDL

-- Modulos propios
import qualified Types

-- Mapa de prueba: Unas paredes rodeando y un bloque en el medio
mapaBox :: [Types.Obstaculo]
mapaBox = 
    [ Types.Obstaculo (SDL.V2 0 0)     (SDL.V2 800 20)  0   -- Techo
    , Types.Obstaculo (SDL.V2 0 580)   (SDL.V2 800 20)  0   -- Suelo
    , Types.Obstaculo (SDL.V2 0 0)     (SDL.V2 20 600)  0   -- Pared Izq
    , Types.Obstaculo (SDL.V2 780 0)   (SDL.V2 20 600)  0   -- Pared Der
    , Types.Obstaculo (SDL.V2 300 200) (SDL.V2 100 100) 70   -- Bloque central
    ]