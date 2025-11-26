module Mapas.Mapa where

import Linear.V2 (V2(..))
import Types (Obstaculo(..))

-- Mapa de prueba: Unas paredes rodeando y un bloque en el medio
cargarMapa :: [Obstaculo]
cargarMapa = 
    [ Obstaculo (V2 0 0)     (V2 800 20)  -- Techo
    , Obstaculo (V2 0 580)   (V2 800 20)  -- Suelo
    , Obstaculo (V2 0 0)     (V2 20 600)  -- Pared Izq
    , Obstaculo (V2 780 0)   (V2 20 600)  -- Pared Der
    , Obstaculo (V2 300 200) (V2 100 100) -- Bloque central
    ]