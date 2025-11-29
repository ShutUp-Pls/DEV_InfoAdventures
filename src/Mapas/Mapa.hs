module Mapas.Mapa where
-- Modulos del sistema
import qualified SDL
-- Modulos propios
import qualified Globals.Types as GType

-- Mapa de prueba: Unas paredes rodeando y un bloque en el medio
mapaBox :: [GType.Box]
mapaBox = 
    [ GType.Box (SDL.V2 0 0)     (SDL.V2 800 20)  0  0
    , GType.Box (SDL.V2 0 580)   (SDL.V2 800 20)  0  0
    , GType.Box (SDL.V2 0 0)     (SDL.V2 20 600)  0  0
    , GType.Box (SDL.V2 780 0)   (SDL.V2 20 600)  0  0
    , GType.Box (SDL.V2 300 200) (SDL.V2 100 100) 70 0
    ]

mapaTest :: [GType.Box]
mapaTest = limites ++ plataformas ++ obstaculosRotados ++ estructuras
  where
    -- 1. Límites del Mundo (2000 ancho x 1000 alto)
    limites = 
        [ GType.Box (SDL.V2 0 0)      (SDL.V2 2000 20) 0 0   -- Techo
        , GType.Box (SDL.V2 0 980)    (SDL.V2 2000 20) 0 0   -- Suelo
        , GType.Box (SDL.V2 0 0)      (SDL.V2 20 1000) 0 0   -- Pared Izquierda
        , GType.Box (SDL.V2 1980 0)   (SDL.V2 20 1000) 0 0   -- Pared Derecha
        ]

    -- 2. Zona de Saltos (Escalera progresiva)
    plataformas =
        [ GType.Box (SDL.V2 200 850)  (SDL.V2 150 20) 0 0
        , GType.Box (SDL.V2 400 750)  (SDL.V2 150 20) 0 0
        , GType.Box (SDL.V2 600 650)  (SDL.V2 150 20) 0 0
        , GType.Box (SDL.V2 200 550)  (SDL.V2 400 20) 0 0    -- Plataforma larga superior
        ]

    -- 3. Zona de "Caos" (Cajas rotadas para probar físicas)
    obstaculosRotados =
        [ GType.Box (SDL.V2 900 800)  (SDL.V2 100 100) 45 0  -- Diamante (45 grados)
        , GType.Box (SDL.V2 1100 700) (SDL.V2 80 80)   30 0  -- Rotación leve
        , GType.Box (SDL.V2 1250 850) (SDL.V2 100 20)  90 0  -- Muro vertical (rotado)
        , GType.Box (SDL.V2 1000 400) (SDL.V2 300 20)  15 0  -- Rampa inclinada
        ]

    -- 4. Estructuras grandes / Pasillos
    estructuras =
        [ GType.Box (SDL.V2 1400 200) (SDL.V2 50 600)  0 0   -- Columna gigante colgante
        , GType.Box (SDL.V2 1600 600) (SDL.V2 200 20)  0 0   -- Puente final
        , GType.Box (SDL.V2 1650 900) (SDL.V2 50 50)   0 0   -- Bloque pequeño
        ]