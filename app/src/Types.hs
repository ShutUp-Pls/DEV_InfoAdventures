module Types where

import Linear.V2 (V2)
-- import Foreign.C.Types (CInt)

-- Definimos la posición como un vector 2D de enteros de C (para SDL)
type Position = V2 Float

-- Datos del Jugador
data Jugador = Jugador
    { posJugador :: Position
    , velocidad  :: Float
    } deriving (Show, Eq)

-- Estado Global del Juego
data GameState = GameState
    { jugador :: Jugador
    , mapa    :: String
    } deriving (Show, Eq)

-- Inputs soportados
data Input = Input
    { up    :: Bool
    , down  :: Bool
    , left  :: Bool
    , right :: Bool
    , shift :: Bool
    } deriving (Show, Eq)