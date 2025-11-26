module Types where

import Linear.V2 (V2)
-- import Foreign.C.Types (CInt)

-- Definimos la posición como un vector 2D de enteros de C (para SDL)
type Position = V2 Float

-- Obstaculo de colisión para mapas
data Obstaculo = Obstaculo
    { posObstaculo  :: Position
    , tamObstaculo :: V2 Float
    } deriving (Show, Eq)

-- Alias para mapas
type Mapa = [Obstaculo]

-- Datos del Jugador
data Jugador = Jugador
    { posJugador :: V2 Float
    , velJugador  :: Float
    , tamJugador :: V2 Float
    } deriving (Show, Eq)

-- Datos del Enemigo
data Enemigo = Enemigo
    { posEnemigo   :: V2 Float
    , velEnemigo   :: Float
    , tamEnemigo   :: V2 Float
    , rangoVision  :: Float
    } deriving (Show, Eq)

-- Estado Global del Juego
data GameState = GameState
    { jugador :: Jugador
    , enemigo      :: Enemigo
    , mapa    :: Mapa
    , camaraPos    :: V2 Float
    , deadzoneSize :: V2 Float
    } deriving (Show, Eq)

-- Inputs soportados
data Input = Input
    { up    :: Bool
    , down  :: Bool
    , left  :: Bool
    , right :: Bool
    , shift :: Bool
    , decreaseDZ :: Bool
    , increaseDZ :: Bool
    } deriving (Show, Eq)