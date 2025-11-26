module Types where

import Linear.V2 (V2)

-- Obstaculo de colisión para mapas
data Obstaculo = Obstaculo
    { posObstaculo  :: V2 Float
    , tamObstaculo :: V2 Float
    } deriving (Show, Eq)

-- Pattern Matching para decidir que tipo de Item es un Item
data TipoItem 
    = Vida Float
    | Velocidad Float
    | Puntos Float
    deriving (Show, Eq)

data Item = Item
    { posItem  :: V2 Float
    , tamItem  :: V2 Float
    , tipoItem :: TipoItem
    , activo   :: Bool
    } deriving (Show, Eq)

-- Datos del Jugador
data Jugador = Jugador
    { posJugador  :: V2 Float
    , velJugador  :: Float
    , velCorrerJ  :: Float
    , velCaminarJ :: Float
    , velFactorJ  :: Float
    , vidJugador  :: Float
    , tamJugador  :: V2 Float
    , empujeJ     :: Float
    , velGolpeJ   :: V2 Float
    } deriving (Show, Eq)

-- Datos del Enemigo
data Enemigo = Enemigo
    { posEnemigo   :: V2 Float
    , velEnemigo   :: Float
    , vidEnemigo   :: Float
    , tamEnemigo   :: V2 Float
    , rangoVision  :: Float
    , empujeE      :: Float
    , velGolpeE    :: V2 Float
    } deriving (Show, Eq)

-- Datos de la Camara
data Camara = Camara
    { posCamara    :: V2 Float
    , deadzoneSize :: V2 Float
    } deriving (Show, Eq)

-- Datos del Juego en si
data GameState = GameState
    { jugador :: Jugador
    , items    :: [Item]
    , enemigos :: [Enemigo]
    , mapa    :: [Obstaculo]
    , camara  :: Camara
    } deriving (Show, Eq)

-- Inputs soportados
data Input = Input
    { arriba     :: Bool
    , abajo      :: Bool
    , izquierda  :: Bool
    , derecha    :: Bool
    , shift      :: Bool
    , decreaseDZ :: Bool
    , increaseDZ :: Bool
    } deriving (Show, Eq)