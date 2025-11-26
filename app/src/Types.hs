module Types where
    
-- Módulos del sistema
import qualified SDL
import qualified Data.Text as DT

-- Obstaculo de colisión para mapas
data Obstaculo = Obstaculo
    { posObstaculo  :: SDL.V2 Float
    , tamObstaculo :: SDL.V2 Float
    } deriving (Show, Eq)

-- Datos de un item en pantalla
data Item = Item
    { posItem  :: SDL.V2 Float
    , tamItem  :: SDL.V2 Float
    , tipoItem :: TipoItem
    , activo   :: Bool
    } deriving (Show, Eq)

-- Pattern Matching para decidir que tipo de Item es un Item
data TipoItem 
    = Vida Float
    | Velocidad Float Float Bool
    | Puntos Float
    deriving (Show, Eq)

-- Un Buff activo en el jugador
data Buff = Buff 
    { buffNombre   :: DT.Text
    , buffEtiqueta :: DT.Text
    , buffTiempo   :: Float
    , buffValor    :: Float
    } deriving (Show, Eq)

-- Datos del Jugador
data Jugador = Jugador
    { posJugador  :: SDL.V2 Float
    , velJugador  :: Float
    , velCorrerJ  :: Float
    , velCaminarJ :: Float
    , velFactorJ  :: Float
    , vidJugador  :: Float
    , tamJugador  :: SDL.V2 Float
    , empujeJ     :: Float
    , velGolpeJ   :: SDL.V2 Float
    , buffsActivos :: [Buff]
    } deriving (Show, Eq)

-- Datos del Enemigo
data Enemigo = Enemigo
    { posEnemigo   :: SDL.V2 Float
    , velEnemigo   :: Float
    , vidEnemigo   :: Float
    , tamEnemigo   :: SDL.V2 Float
    , rangoVision  :: Float
    , empujeE      :: Float
    , velGolpeE    :: SDL.V2 Float
    } deriving (Show, Eq)

-- Datos de la Camara
data Camara = Camara
    { posCamara    :: SDL.V2 Float
    , deadzoneSize :: SDL.V2 Float
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