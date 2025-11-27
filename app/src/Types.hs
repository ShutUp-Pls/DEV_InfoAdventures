module Types where
    
-- Módulos del sistema
import qualified SDL
import qualified Data.Text as DT

-- ==========================================
-- DEFINICIÓN DE DATOS (DATA TYPES)
-- ==========================================

-- Obstaculo de colisión para mapas
data Obstaculo = Obstaculo
    { posObstaculo :: SDL.V2 Float
    , tamObstaculo :: SDL.V2 Float
    , angObstaculo :: Float
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

-- ==========================================
-- CLASES DE TIPOS (INTERFACES)
-- ==========================================

-- Hitbox: Cosas que ocupan espacio
class Hitbox a where
    getPos :: a -> SDL.V2 Float
    getTam :: a -> SDL.V2 Float
    getAng :: a -> Float

-- EntidadFisica: Cosas que se mueven y tienen física
class Hitbox a => EntidadFisica a where
    setPos      :: SDL.V2 Float -> a -> a
    
    getVelGolpe :: a -> SDL.V2 Float
    setVelGolpe :: SDL.V2 Float -> a -> a

-- ==========================================
-- INSTANCIAS (IMPLEMENTACIÓN)
-- ==========================================

instance Hitbox Jugador where
    getPos = posJugador
    getTam = tamJugador
    getAng _ = 0

instance EntidadFisica Jugador where
    setPos p j = j { posJugador = p }
    getVelGolpe = velGolpeJ
    setVelGolpe v j = j { velGolpeJ = v }

instance Hitbox Enemigo where
    getPos = posEnemigo
    getTam = tamEnemigo
    getAng _ = 0

instance EntidadFisica Enemigo where
    setPos p e = e { posEnemigo = p }
    getVelGolpe = velGolpeE
    setVelGolpe v e = e { velGolpeE = v }

instance Hitbox Item where
    getPos = posItem
    getTam = tamItem
    getAng _ = 0

instance Hitbox Obstaculo where
    getPos (Obstaculo p _ _) = p
    getTam (Obstaculo _ s _) = s
    getAng (Obstaculo _ _ a) = a