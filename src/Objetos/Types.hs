{-# LANGUAGE TemplateHaskell #-}
module Objetos.Types where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro.TH      as LMT
-- Módulos propios
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType

data ComportamientoParticula 
    = MovimientoLineal
    | MovimientoGradualDown
    deriving (Show, Eq)

data Particula = Particula
    { _parEnt    :: GType.Entidad
    , _parTip    :: ComportamientoParticula
    , _parId     :: Int
    } deriving (Show, Eq)
LMT.makeLenses ''Particula

data SpawnType 
    = SpawnEnemigo PType.Zombie
    | SpawnItem GType.Item
    deriving (Show, Eq)

data Spawner = Spawner
    { _spaBox        :: GType.Box
    , _areaSpawn     :: Float
    , _tipoSpawn     :: SpawnType
    , _rangoTiempo   :: (Float, Float)
    , _tiempoActual  :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Spawner

data Camara = Camara
    { _posCamara    :: SDL.V2 Float
    , _deadzoneSize :: SDL.V2 Float
    , _zoomLevel    :: Float
    , _zoomBase     :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Camara