{-# LANGUAGE TemplateHaskell #-}
module Objetos.Types where
-- Módulos del sistema
import qualified SDL
import qualified Data.Text          as DT
import qualified Lens.Micro.TH      as LMT
-- Módulos propios
import qualified Personajes.Types   as PT
import qualified Globals.Types      as GType

idBuffVel :: Int
idBuffVel = 1

idBuffVid :: Int
idBuffVid = 2

data ItemBuff = ItemBuff
    { _iteBox   :: GType.Box
    , _iteBuf   :: GType.Buff
    , _iteNom   :: DT.Text
    , _iteAct   :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''ItemBuff

data SpawnType 
    = SpawnEnemigo PT.Zombie
    | SpawnItem ItemBuff
    deriving (Show, Eq)

data Spawner = Spawner
    { _spaBox        :: GType.Box
    , _areaSpawn     :: Float
    , _tipoSpawn     :: SpawnType
    , _rangoTiempo   :: (Float, Float)
    , _tiempoActual  :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Spawner

data ComportamientoParticula 
    = MovimientoLineal
    | MovimientoGradualDown
    deriving (Show, Eq)

data Particula = Particula
    { _parBox    :: GType.Box
    , _parVel    :: GType.Movimiento
    , _parVid    :: GType.Vida
    , _parTip    :: ComportamientoParticula
    } deriving (Show, Eq)
LMT.makeLenses ''Particula

data Camara = Camara
    { _posCamara    :: SDL.V2 Float
    , _deadzoneSize :: SDL.V2 Float
    , _zoomLevel    :: Float
    , _zoomBase     :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Camara