{-# LANGUAGE TemplateHaskell #-}
module Types where 
-- Módulos del sistema
import qualified System.Random  as SR
import qualified Lens.Micro.TH  as LMT
-- Módulos propios
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
import qualified Objetos.Types      as OType

data GameState = GameState
    { _camara        ::   OType.Camara
    , _jugador       ::   PType.Jugador
    , _rng           ::   SR.StdGen
    , _enemigos      :: [ PType.Zombie   ]
    , _items         :: [ GType.Item     ]
    , _particulas    :: [ OType.Particula]
    , _mapa          :: [ GType.Box      ]
    , _spawners      :: [ OType.Spawner  ]
    , _tiempoJuego   :: Float
    , _tiempoTotal   :: Float
    , _cooldownUI   :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''GameState

data Input = Input
    { _arriba        :: Bool
    , _abajo         :: Bool
    , _izquierda     :: Bool
    , _derecha       :: Bool
    , _shift         :: Bool
    , _decreaseDZ    :: Bool
    , _increaseDZ    :: Bool
    , _zoomIn        :: Bool
    , _zoomOut       :: Bool
    , _teclaRespawn  :: Bool
    , _teclaSalir    :: Bool
    , _disparar      :: Bool
    , _prevWeapon    :: Bool
    , _nextWeapon    :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''Input