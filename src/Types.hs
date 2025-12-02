{-# LANGUAGE TemplateHaskell #-}
module Types where 
-- Módulos del sistema
import qualified System.Random  as SR
import qualified Lens.Micro.TH  as LMT
-- Módulos propios
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType

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

data TutorialFase 
    = FaseIntro      -- Overlay explicativo del HUD
    | FaseArmas      -- Entrega de armas y explicación de disparo
    | FaseCamara     -- Desbloqueo de Zoom y Deadzone
    | FaseZombieMsg  -- "Cuidado un zombie"
    | FaseCombate    -- Esperando a que mate al zombie
    | FaseBuffsMsg   -- Explicación de items y spawn de buffs
    | FaseBuffsWait  -- Espera (5s)
    | FaseTiempoMsg  -- Explicación del tiempo
    | FaseSobrevive  -- Mensaje final
    | FaseFin        -- Tutorial terminado (Juego normal)
    | FaseNula       -- No hay tutorial
    deriving (Show, Eq)

data GameState = GameState
    { _camara        ::   GType.Camara
    , _jugador       ::   PType.Jugador
    , _rng           ::   SR.StdGen
    , _enemigos      :: [ PType.Zombie    ]
    , _items         :: [ GType.Item      ]
    , _particulas    :: [ GType.Particula ]
    , _mapa          :: [ GType.Box       ]
    , _spawners      :: [ Spawner         ]
    , _tiempoJuego   :: Float
    , _tiempoTotal   :: Float
    , _cooldownUI   :: Float
    , _tutorialActivo :: Bool
    , _faseTutorial   :: TutorialFase
    , _timerTutorial  :: Float
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
    , _espacio       :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''Input