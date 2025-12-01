{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Globals.Types where
-- Módulos de sistema
import qualified SDL
import qualified Control.Lens   as CL
import qualified Lens.Micro.TH  as LMT
import qualified Data.Text      as DT

data Box = Box
    { _boxPos    :: SDL.V2 Float
    , _boxTam    :: SDL.V2 Float
    , _boxAng    :: Float
    , _boxRad    :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Box

data Movimiento = Movimiento
    { _movVel    :: Float
    , _movRot    :: Float
    , _movFac    :: Float
    , _movAct   :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Movimiento

data Vida = Vida
    { _vidAct    :: Float
    , _vidMax    :: Float
    , _vidMrt    :: Int
    } deriving (Show, Eq)
LMT.makeLenses ''Vida

data Empuje = Empuje
    { _empVec    :: SDL.V2 Float
    , _empFrz    :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Empuje

data Buff = Buff 
    { _bufID     :: Int
    , _bufTmp    :: Float
    , _bufVlr    :: Float
    , _bufNom    :: DT.Text
    } deriving (Show, Eq)
LMT.makeLenses ''Buff

data Arma = Arma
    { _armID          :: Int
    , _eaHeatPerShot  :: Float
    , _eaCoolRate     :: Float 
    , _eaFireRate     :: Float
    , _eaMaxHeat      :: Float
    
    , _eaHeat         :: Float
    , _eaCool         :: Float
    , _eaJammed       :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''Arma

data TipoItem 
    = EsArma Arma
    | EsBuff Buff
    | NoItem
    deriving (Show, Eq)
CL.makePrisms ''TipoItem

data Item = Item
    { _iteId    :: Int
    , _iteBox   :: Box
    , _iteTipo  :: TipoItem
    , _iteNom   :: DT.Text
    , _iteInv   :: Bool
    , _iteAct   :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''Item

data Entidad = Entidad
    { _entBox    :: Box
    , _entMov    :: Movimiento
    , _entVid    :: Vida
    , _entEmp    :: Empuje
    , _entBuf    :: [Buff]
    , _entInv    :: [Item]
    , _entHnd    :: Item
    } deriving (Show, Eq)
LMT.makeLenses ''Entidad

itemVacio :: Item
itemVacio = Item
    { _iteId  = 0
    , _iteBox = Box 
        { _boxPos = (SDL.V2 0 0)
        , _boxTam = (SDL.V2 0 0)
        , _boxAng = 0
        , _boxRad = 0
        }
    , _iteNom = ""
    , _iteInv = False
    , _iteAct = False
    , _iteTipo = NoItem
    }