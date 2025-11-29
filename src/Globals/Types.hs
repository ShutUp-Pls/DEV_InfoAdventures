{-# LANGUAGE TemplateHaskell #-}
module Globals.Types where
-- Módulos de sistema
import qualified SDL
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

data Entidad = Entidad
    { _entBox    :: Box
    , _entMov    :: Movimiento
    , _entVid    :: Vida
    , _entEmp    :: Empuje
    , _entBuf    :: [Buff]
    } deriving (Show, Eq)
LMT.makeLenses ''Entidad