{-# LANGUAGE TemplateHaskell #-}
module Personajes.Types where
-- Modulos del sistema
import qualified SDL
import qualified Lens.Micro.TH as LMT
-- Modulos propios
import qualified Globals.Types as GType

data Jugador = Jugador
    { _jugEnt      :: GType.Entidad
    , _factCorrer  :: Float
    , _spawnPoint  :: SDL.V2 Float
    , _estaVivo    :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''Jugador

data Zombie = Zombie
    { _zmbEnt       :: GType.Entidad
    , _zmbRadVis    :: Float
    , _zmbVerJug    :: Bool
    , _zmbDamage    :: Float
    , _zmbTeamRd    :: Float
    , _zmdId        :: Int
    , _zmbHitTimer  :: Float
    } deriving (Show, Eq)
LMT.makeLenses ''Zombie