{-# LANGUAGE TemplateHaskell #-}
module Personajes.Types where

import qualified SDL
import qualified Globals.Types as GType
import qualified Lens.Micro.TH as LMT

data Jugador = Jugador
    { _jugEnt    :: GType.Entidad
    , _factCorrer  :: Float
    , _spawnPoint  :: SDL.V2 Float
    } deriving (Show, Eq)
LMT.makeLenses ''Jugador

data Zombie = Zombie
    { _eneEnt    :: GType.Entidad
    , _eneRadVis :: Float
    , _eneVerJug :: Bool
    } deriving (Show, Eq)
LMT.makeLenses ''Zombie