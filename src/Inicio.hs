{-# LANGUAGE OverloadedStrings #-}
module Inicio where
-- Módulos del sistema
import qualified SDL
import qualified System.Random as SR
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType

import qualified Mapas.Mapa as MM

import qualified Objetos.Arma       as OArma
import qualified Objetos.Buff       as OBuff

import qualified Personajes.Jugador as PJ
import qualified Personajes.Zombie  as PE

aplicarRetraso :: Float -> Types.Spawner -> Types.Spawner
aplicarRetraso segundosExtra sp = sp { Types._tiempoActual = (Types._tiempoActual sp) + segundosExtra }

itemsIniciales :: [GType.Item]
itemsIniciales = 
    [ OBuff.crearItemBuffVidA (SDL.V2 300 900)
    , OBuff.crearItemBuffVelA (SDL.V2 500 900)
    , OArma.crearItemArma OArma.idArmGlock       (SDL.V2 3880 1350)
    , OArma.crearItemArma OArma.idArmEscopeta    (SDL.V2 4000 1350)
    , OArma.crearItemArma OArma.idArmFusil       (SDL.V2 4120 1350)
    , OArma.crearItemArma OArma.idArmLanzallamas (SDL.V2 4240 1350)
    ]

spawnerZBGlobal :: Types.Spawner
spawnerZBGlobal = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnEnemigo (PE.crearZombie PE.idZmbBasico (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 5.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerZRGlobal :: Types.Spawner
spawnerZRGlobal = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnEnemigo (PE.crearZombie PE.idZmbCorredor (SDL.V2 0 0))
    , Types._rangoTiempo  = (5.0, 15.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerZTGlobal :: Types.Spawner
spawnerZTGlobal = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnEnemigo (PE.crearZombie PE.idZmbTanque (SDL.V2 0 0))
    , Types._rangoTiempo  = (10.0, 20.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerTiempo :: Types.Spawner
spawnerTiempo = Types.Spawner
    { Types._areaSpawn    = 300.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemTiempo (SDL.V2 0 0))
    , Types._rangoTiempo  = (10.0, 20.0)
    , Types._tiempoActual = 5.0
    , Types._spaBox       = GType.Box (SDL.V2 1000 500) (SDL.V2 32 32) 0.0 0.0
    }

    
estadoInicial :: Bool -> IO Types.GameState
estadoInicial isTutorial = do
    semilla <- SR.newStdGen

    let retrasoInicial = if isTutorial then 20.0 else 0.0

    let listaSpawnersBase = [spawnerZBGlobal, spawnerZRGlobal, spawnerZTGlobal, spawnerTiempo]
    let listaSpawnersFinal = map (aplicarRetraso retrasoInicial) listaSpawnersBase

    return $ Types.GameState
        { Types._jugador        = PJ.crearJugador (SDL.V2 4500 500) (SDL.V2 4500 500)
        , Types._enemigos       = []
        , Types._items          = itemsIniciales
        , Types._particulas     = []
        , Types._mapa           = MM.mapaBox
        , Types._rng            = semilla
        , Types._spawners       = listaSpawnersFinal
        , Types._camara         = GType.Camara (SDL.V2 400 300) (SDL.V2 100 100) 0.75 0.75
        , Types._tiempoJuego    = 30.0
        , Types._tiempoTotal    = 0.0
        , Types._cooldownUI     = 0.0
        }   