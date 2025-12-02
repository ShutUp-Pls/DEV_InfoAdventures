{-# LANGUAGE OverloadedStrings #-}
module Inicio where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro         as LMi
import qualified System.Random      as SR
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
import qualified Mapas.Mapa         as MM
import qualified Objetos.Arma       as OArma
import qualified Objetos.Buff       as OBuff
import qualified Personajes.Jugador as PJ
import qualified Personajes.Zombie  as PE

aplicarRetraso :: Float -> Types.Spawner -> Types.Spawner
aplicarRetraso segundosExtra sp = sp { Types._tiempoActual = (Types._tiempoActual sp) + segundosExtra }

itemsIniciales :: [GType.Item]
itemsIniciales = 
    [ OArma.crearItemArma OArma.idArmGlock       (SDL.V2 3880 1350)
    , OArma.crearItemArma OArma.idArmEscopeta    (SDL.V2 4000 1350)
    , OArma.crearItemArma OArma.idArmFusil       (SDL.V2 4120 1350)
    , OArma.crearItemArma OArma.idArmLanzallamas (SDL.V2 4240 1350)
    ]

spawnerZBGlobal :: Types.Spawner
spawnerZBGlobal = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnEnemigo (PE.crearZombie PE.idZmbBasico (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 2.5)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerZRGlobal :: Types.Spawner
spawnerZRGlobal = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnEnemigo (PE.crearZombie PE.idZmbCorredor (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 5.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerZTGlobal :: Types.Spawner
spawnerZTGlobal = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnEnemigo (PE.crearZombie PE.idZmbTanque (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 7.5)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerTiempo :: Types.Spawner
spawnerTiempo = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffTiempo (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 5.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerVelA :: Types.Spawner
spawnerVelA = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffVelA (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 5.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerVelB :: Types.Spawner
spawnerVelB = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffVelB (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 15.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerVelC :: Types.Spawner
spawnerVelC = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffVelC (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 45.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerVidA :: Types.Spawner
spawnerVidA = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffVidA (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 8.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerVidB :: Types.Spawner
spawnerVidB = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffVidB (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 20.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerVidC :: Types.Spawner
spawnerVidC = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffVidC (SDL.V2 0 0))
    , Types._rangoTiempo  = (30.0, 60.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerTiempoB :: Types.Spawner
spawnerTiempoB = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffTiempoB (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 25.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerTiempoC :: Types.Spawner
spawnerTiempoC = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OBuff.crearItemBuff OBuff.idBuffTiempoC (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 80.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }
    
spawnerGlock :: Types.Spawner
spawnerGlock = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmGlock (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 10.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerSMG :: Types.Spawner
spawnerSMG = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmSMG (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 12.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerEscopeta :: Types.Spawner
spawnerEscopeta = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmEscopeta (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 25.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerFusil :: Types.Spawner
spawnerFusil = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmFusil (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 25.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerSniper :: Types.Spawner
spawnerSniper = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmSniper (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 45.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerLanzallamas :: Types.Spawner
spawnerLanzallamas = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmLanzallamas (SDL.V2 0 0))
    , Types._rangoTiempo  = (0.0, 45.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerRPG :: Types.Spawner
spawnerRPG = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmRPG (SDL.V2 0 0))
    , Types._rangoTiempo  = (180.0, 300.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerPlasma :: Types.Spawner
spawnerPlasma = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmPlasma (SDL.V2 0 0))
    , Types._rangoTiempo  = (180.0, 300.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }

spawnerMinigun :: Types.Spawner
spawnerMinigun = Types.Spawner
    { Types._areaSpawn    = 4800.0
    , Types._tipoSpawn    = Types.SpawnItem (OArma.crearItemArma OArma.idArmMinigun (SDL.V2 0 0))
    , Types._rangoTiempo  = (240, 300.0)
    , Types._tiempoActual = 0.0
    , Types._spaBox       = GType.Box (SDL.V2 4700 450) (SDL.V2 32 32) 0.0 0.0
    }
    
estadoInicial :: Bool -> IO Types.GameState
estadoInicial isTutorial = do
    semilla <- SR.newStdGen

    let retrasoInicial = if isTutorial then 9999.0 else 0.0

    let spawnersEnemigos = [spawnerZBGlobal, spawnerZRGlobal, spawnerZTGlobal]
    let spawnersBuffs    = [ spawnerTiempo, spawnerTiempoB, spawnerTiempoC
                           , spawnerVelA, spawnerVelB, spawnerVelC
                           , spawnerVidA, spawnerVidB, spawnerVidC
                           ]
    let spawnersArmas    = [ spawnerGlock, spawnerSMG, spawnerEscopeta, spawnerFusil
                           , spawnerSniper, spawnerLanzallamas
                           , spawnerRPG, spawnerPlasma, spawnerMinigun
                           ]

    let listaSpawnersBase = spawnersEnemigos ++ spawnersBuffs ++ spawnersArmas
    let listaSpawnersFinal = map (aplicarRetraso retrasoInicial) listaSpawnersBase

    let spawnPointReal = SDL.V2 4500 500
    let posTutorial    = SDL.V2 4008 1180
    
    let jugadorBase = PJ.crearJugador spawnPointReal spawnPointReal

    let jugadorFinal = if isTutorial 
                       then jugadorBase LMi.& PType.jugEnt . GType.entBox . GType.boxPos LMi..~ posTutorial
                       else jugadorBase

    let itemsJuego = if isTutorial then [] else itemsIniciales
    let tiempoInit = if isTutorial then 60.0 else 30.0
    let faseInit   = if isTutorial then Types.FaseIntro else Types.FaseNula

    return $ Types.GameState
        { Types._jugador        = jugadorFinal
        , Types._enemigos       = []
        , Types._items          = itemsJuego
        , Types._particulas     = []
        , Types._mapa           = MM.mapaBox
        , Types._rng            = semilla
        , Types._spawners       = listaSpawnersFinal
        , Types._camara         = GType.Camara (SDL.V2 400 300) (SDL.V2 100 100) 0.75 0.75
        , Types._tiempoJuego    = tiempoInit
        , Types._tiempoTotal    = 0.0
        , Types._cooldownUI     = 0.0
        , Types._tutorialActivo = isTutorial
        , Types._faseTutorial   = faseInit
        , Types._timerTutorial  = 0.0
        }