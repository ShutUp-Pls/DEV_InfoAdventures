{-# LANGUAGE OverloadedStrings #-}
module Inicio where
-- Módulos del sistema
import qualified SDL
import qualified System.Random as SR
-- Módulos propios
import qualified Types
import qualified Globals.Types as GType
import qualified Personajes.Types as PType
import qualified Objetos.Types as OType
import qualified Personajes.Jugador as PJ
import qualified Personajes.Zombie as PE
import qualified Mapas.Mapa as MM

enemigosIniciales :: [PType.Zombie]
enemigosIniciales = 
    [ PE.crearZombie (SDL.V2 600 200)
    , PE.crearZombie (SDL.V2 100 100)
    ]

itemsIniciales :: [OType.ItemBuff]
itemsIniciales = 
    [
      OType.ItemBuff 
        { OType._iteAct = True 
        , OType._iteNom = "Poción Vida"
        , OType._iteBox = GType.Box (SDL.V2 300 300) (SDL.V2 20 20) 0.0 10.0
        , OType._iteBuf = GType.Buff 
            { GType._bufID  = OType.idBuffVid     
            , GType._bufTmp = 0.0
            , GType._bufVlr = 10.0
            , GType._bufNom = ""
            }
        }
    ,
      OType.ItemBuff 
        { OType._iteAct = True 
        , OType._iteNom = "Bebida Energy"
        , OType._iteBox = GType.Box (SDL.V2 500 100) (SDL.V2 20 20) 0.0 10.0
        , OType._iteBuf = GType.Buff 
            { GType._bufID  = OType.idBuffVel
            , GType._bufTmp = 5.0
            , GType._bufVlr = 2.0
            , GType._bufNom = ""
            }
        }
    ]

spawnerTest :: OType.Spawner
spawnerTest = OType.Spawner
    { OType._areaSpawn    = 150.0
    , OType._tipoSpawn    = OType.SpawnEnemigo (PE.crearZombie (SDL.V2 0 0))
    , OType._rangoTiempo  = (2.0, 5.0)
    , OType._tiempoActual = 10.0
    , OType._spaBox       = GType.Box (SDL.V2 1700 350) (SDL.V2 32 32) 0.0 0.0
    }
    
estadoInicial :: IO Types.GameState
estadoInicial = do
    semilla <- SR.newStdGen

    return $ Types.GameState
        { Types._jugador    = PJ.nuevoJugador
        , Types._enemigos   = enemigosIniciales
        , Types._itemsBuff      = itemsIniciales
        , Types._particulas = []
        , Types._mapa       = MM.mapaTest
        , Types._rng        = semilla
        , Types._spawners   = [spawnerTest]
        , Types._camara     = OType.Camara (SDL.V2 400 300) (SDL.V2 100 100) 1.0 1.0
        }