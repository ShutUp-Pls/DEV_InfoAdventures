module Inicio where

-- Modulos del sistema
import qualified SDL

-- Modulos propios
import qualified Types as TP
import qualified Personajes.Enemigo as PE
import qualified Mapas.Mapa as MM

nuevoJugador :: TP.Jugador
nuevoJugador = TP.Jugador
    { TP.posJugador   = SDL.V2 400 300
    , TP.velJugador   = 3.0
    , TP.buffsActivos = []
    , TP.velCaminarJ  = 3.0
    , TP.velCorrerJ   = 6.0
    , TP.velFactorJ   = 1.0
    , TP.vidJugador   = 100.0
    , TP.tamJugador   = SDL.V2 30.0 30.0
    , TP.empujeJ      = 10.0
    , TP.velGolpeJ    = SDL.V2 0 0
    }

enemigosIniciales :: [TP.Enemigo]
enemigosIniciales = 
    [ PE.crearEnemigo (SDL.V2 600 200)
    , PE.crearEnemigo (SDL.V2 100 100)
    , PE.crearEnemigo (SDL.V2 700 500)
    , PE.crearEnemigo (SDL.V2 200 450)
    ]

itemsIniciales :: [TP.Item]
itemsIniciales = 
    [ TP.Item (SDL.V2 300 300) (SDL.V2 20 20) (TP.Vida 10) True
    , TP.Item (SDL.V2 500 100) (SDL.V2 20 20) (TP.Velocidad 2.0 5.0 True) True
    , TP.Item (SDL.V2 600 100) (SDL.V2 20 20) (TP.Velocidad 3.0 3.0 False) True
    ]

estadoInicial :: TP.GameState
estadoInicial = TP.GameState
    { TP.jugador  = nuevoJugador
    , TP.enemigos = enemigosIniciales
    , TP.items    = itemsIniciales
    , TP.mapa     = MM.mapaBox
    , TP.camara   = TP.Camara
        { TP.posCamara    = SDL.V2 400 300
        , TP.deadzoneSize = SDL.V2 100 100
        }
    }