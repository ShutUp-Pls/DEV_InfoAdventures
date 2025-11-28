module Inicio where

-- Modulos del sistema
import qualified SDL

-- Modulos propios
import qualified Types
import qualified Personajes.Enemigo as PE
import qualified Mapas.Mapa as MM

nuevoJugador :: Types.Jugador
nuevoJugador = Types.Jugador
    { Types.posJugador   = SDL.V2 400 300
    , Types.tamJugador   = SDL.V2 30.0 30.0
    , Types.velGolpeJ    = SDL.V2 0 0
    , Types.empujeJ      = 10.0
    , Types.velRotacion  = 10.0
    , Types.velJugador   = 3.0
    , Types.angJugador   = 0.0
    , Types.buffsActivos = []
    , Types.velCaminarJ  = 3.0
    , Types.velCorrerJ   = 6.0
    , Types.velFactorJ   = 1.0
    , Types.vidJugador   = 100.0
    }

enemigosIniciales :: [Types.Enemigo]
enemigosIniciales = 
    [ PE.crearEnemigo (SDL.V2 600 200)
    , PE.crearEnemigo (SDL.V2 100 100)
    , PE.crearEnemigo (SDL.V2 700 500)
    , PE.crearEnemigo (SDL.V2 200 450)
    ]

itemsIniciales :: [Types.Item]
itemsIniciales = 
    [ Types.Item (SDL.V2 300 300) (SDL.V2 20 20) (Types.Vida 10) True
    , Types.Item (SDL.V2 500 100) (SDL.V2 20 20) (Types.Velocidad 2.0 5.0 True) True
    , Types.Item (SDL.V2 600 100) (SDL.V2 20 20) (Types.Velocidad 3.0 3.0 False) True
    ]

estadoInicial :: Types.GameState
estadoInicial = Types.GameState
    { Types.jugador  = nuevoJugador
    , Types.enemigos = enemigosIniciales
    , Types.items    = itemsIniciales
    , Types.mapa     = MM.mapaBox
    , Types.camara   = Types.Camara
        { Types.posCamara    = SDL.V2 400 300
        , Types.deadzoneSize = SDL.V2 100 100
        }
    }