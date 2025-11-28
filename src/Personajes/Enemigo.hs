module Personajes.Enemigo where

-- Modulos del sitema
import qualified SDL
import qualified Control.Monad.State as CMS
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Fisica.Movimiento as FM
import qualified Graficos.Dibujado as GD
import qualified Objetos.Cono as OC

crearEnemigo :: SDL.V2 Float -> Types.Enemigo
crearEnemigo pos = Types.Enemigo
    { Types.posEnemigo      = pos
    , Types.tamEnemigo      = SDL.V2 30 30
    , Types.velGolpeE       = SDL.V2 0 0
    , Types.empujeE         = 5.0
    , Types.velEnemigo      = 1.5
    , Types.angEnemigo      = 0.0
    , Types.vidEnemigo      = 100.0
    , Types.rangoVision     = 200.0
    , Types.radInterno      = 1.0
    , Types.rechazoE        = 0.2
    , Types.veJugador       = False
    }

actFisicasMovEnemigo :: SDL.V2 Float -> [Types.Obstaculo] -> CMS.State Types.Enemigo ()
actFisicasMovEnemigo velocidad mapObstaculos = do
    _ <- FM.resolverFisica velocidad mapObstaculos
    return ()

calcularDirEnemigo :: Types.Enemigo -> Types.Jugador -> SDL.V2 Float
calcularDirEnemigo enemigo player =
    let posE = Types.posEnemigo enemigo
        posJ = Types.posJugador player
        dist = LM.distance posE posJ
        rango = Types.rangoVision enemigo
    in if dist < rango
       then SDL.V2 1 0
       else SDL.V2 0 0

checkVision :: Types.Enemigo -> Types.Jugador -> Bool
checkVision enemigo player =
    let posE = Types.posEnemigo enemigo
        posJ = Types.posJugador player
        dist = LM.distance posE posJ
        rango = Types.rangoVision enemigo

    in dist < rango

moverEnemigo :: Types.Enemigo -> SDL.V2 Float -> [Types.Obstaculo] -> Types.Jugador -> Types.Enemigo
moverEnemigo enemigoIni _ mapObstaculos player = 
    let 
        -- 1. Determinamos si lo ve
        detectado = checkVision enemigoIni player

        -- 2. Lógica de movimiento
        posE = Types.posEnemigo enemigoIni
        posJ = Types.posJugador player
        angActual = Types.angEnemigo enemigoIni
        
        -- BORRADO: delta = if detectado ... (Ya no lo necesitamos)

        rotSpeed = 15.0
        speed    = Types.velEnemigo enemigoIni
        
        -- Si lo detecta, activamos la bandera de movimiento
        shouldMove = detectado 
        
        targetAng = Utils.calcularAnguloHacia posE posJ
        nuevoAngulo = if shouldMove
                      then Utils.suavizarAngulo angActual targetAng rotSpeed
                      else angActual

        forwardDir = Utils.anguloAVector nuevoAngulo
        
        velocity = if shouldMove
                   then forwardDir LV.^* speed
                   else SDL.V2 0 0

        enemigoMovido = CMS.execState (actFisicasMovEnemigo velocity mapObstaculos) enemigoIni

    in
        enemigoMovido 
            { Types.angEnemigo = nuevoAngulo 
            , Types.veJugador  = detectado 
            }

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> Types.Enemigo -> IO ()
dibujar renderer skinTexture camPos zoom enem = do
    let posE = Types.posEnemigo enem
    let tamE = Types.tamEnemigo enem
    let angE = Types.angEnemigo enem

    GD.dibujarTextura renderer skinTexture camPos zoom posE tamE angE (SDL.V3 0 255 0)

    let centroE = posE + (tamE LV.^* 0.5)
    OC.dibujarConoOutline renderer skinTexture camPos zoom centroE angE 100 45