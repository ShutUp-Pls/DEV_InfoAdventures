module Personajes.Zombie where

-- Módulos del sistema
import qualified SDL
import qualified Control.Monad.State as CMS
import qualified Linear.Vector       as LV
import qualified Lens.Micro          as LMi

-- Módulos propios
import qualified Personajes.Types   as PType
import qualified Personajes.IA      as IA
import qualified Globals.Types      as GType
import qualified Objetos.Cono       as OC
import qualified Graficos.Dibujado  as GD
import qualified Fisica.Colisiones  as FC

crearZombie :: SDL.V2 Float -> PType.Zombie
crearZombie pos = PType.Zombie
    { PType._eneEnt = GType.Entidad
        { GType._entBox = GType.Box
            { GType._boxPos = pos
            , GType._boxTam = SDL.V2 30 30
            , GType._boxAng = 0.0
            , GType._boxRad = 15.0
            }
        , GType._entMov = GType.Movimiento
            { GType._movVel = 1.5
            , GType._movRot = 15.0
            , GType._movFac = 1.0
            , GType._movAct = 0.0
            }
        , GType._entVid = GType.Vida
            { GType._vidAct = 100.0
            , GType._vidMax = 100.0
            , GType._vidMrt = 0
            }
        , GType._entEmp = GType.Empuje
            { GType._empVec = SDL.V2 0 0
            , GType._empFrz = 5.0
            }
        , GType._entBuf = []
        }
    , PType._eneRadVis = 200.0
    , PType._eneVerJug = False
    }

moverZombie :: PType.Zombie -> [GType.Box] -> PType.Jugador -> PType.Zombie
moverZombie zombieActual mapObstaculos player = 
    let 
        entidadInicial = zombieActual   LMi.^. PType.eneEnt
        rangoVis       = zombieActual   LMi.^. PType.eneRadVis
        posZombie      = entidadInicial LMi.^. GType.entBox . GType.boxPos
        posJugador     = player         LMi.^. PType.jugEnt . GType.entBox . GType.boxPos

        detectado = IA.enRangoDeVision posZombie rangoVis posJugador
        entidadFinal = CMS.execState 
            (IA.rutinaPersecucion posJugador detectado mapObstaculos) 
            entidadInicial
    in
        zombieActual 
            LMi.& PType.eneEnt LMi..~ entidadFinal
            LMi.& PType.eneVerJug LMi..~ detectado

updateEnemies :: PType.Jugador -> [PType.Zombie] -> [GType.Box] -> [PType.Zombie]
updateEnemies jug enemigos mapa =
    let 
        zombiesMovidos = map (\enemy -> moverZombie enemy mapa jug) enemigos
    in
        FC.resolverColisionesEnemigos zombiesMovidos

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> PType.Zombie -> IO ()
dibujar renderer skinTexture camPos zoom enem = do
    let posE = enem LMi.^. PType.eneEnt . GType.entBox . GType.boxPos
    let tamE = enem LMi.^. PType.eneEnt . GType.entBox . GType.boxTam
    let angE = enem LMi.^. PType.eneEnt . GType.entBox . GType.boxAng

    GD.dibujarTextura renderer skinTexture camPos zoom posE tamE angE (SDL.V4 0 255 0 255)

    let centroE = posE + (tamE LV.^* 0.5)
    OC.dibujarConoOutline renderer skinTexture camPos zoom centroE angE 100 45