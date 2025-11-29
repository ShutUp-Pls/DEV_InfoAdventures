module Personajes.Jugador where
-- Módulos del sistema
import qualified SDL
import qualified Linear.Vector       as LV
import qualified Control.Monad.State as CMS
import qualified Lens.Micro          as LMi
-- Módulos propios
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
import qualified Types              as Input
import qualified Graficos.Dibujado  as GD
import qualified Objetos.Cono       as OC 
import qualified Personajes.Control as PC

nuevoJugador :: PType.Jugador
nuevoJugador = PType.Jugador
    { PType._factCorrer = 0.75
    , PType._spawnPoint = SDL.V2 400 300
    , PType._jugEnt = GType.Entidad
        { GType._entBox = GType.Box
            { GType._boxPos = SDL.V2 400 300
            , GType._boxTam = SDL.V2 30 30
            , GType._boxAng = 0.0
            , GType._boxRad = 15.0
            }
        , GType._entMov = GType.Movimiento 
            { GType._movVel = 4.0
            , GType._movRot = 10.0 
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
            , GType._empFrz = 10.0
            }
        , GType._entBuf = []
        }
    }

moverJugador :: Input.Input -> PType.Jugador -> [GType.Box] -> PType.Jugador
moverJugador input jugadorActual mapObstaculos = 
    let 
        entidadInicial = jugadorActual LMi.^. PType.jugEnt
        bonusRun       = jugadorActual LMi.^. PType.factCorrer
        entidadFinal = CMS.execState 
            (PC.rutinaControl input bonusRun mapObstaculos) 
            entidadInicial
    in
        jugadorActual LMi.& PType.jugEnt LMi..~ entidadFinal

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> PType.Jugador -> IO ()
dibujar renderer skinTexture camPos zoom player = do
    let posJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    let tamJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxTam
    let angJ = player LMi.^. PType.jugEnt . GType.entBox . GType.boxAng
    
    GD.dibujarTextura renderer skinTexture camPos zoom posJ tamJ angJ (SDL.V4 255 255 255 255)

    let centroJ = posJ + (tamJ LV.^* 0.5)
    OC.dibujarConoOutline renderer skinTexture camPos zoom centroJ angJ 60 30