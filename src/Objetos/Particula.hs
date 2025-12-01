module Objetos.Particula  where
-- Modulos del sistema
import qualified SDL
import qualified Lens.Micro     as LMi
import qualified System.Random  as SR
-- Modulos propios
import qualified Objetos.Types      as OType
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD

idParBala   :: Int
idParBala   = 2000
tamanoBala, velocidadBala, vidaBala, fuerzaBala :: Float
tamanoBala = 8.0
velocidadBala = 1000.0
vidaBala = 2.0
fuerzaBala = 20.0

idParFuego :: Int
idParFuego = 2003
fuerzaFuego :: Float
fuerzaFuego = 15.0

idParChispa :: Int
idParChispa = 2001

tamanoChispa, velocidadChispa, vidaChispa :: Float
tamanoChispa    = 6.0
velocidadChispa = 300.0
vidaChispa      = 0.3

idParHumo :: Int
idParHumo = 2002

crearEntidadParticula :: Int -> SDL.V2 Float -> Float -> GType.Entidad
crearEntidadParticula parId pos angulo = GType.Entidad
    { GType._entBox = crearBoxParticula parId pos angulo
    , GType._entMov = crearMovParticula parId
    , GType._entVid = crearVidaParticula parId
    , GType._entEmp = crearEmpujeParticula parId
    , GType._entBuf = []
    , GType._entInv = []
    , GType._entHnd = GType.itemVacio
    }

crearBoxParticula :: Int -> SDL.V2 Float -> Float -> GType.Box
crearBoxParticula parId pos angulo
    | parId == idParBala   = mkBox tamanoBala
    | parId == idParChispa = mkBox tamanoChispa
    | parId == idParFuego  = mkBox 20.0 
    | otherwise            = error "crearBoxParticula: id de partícula desconocido"
  where
    mkBox tam = GType.Box
        { GType._boxPos = pos
        , GType._boxTam = SDL.V2 tam tam
        , GType._boxAng = angulo
        , GType._boxRad = tam / 2
        }

crearMovParticula :: Int -> GType.Movimiento
crearMovParticula parId
    | parId == idParBala   = mkMov velocidadBala
    | parId == idParChispa = mkMov velocidadChispa
    | parId == idParFuego  = mkMov 400.0 
    | otherwise            = error "crearMovParticula: id de partícula desconocido"
  where
    mkMov vel = GType.Movimiento
        { GType._movVel = vel
        , GType._movRot = 0
        , GType._movFac = 1.0
        , GType._movAct = 0.0
        }

crearVidaParticula :: Int -> GType.Vida
crearVidaParticula parId
    | parId == idParBala   = mkVida vidaBala
    | parId == idParChispa = mkVida vidaChispa
    | parId == idParFuego  = mkVida 0.6
    | otherwise            = error "crearVidaParticula: id de partícula desconocido"
  where
    mkVida v = GType.Vida
        { GType._vidAct = v
        , GType._vidMax = v
        , GType._vidMrt = 0
        }

crearEmpujeParticula :: Int -> GType.Empuje
crearEmpujeParticula parId
    | parId == idParBala   = baseEmp
    | parId == idParChispa = baseEmp
    | parId == idParFuego  = baseEmp
    | otherwise            = error "crearEmpujeParticula: id de partícula desconocido"
  where
    baseEmp = GType.Empuje
        { GType._empVec = SDL.V2 0 0
        , GType._empFrz = 0
        }

crearEntidadDinamica :: Int -> SDL.V2 Float -> Float -> Float -> Float -> Float -> GType.Entidad
crearEntidadDinamica parId pos angulo tamano velocidad vida = GType.Entidad
    { GType._entBox = mkBoxDinamica pos angulo tamano
    , GType._entMov = mkMovDinamica velocidad
    , GType._entVid = mkVidaDinamica vida
    , GType._entEmp = crearEmpujeParticula parId
    , GType._entBuf = []
    , GType._entInv = []
    , GType._entHnd = GType.itemVacio
    }
  where
    mkBoxDinamica p a t = GType.Box
        { GType._boxPos = p
        , GType._boxTam = SDL.V2 t t
        , GType._boxAng = a
        , GType._boxRad = t / 2
        }
    mkMovDinamica v = GType.Movimiento
        { GType._movVel = v
        , GType._movRot = 0
        , GType._movFac = 1.0
        , GType._movAct = 0.0
        }
    mkVidaDinamica v = GType.Vida
        { GType._vidAct = v
        , GType._vidMax = v
        , GType._vidMrt = 0
        }


generarExplosion :: SDL.V2 Float -> Int -> Int -> OType.ComportamientoParticula -> (Float, Float) -> (Float, Float) -> (Float, Float) -> SR.StdGen -> ([OType.Particula], SR.StdGen)
generarExplosion pos n parId tipoMov rVel rVid rTam genInicial = 
    go n genInicial []
  where
    go 0 gen acc = (acc, gen)
    go k gen acc =
        let 
            (angle, g1) = SR.randomR (0.0, 360.0) gen
            (speed, g2) = SR.randomR rVel g1
            (vida,  g3) = SR.randomR rVid g2
            (tam,   g4) = SR.randomR rTam g3
            ent = crearEntidadDinamica parId pos angle tam speed vida
            part = OType.Particula { OType._parEnt = ent, OType._parTip = tipoMov, OType._parId = parId}
        in
            go (k - 1) g4 (part : acc)

generarAbanicoFuego :: SDL.V2 Float -> Float -> Int -> SR.StdGen -> ([OType.Particula], SR.StdGen)
generarAbanicoFuego posOrigen anguloBase n genInicial =
    go n genInicial []
  where
    spreadAngle = 30.0
    rVel = (350.0, 550.0)
    rVid = (0.4, 0.8)
    rTam = (15.0, 35.0)

    go 0 gen acc = (acc, gen)
    go k gen acc =
        let
            -- Variación aleatoria del ángulo dentro del spread
            (angleVar, g1) = SR.randomR (-spreadAngle/2, spreadAngle/2) gen
            finalAngle = anguloBase + angleVar
            
            (speed, g2) = SR.randomR rVel g1
            (vida,  g3) = SR.randomR rVid g2
            (tam,   g4) = SR.randomR rTam g3
            
            -- Usamos idParFuego y MovimientoGradualDown para que frenen rápido
            ent = crearEntidadDinamica idParFuego posOrigen finalAngle tam speed vida
            part = OType.Particula { OType._parEnt = ent, OType._parTip = OType.MovimientoGradualDown, OType._parId = idParFuego}
        in
            go (k - 1) g4 (part : acc)

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> OType.Particula -> IO ()
dibujar renderer texture camPos zoom particula = do
    let pos  = particula LMi.^. OType.parEnt . GType.entBox . GType.boxPos
        tam  = particula LMi.^. OType.parEnt . GType.entBox . GType.boxTam
        pId  = particula LMi.^. OType.parId

        color =
            case pId of
                _ | pId == idParBala    -> SDL.V4 200 230 255 255
                  | pId == idParChispa  -> SDL.V4 255 230 150 255
                  | pId == idParFuego   -> SDL.V4 255 120 40 220
                  | pId == idParHumo    -> SDL.V4 80 80 80 180
                  | otherwise           -> SDL.V4 100 100 100 255

    GD.dibujarTextura renderer texture camPos zoom pos tam 0.0 color