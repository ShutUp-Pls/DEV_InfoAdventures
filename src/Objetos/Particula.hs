module Objetos.Particula  where
-- Modulos del sistema
import qualified SDL
import qualified Lens.Micro     as LMi
import qualified Linear.Vector  as LV
import qualified System.Random  as SR
-- Modulos propios
import qualified Globals.Types      as GType

import qualified Fisica.Colisiones  as FCol

import qualified Graficos.Dibujado  as GD

idParBala, idParFuego, idParChispa, idParHumo :: Int
idParBala   = 2000
idParChispa = 2001
idParHumo   = 2002
idParFuego  = 2003
idParPlasma, idParCohete, idParSangre :: Int
idParPlasma = 2004
idParCohete = 2005
idParSangre = 2006

data ParticulaConfig = ParticulaConfig
    { _confTam   :: Float
    , _confVel   :: Float
    , _confVid   :: Float
    , _confFuer  :: Float
    , _confDano  :: Float
    , _confTipo  :: GType.ComportamientoParticula
    }

obtenerConfiguracion :: Int -> ParticulaConfig
obtenerConfiguracion pid
    | pid == idParPlasma = ParticulaConfig 
        { _confTam   = 12.0
        , _confVel   = 850.0
        , _confVid   = 1.5
        , _confFuer  = 30.0
        , _confDano  = 45.0
        , _confTipo  = GType.MovimientoLineal
        }
    | pid == idParCohete = ParticulaConfig 
        { _confTam   = 18.0
        , _confVel   = 500.0
        , _confVid   = 3.0
        , _confFuer  = 100.0
        , _confDano  = 250.0
        , _confTipo  = GType.MovimientoLineal
        }
    | pid == idParBala = ParticulaConfig 
        { _confTam   = 8.0
        , _confVel   = 1000.0
        , _confVid   = 2.0
        , _confFuer  = 20.0
        , _confDano  = 20.0
        , _confTipo  = GType.MovimientoLineal
        }
    | pid == idParFuego = ParticulaConfig 
        { _confTam   = 20.0
        , _confVel   = 400.0
        , _confVid   = 0.6
        , _confFuer  = 15.0
        , _confDano  = 20.0
        , _confTipo  = GType.MovimientoGradualDown
        }
    | pid == idParChispa = ParticulaConfig 
        { _confTam   = 6.0
        , _confVel   = 300.0
        , _confVid   = 0.3
        , _confFuer  = 0.0
        , _confDano  = 50.0
        , _confTipo  = GType.MovimientoLineal
        }
    | pid == idParHumo = ParticulaConfig 
        { _confTam   = 15.0
        , _confVel   = 100.0
        , _confVid   = 1.5
        , _confFuer  = 0.0
        , _confDano  = 0.0
        , _confTipo  = GType.MovimientoGradualDown
        }
    | pid == idParSangre = ParticulaConfig 
        { _confTam   = 10.0
        , _confVel   = 350.0
        , _confVid   = 0.8
        , _confFuer  = 0.0
        , _confDano  = 0.0
        , _confTipo  = GType.MovimientoLinealDecr
        }
    | otherwise = error $ "Configuración no definida para ID: " ++ show pid

crearEntidadParticula :: Int -> SDL.V2 Float -> Float -> GType.Entidad
crearEntidadParticula parId pos angulo = 
    let config = obtenerConfiguracion parId
    in GType.Entidad
        { GType._entBox = crearBoxDesdeConfig config pos angulo
        , GType._entMov = crearMovDesdeConfig config
        , GType._entVid = crearVidaDesdeConfig config
        , GType._entEmp = crearEmpujeDesdeConfig config
        , GType._entBuf = []
        , GType._entInv = []
        , GType._entHnd = GType.itemVacio
        }

crearParticula :: Int -> SDL.V2 Float -> Float -> Float -> Float -> Float -> GType.Particula
crearParticula pId pos angulo tamano velocidad vida =
    let 
        config  = obtenerConfiguracion pId
        entidad = crearEntidadDinamica config pos angulo tamano velocidad vida
    in
        GType.Particula 
            { GType._parEnt = entidad
            , GType._parTip = _confTipo config 
            , GType._parId  = pId
            , GType._parDmg = _confDano config
            }

crearBoxDesdeConfig :: ParticulaConfig -> SDL.V2 Float -> Float -> GType.Box
crearBoxDesdeConfig config pos angulo =
    let tam = _confTam config
    in GType.Box
        { GType._boxPos = pos
        , GType._boxTam = SDL.V2 tam tam
        , GType._boxAng = angulo
        , GType._boxRad = tam / 2
        }

crearMovDesdeConfig :: ParticulaConfig -> GType.Movimiento
crearMovDesdeConfig config = GType.Movimiento
    { GType._movVel = _confVel config
    , GType._movRot = 0
    , GType._movFac = 1.0
    , GType._movAct = 0.0
    }

crearVidaDesdeConfig :: ParticulaConfig -> GType.Vida
crearVidaDesdeConfig config = 
    let v = _confVid config
    in GType.Vida { GType._vidAct = v, GType._vidMax = v, GType._vidMrt = 0 }

crearEmpujeDesdeConfig :: ParticulaConfig -> GType.Empuje
crearEmpujeDesdeConfig config = GType.Empuje
    { GType._empVec = SDL.V2 0 0
    , GType._empFrz = _confFuer config
    }

crearEntidadDinamica :: ParticulaConfig -> SDL.V2 Float -> Float -> Float -> Float -> Float -> GType.Entidad
crearEntidadDinamica config pos angulo tamano velocidad vida = GType.Entidad
    { GType._entBox = mkBoxDinamica pos angulo tamano
    , GType._entMov = mkMovDinamica velocidad
    , GType._entVid = mkVidaDinamica vida
    , GType._entEmp = crearEmpujeDesdeConfig config 
    , GType._entBuf = []
    , GType._entInv = []
    , GType._entHnd = GType.itemVacio
    }
  where
    mkBoxDinamica p a t = GType.Box
        { GType._boxPos = p, GType._boxTam = SDL.V2 t t, GType._boxAng = a, GType._boxRad = t / 2 }
    mkMovDinamica v = GType.Movimiento
        { GType._movVel = v, GType._movRot = 0, GType._movFac = 1.0, GType._movAct = 0.0 }
    mkVidaDinamica v = GType.Vida
        { GType._vidAct = v, GType._vidMax = v, GType._vidMrt = 0 }

generarExplosion :: SDL.V2 Float -> Int -> Int -> (Float, Float) -> (Float, Float) -> (Float, Float) -> SR.StdGen -> ([GType.Particula], SR.StdGen)
generarExplosion pos n parId rVel rVid rTam genInicial = 
    go n genInicial []
  where
    go 0 gen acc = (acc, gen)
    go k gen acc =
        let 
            (angle, g1) = SR.randomR (0.0, 360.0) gen
            (speed, g2) = SR.randomR rVel g1
            (vida,  g3) = SR.randomR rVid g2
            (tam,   g4) = SR.randomR rTam g3
            part = crearParticula parId pos angle tam speed vida
        in go (k - 1) g4 (part : acc)

generarAbanicoFuego :: SDL.V2 Float -> Float -> Int -> SR.StdGen -> ([GType.Particula], SR.StdGen)
generarAbanicoFuego posOrigen anguloBase n genInicial =
    go n genInicial []
  where
    spreadAngle = 30.0
    rVel = (700.0, 1000.0)
    rVid = (0.4, 0.8)
    rTam = (15.0, 35.0)

    go 0 gen acc = (acc, gen)
    go k gen acc =
        let
            (angleVar, g1) = SR.randomR (-spreadAngle/2, spreadAngle/2) gen
            finalAngle = anguloBase + angleVar
            (speed, g2) = SR.randomR rVel g1
            (vida,  g3) = SR.randomR rVid g2
            (tam,   g4) = SR.randomR rTam g3
            part = crearParticula idParFuego posOrigen finalAngle tam speed vida
        in go (k - 1) g4 (part : acc)

generarProyectil :: Int -> SDL.V2 Float -> Float -> [GType.Particula]
generarProyectil pId posBoquilla angulo =
    let 
        config      = obtenerConfiguracion pId
        tamano      = _confTam config
        dano        = _confDano config
        tipoMov     = _confTipo config
        posCentrada = posBoquilla - (SDL.V2 (tamano/2) (tamano/2))
        entidad     = crearEntidadParticula pId posCentrada angulo
    in
        [ GType.Particula 
            { GType._parEnt = entidad
            , GType._parTip = tipoMov
            , GType._parId  = pId
            , GType._parDmg = dano
            }
        ]

generarEscopetazo :: SDL.V2 Float -> Float -> Int -> SR.StdGen -> ([GType.Particula], SR.StdGen)
generarEscopetazo posOrigen anguloBase n genInicial =
    go n genInicial []
  where
    spreadAngle = 15.0 
    rVel = (900.0, 1100.0)
    
    go 0 gen acc = (acc, gen)
    go k gen acc =
        let
            (angleVar, g1) = SR.randomR (-spreadAngle/2, spreadAngle/2) gen
            finalAngle = anguloBase + angleVar
            (speed, g2) = SR.randomR rVel g1
            partBase = crearParticula idParBala posOrigen finalAngle 6.0 speed 0.5
        in go (k - 1) g2 (partBase : acc)

generarSangre :: SDL.V2 Float -> Int -> SR.StdGen -> ([GType.Particula], SR.StdGen)
generarSangre pos n genInicial = 
    go n genInicial []
  where
    go 0 gen acc = (acc, gen)
    go k gen acc =
        let 
            (angle, g1) = SR.randomR (0.0, 360.0) gen
            (vida,  g2) = SR.randomR (0.3, 0.6) g1 
            (tam,   g3) = SR.randomR (8.0, 12.0) g2
            
            part = crearParticula idParSangre pos angle tam 0.0 vida
        in go (k - 1) g3 (part : acc)

actualizarParticulas :: Float -> [GType.Box] -> [GType.Particula] -> [GType.Particula]
actualizarParticulas dt mapa particulas =
    let 
        particulasProcesadas = map (procesarParticula dt) particulas
        particulasVivas      = filter (\p -> (p LMi.^. GType.parEnt . GType.entVid . GType.vidAct) > 0) particulasProcesadas
        particulasSinMuro    = filter (\p -> not (colisionaConMuro p mapa)) particulasVivas
    in particulasSinMuro

colisionaConMuro :: GType.Particula -> [GType.Box] -> Bool
colisionaConMuro p mapa =
    let box = p LMi.^. GType.parEnt . GType.entBox
    in case FCol.checkColision box mapa of
        Just _  -> True
        Nothing -> False

procesarParticula :: Float -> GType.Particula -> GType.Particula
procesarParticula dt particulaInicial =
    let
        posActual           = particulaInicial LMi.^. GType.parEnt . GType.entBox . GType.boxPos
        angDeg              = particulaInicial LMi.^. GType.parEnt . GType.entBox . GType.boxAng
        velocidad           = particulaInicial LMi.^. GType.parEnt . GType.entMov . GType.movVel
        angRad              = angDeg * (pi / 180.0)
        vectorDir           = SDL.V2 (cos angRad) (sin angRad)
        desplazamiento      = vectorDir LV.^* (velocidad * dt)
        nuevaPosicion       = posActual + desplazamiento
        particulaMovida     = particulaInicial LMi.& GType.parEnt . GType.entBox . GType.boxPos LMi..~ nuevaPosicion
        vidaActual          = particulaMovida LMi.^. GType.parEnt . GType.entVid . GType.vidAct
        nuevaVida           = vidaActual - dt
        particulaEnvejecida = particulaMovida LMi.& GType.parEnt . GType.entVid . GType.vidAct LMi..~ nuevaVida
        tipoParticula       = particulaEnvejecida LMi.^. GType.parTip
        particulaFinal      = aplicarComportamiento tipoParticula particulaEnvejecida
    in particulaFinal

aplicarComportamiento :: GType.ComportamientoParticula -> GType.Particula -> GType.Particula
aplicarComportamiento tipo p =
    let pId       = p LMi.^. GType.parId
        conf      = obtenerConfiguracion pId
        velBase   = _confVel conf
        
        vidAct    = p LMi.^. GType.parEnt . GType.entVid . GType.vidAct
        vidMax    = p LMi.^. GType.parEnt . GType.entVid . GType.vidMax
        
        ratio     = if vidMax > 0 then vidAct / vidMax else 0
    in
    case tipo of
        GType.MovimientoLineal -> p
        GType.MovimientoGradualDown -> p
                LMi.& GType.parEnt . GType.entMov . GType.movVel LMi.%~ (* 0.90)
                LMi.& GType.parEnt . GType.entBox . GType.boxTam LMi.%~ (LV.^* 0.95)
        GType.MovimientoAcelerado -> 
            let velActual = p LMi.^. GType.parEnt . GType.entMov . GType.movVel
                nuevaVel  = min 1000.0 (velActual + 50.0) 
            in p LMi.& GType.parEnt . GType.entMov . GType.movVel LMi..~ nuevaVel

        GType.MovimientoSalpicadura ->
            let mitad  = vidMax / 2
                factor = if vidAct > mitad then 1.15 else 0.85
                
            in p LMi.& GType.parEnt . GType.entMov . GType.movVel LMi.%~ (* factor)
                 LMi.& GType.parEnt . GType.entBox . GType.boxTam LMi.%~ (LV.^* 0.98)
        GType.MovimientoLinealDecr ->
             p LMi.& GType.parEnt . GType.entMov . GType.movVel LMi..~ (velBase * ratio)

        GType.MovimientoLinealIncr ->
             p LMi.& GType.parEnt . GType.entMov . GType.movVel LMi..~ (velBase * (1.0 - ratio))

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Particula -> IO ()
dibujar renderer texture camPos zoom particula = do
    let pos  = particula LMi.^. GType.parEnt . GType.entBox . GType.boxPos
        tam  = particula LMi.^. GType.parEnt . GType.entBox . GType.boxTam
        ang  = particula LMi.^. GType.parEnt . GType.entBox . GType.boxAng
        pId  = particula LMi.^. GType.parId

    if GD.esVisible pos tam ang camPos zoom 
        then do
          let (color, finalTam, finalAng) = case pId of
                  _ | pId == idParBala    -> (SDL.V4 255 240 200 255, tam, ang)
                    | pId == idParChispa  -> (SDL.V4 255 230 150 255, tam, ang)
                    | pId == idParFuego   -> (SDL.V4 255 100 20 200,  tam, ang)
                    | pId == idParHumo    -> (SDL.V4 100 100 100 150, tam, ang)
                    | pId == idParPlasma  -> (SDL.V4 0 255 255 255, SDL.V2 (tam LMi.^. SDL._x * 2.5) (tam LMi.^. SDL._y * 0.6), ang)
                    | pId == idParCohete  -> (SDL.V4 80 100 80 255, tam, ang)
                    | pId == idParSangre  -> (SDL.V4 130 0 0 230, tam, ang)
                    | otherwise           -> (SDL.V4 255 255 255 255, tam, ang)

          GD.dibujarTextura renderer texture camPos zoom pos finalTam finalAng color
        else return()