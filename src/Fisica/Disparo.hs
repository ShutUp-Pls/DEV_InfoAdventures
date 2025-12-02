module Fisica.Disparo where

import qualified SDL
import qualified System.Random      as SR
import qualified Linear.Vector      as LV
import qualified Lens.Micro         as LMi
import qualified Data.Maybe         as DM

import qualified Personajes.Types   as PType
import qualified Globals.Types      as GType
import qualified Objetos.Particula  as OPart
import qualified Objetos.Arma       as OArma

procesarDisparo :: Float -> Bool -> SR.StdGen -> PType.Jugador -> ([GType.Particula], PType.Jugador, SR.StdGen)
procesarDisparo dt click rng jug = DM.fromMaybe ([], jug, rng) $ do
    armaState <- jug LMi.^? PType.jugEnt . GType.entHnd . GType.iteTipo . GType._EsArma
    
    let (armaEnfriada, desatascada) = aplicarEnfriamiento dt armaState
    let puedeDisparar = click && desatascada && (armaEnfriada LMi.^. GType.eaCool <= 0)

    if puedeDisparar then do
        let (parts, rngFinal) = resolverDisparo jug (armaEnfriada LMi.^. GType.armID) rng
        let armaDisparada     = aplicarCalentamiento armaEnfriada
        let jugFinal          = setArma armaDisparada jug
        return (parts, jugFinal, rngFinal)
    else do
        let jugFinal = setArma armaEnfriada jug
        return ([], jugFinal, rng)

aplicarEnfriamiento :: Float -> GType.Arma -> (GType.Arma, Bool)
aplicarEnfriamiento dt arma = 
    let newCool = max 0 ((arma LMi.^. GType.eaCool) - dt)
        newHeat = max 0 ((arma LMi.^. GType.eaHeat) - ((arma LMi.^. GType.eaCoolRate) * dt))
        stillJammed = (arma LMi.^. GType.eaJammed) && (newHeat > 0)
        newState = arma LMi.& GType.eaCool LMi..~ newCool LMi.& GType.eaHeat LMi..~ newHeat LMi.& GType.eaJammed LMi..~ stillJammed
    in (newState, not stillJammed)

aplicarCalentamiento :: GType.Arma -> GType.Arma
aplicarCalentamiento arma =
    let heat  = arma LMi.^. GType.eaHeat + arma LMi.^. GType.eaHeatPerShot
        maxH  = arma LMi.^. GType.eaMaxHeat
        isJam = heat >= maxH
    in arma LMi.& GType.eaCool LMi..~ (arma LMi.^. GType.eaFireRate)
            LMi.& GType.eaHeat LMi..~ (if isJam then maxH else heat)
            LMi.& GType.eaJammed LMi..~ isJam

resolverDisparo :: PType.Jugador -> Int -> SR.StdGen -> ([GType.Particula], SR.StdGen)
resolverDisparo jug wId rng =
    let (pos, ang) = calcularSalida (jug LMi.^. PType.jugEnt . GType.entBox)
    in case wId of
        _ | wId == OArma.idArmLanzallamas -> OPart.generarAbanicoFuego pos ang 5 rng
          | wId == OArma.idArmEscopeta    -> OPart.generarEscopetazo pos ang 10 rng
          | wId == OArma.idArmRPG         -> (OPart.generarProyectil OPart.idParCohete pos ang, rng)
          | wId == OArma.idArmPlasma      -> (OPart.generarProyectil OPart.idParPlasma pos ang, rng)
          | wId == OArma.idArmSniper      -> (OPart.generarProyectil OPart.idParChispa pos ang, rng)
          | otherwise                     -> (OPart.generarProyectil OPart.idParBala pos ang, rng)

calcularSalida :: GType.Box -> (SDL.V2 Float, Float)
calcularSalida box =
    let angRad = (box LMi.^. GType.boxAng) * (pi / 180.0)
        dir    = SDL.V2 (cos angRad) (sin angRad)
        center = (box LMi.^. GType.boxPos) + ((box LMi.^. GType.boxTam) LV.^* 0.5)
    in (center + (dir LV.^* ((box LMi.^. GType.boxRad) + 5.0)), box LMi.^. GType.boxAng)

setArma :: GType.Arma -> PType.Jugador -> PType.Jugador
setArma arma jug = jug LMi.& PType.jugEnt . GType.entHnd . GType.iteTipo . GType._EsArma LMi..~ arma