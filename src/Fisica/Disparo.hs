module Fisica.Disparo where
-- Módulos del sistema
import qualified SDL
import qualified System.Random      as SR
import qualified Linear.Vector      as LV
import qualified Lens.Micro         as LMi
-- Módulos propios
import qualified Personajes.Types   as PType
import qualified Globals.Types      as GType
import qualified Objetos.Particula  as OPart
import qualified Objetos.Arma       as OArma

procesarDisparo :: Float -> Bool -> SR.StdGen -> PType.Jugador -> ([GType.Particula], PType.Jugador, SR.StdGen)
procesarDisparo dt isClicking rng jugador = 
    let 
        maybeArma = jugador LMi.^? PType.jugEnt . GType.entHnd . GType.iteTipo . GType._EsArma
    in 
    case maybeArma of
        Nothing -> ([], jugador, rng)
        Just wState ->
            let
                -- 1. Lógica de Cooldown y Heat (Se mantiene igual)
                myMaxHeat     = wState LMi.^. GType.eaMaxHeat
                myHeatPerShot = wState LMi.^. GType.eaHeatPerShot
                myCoolRate    = wState LMi.^. GType.eaCoolRate
                myFireRate    = wState LMi.^. GType.eaFireRate
                weaponId      = wState LMi.^. GType.armID
                
                currentHeat   = wState LMi.^. GType.eaHeat
                currentCool   = wState LMi.^. GType.eaCool
                wasJammed     = wState LMi.^. GType.eaJammed

                newCool       = max 0 (currentCool - dt)
                newHeatBase   = max 0 (currentHeat - (myCoolRate * dt))
                isStillJammed = wasJammed && (newHeatBase > 0)
                shouldShoot   = isClicking && (newCool <= 0) && not isStillJammed

            in if shouldShoot
               then
                    let 
                        ent             = jugador LMi.^. PType.jugEnt
                        box             = ent LMi.^. GType.entBox
                        posTL           = box LMi.^. GType.boxPos
                        size            = box LMi.^. GType.boxTam
                        radio           = box LMi.^. GType.boxRad 
                        angleDeg        = box LMi.^. GType.boxAng
                        center          = posTL + (size LV.^* 0.5)
                        
                        angleRad        = angleDeg * (pi / 180.0)
                        dir             = SDL.V2 (cos angleRad) (sin angleRad)
                        distanciaSalida = radio + 5.0 
                        posSalida       = center + (dir LV.^* distanciaSalida)

                        (nuevasParticulas, rngFinal) = 
                            if weaponId == OArma.idArmLanzallamas 
                            then OPart.generarAbanicoFuego posSalida angleDeg 5 rng
                            else (OPart.generarProyectil OPart.idParBala posSalida angleDeg, rng)

                        heatPostShot = newHeatBase + myHeatPerShot
                        (finalHeat, finalJammed) = 
                            if heatPostShot >= myMaxHeat 
                            then (myMaxHeat, True) 
                            else (heatPostShot, isStillJammed)

                        nuevoEstadoArma = wState
                            LMi.& GType.eaCool    LMi..~ myFireRate 
                            LMi.& GType.eaHeat    LMi..~ finalHeat
                            LMi.& GType.eaJammed  LMi..~ finalJammed

                        jugadorActualizado = jugador LMi.& PType.jugEnt . GType.entHnd . GType.iteTipo . GType._EsArma LMi..~ nuevoEstadoArma

                    in (nuevasParticulas, jugadorActualizado, rngFinal)
               
               else
                    let 
                        nuevoEstadoArma = wState
                            LMi.& GType.eaCool    LMi..~ newCool
                            LMi.& GType.eaHeat    LMi..~ newHeatBase
                            LMi.& GType.eaJammed  LMi..~ isStillJammed
                        
                        jugadorActualizado = jugador LMi.& PType.jugEnt . GType.entHnd . GType.iteTipo . GType._EsArma LMi..~ nuevoEstadoArma
                        
                    in ([], jugadorActualizado, rng)