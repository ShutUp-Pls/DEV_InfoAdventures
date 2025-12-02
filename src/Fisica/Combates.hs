module Fisica.Combates where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro             as LMi
import qualified Linear.Metric          as LMe
import qualified Linear.Vector          as LVe
-- Módilos propios
import qualified Globals.Types          as GType
import qualified Personajes.Types       as PType

import qualified Fisica.Colisiones      as FC

choqueJugadorZombies :: PType.Jugador -> [PType.Zombie] -> (PType.Jugador, [PType.Zombie])
choqueJugadorZombies jug zombies =
    foldl procesar (jug, []) zombies
  where
    procesar (jugActual, zsAcum) z =
        let 
            jugEnt              = jugActual LMi.^. PType.jugEnt
            zomEnt                = z         LMi.^. PType.zmbEnt
            zomDmg                = z         LMi.^. PType.zmbDamage
            resultadoColision   = detectarColisionFisica zomEnt jugEnt
        in
            case resultadoColision of
                Nothing     -> (jugActual, zsAcum ++ [z])
                Just mtv    -> 
                    let
                        dmgParaZ = 0.0
                        dmgParaJ = zomDmg

                        empParaZ = jugEnt LMi.^. GType.entEmp . GType.empFrz 
                        empParaJ = zomEnt   LMi.^. GType.entEmp . GType.empFrz 

                        (zEntFisica, jugEntFisica)  = aplicarEmpujeMutuo empParaZ empParaJ mtv zomEnt jugEnt
                        (zEntFinal, jugEntFinal)    = aplicarDanoMutuo dmgParaZ dmgParaJ zEntFisica jugEntFisica

                        jugNuevo = jugActual LMi.& PType.jugEnt LMi..~ jugEntFinal
                        zNuevo   = z        LMi.& PType.zmbEnt LMi..~ zEntFinal
                    in (jugNuevo, zsAcum ++ [zNuevo])

choqueParticulasZombies :: [GType.Particula] -> [PType.Zombie] -> ([GType.Particula], [PType.Zombie])
choqueParticulasZombies particulas zombies = 
    foldl procesarParticula ([], zombies) particulas
  where
    procesarParticula (partsAcum, zombiesActuales) particula =
        case checkImpactoUnico particula zombiesActuales of
            Nothing -> (partsAcum ++ [particula], zombiesActuales)
            Just (zombieIndex, zombieModificado) ->
                let zombiesActualizados = replaceAt zombieIndex zombieModificado zombiesActuales
                in (partsAcum, zombiesActualizados)

checkImpactoUnico :: GType.Particula -> [PType.Zombie] -> Maybe (Int, PType.Zombie)
checkImpactoUnico particula zombies = 
    go 0 zombies
  where
    parEnt = particula LMi.^. GType.parEnt
    parDmg = particula LMi.^. GType.parDmg

    go _ [] = Nothing
    go idx (z:zs) =
        let zEnt = z LMi.^. PType.zmbEnt
        in case detectarColisionFisica zEnt parEnt of
            Nothing  -> go (idx + 1) zs
            Just mtv -> 
                let 
                    empParaZ = parEnt LMi.^. GType.entEmp . GType.empFrz 
                    empParaP = 0.0

                    dmgParaZ = parDmg
                    dmgParaP = 0.0

                    (zEntFisica, _) = aplicarEmpujeMutuo empParaZ empParaP mtv zEnt parEnt
                    (zEntFinal, _)  = aplicarDanoMutuo dmgParaZ dmgParaP zEntFisica parEnt

                    zombieFinal = z 
                        LMi.& PType.zmbEnt LMi..~ zEntFinal
                        LMi.& PType.zmbVerJug LMi..~ True
                        LMi.& PType.zmbHitTimer LMi..~ 2.0

                in Just (idx, zombieFinal)

detectarColisionFisica :: GType.Entidad -> GType.Entidad -> Maybe (SDL.V2 Float)
detectarColisionFisica entA entB =
    let boxA = entA LMi.^. GType.entBox
        boxB = entB LMi.^. GType.entBox
    in FC.mtvBoxes boxA boxB

aplicarEmpujeMutuo :: Float -> Float -> SDL.V2 Float -> GType.Entidad -> GType.Entidad -> (GType.Entidad, GType.Entidad)
aplicarEmpujeMutuo empParaA empParaB mtv entA entB =
    let 
        dir = LMe.normalize mtv
        vecParaA = dir LVe.^* empParaA
        vecParaB = (dir LVe.^* (-1)) LVe.^* empParaB

        entANueva = entA LMi.& GType.entEmp . GType.empVec LMi..~ vecParaA
        entBNueva = entB LMi.& GType.entEmp . GType.empVec LMi..~ vecParaB
    in
        (entANueva, entBNueva)

aplicarDanoMutuo :: Float -> Float -> GType.Entidad -> GType.Entidad -> (GType.Entidad, GType.Entidad)
aplicarDanoMutuo dmgParaA dmgParaB entA entB =
    let 
        entAMod = if dmgParaA > 0 
                  then entA LMi.& GType.entVid . GType.vidAct LMi.%~ (\v -> v - dmgParaA)
                  else entA
                  
        entBMod = if dmgParaB > 0
                  then entB LMi.& GType.entVid . GType.vidAct LMi.%~ (\v -> v - dmgParaB)
                  else entB
    in 
        (entAMod, entBMod)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 newElement (_:xs) = newElement : xs
replaceAt n newElement (x:xs) = x : replaceAt (n - 1) newElement xs
