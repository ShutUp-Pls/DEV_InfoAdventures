module Juego where

-- Módulos del sistema
import qualified SDL
import qualified Control.Monad.State    as CMS
import qualified Lens.Micro             as LMi
import qualified Lens.Micro.Mtl         as LMi
import qualified Control.Monad          as CMo
import qualified Data.List              as DL
import qualified Data.Maybe             as DM
import qualified System.Random          as SR
-- Módulos propios
import qualified Types
import qualified Globals.Types          as GType
import qualified Personajes.Types       as PType

import qualified Globals.Camara         as OC
import qualified Fisica.Colisiones      as FCol
import qualified Fisica.Combates        as FCom
import qualified Fisica.Disparo         as FD
import qualified Personajes.Jugador     as PJ
import qualified Personajes.Zombie      as PZ
import qualified Objetos.Arma           as OArma
import qualified Objetos.Buff           as OBuff
import qualified Objetos.Spawner        as OS
import qualified Objetos.Particula      as OPart

dt :: Float
dt = 0.016

actualizarJuego :: Types.Input -> CMS.State Types.GameState ()
actualizarJuego input = do
    esTutorial <- LMi.use Types.tutorialActivo
    
    if esTutorial 
        then actualizarTutorialM input
        else return ()

    actualizarSistemasGlobalesM

    vivo   <- verificarJugadorVivo
    tiempo <- LMi.use Types.tiempoJuego

    if vivo && tiempo > 0
        then ejecutarJugabilidad input
        else manejarFinDeJuego input


actualizarSistemasGlobalesM :: CMS.State Types.GameState ()
actualizarSistemasGlobalesM = do
    fase   <- LMi.use Types.faseTutorial
    vivo   <- verificarJugadorVivo
    tiempo <- LMi.use Types.tiempoJuego

    let relojActivo = vivo && case fase of
            Types.FaseFin       -> True
            Types.FaseSobrevive -> True
            Types.FaseNula      -> tiempo > 0
            _                   -> False

    CMo.when relojActivo $ Types.tiempoJuego LMi.%= (\t -> max 0 (t - dt))
    
    Types.cooldownUI  LMi.%= (\t -> max 0 (t - dt))

    CMo.when relojActivo $ Types.tiempoTotal LMi.+= dt
    
    rng      <- LMi.use Types.rng
    spawners <- LMi.use Types.spawners
    
    let procesarSpawners = relojActivo

    if procesarSpawners then do
        let (spawnersAct, nuevosEnemigos, nuevosItems, sigRng) = OS.actualizarSpawners dt rng spawners
        Types.spawners LMi..= spawnersAct
        Types.enemigos LMi.%= (++ nuevosEnemigos)
        Types.items    LMi.%= (++ nuevosItems)
        Types.rng      LMi..= sigRng
    else return ()

ejecutarJugabilidad :: Types.Input -> CMS.State Types.GameState ()
ejecutarJugabilidad input = do
    gestionarCambioArma input
    procesarBuffsM
    verificarColisionJugadorZombies
    vivo <- verificarJugadorVivo

    CMo.when vivo $ do
        moverJugadorM input
        gestionarItemsM
        procesarDisparoM input
        actualizarParticulasM
        moverZombiesM
        verificarColisionParticulasZombies
        limpiarZombiesMuertosM
        actualizarCamaraM input

manejarFinDeJuego :: Types.Input -> CMS.State Types.GameState ()
manejarFinDeJuego input = do
    let teclaRespawn = input LMi.^. Types.teclaRespawn
    CMo.when teclaRespawn reaparecerJugador

gestionarCambioArma :: Types.Input -> CMS.State Types.GameState ()
gestionarCambioArma input = do
    cooldown <- LMi.use Types.cooldownUI
    
    CMo.when (cooldown <= 0) $ do
        jugador <- LMi.use Types.jugador
        let siguiente = input LMi.^. Types.nextWeapon
        let anterior  = input LMi.^. Types.prevWeapon
        
        CMo.when (siguiente || anterior) $ do
            if siguiente 
                then Types.jugador LMi..= PJ.cambiarArmaSiguiente jugador
                else Types.jugador LMi..= PJ.cambiarArmaAnterior jugador
            Types.cooldownUI LMi..= 0.2

procesarBuffsM :: CMS.State Types.GameState ()
procesarBuffsM = do
    jugador <- LMi.use Types.jugador
    let jugadorActualizado = PJ.procesarBuffs dt jugador
    Types.jugador LMi..= jugadorActualizado

verificarColisionJugadorZombies :: CMS.State Types.GameState ()
verificarColisionJugadorZombies = do
    jugador  <- LMi.use Types.jugador
    enemigos <- LMi.use Types.enemigos
    
    let (jugadorGolpeado, enemigosPostCombate) = FCom.choqueJugadorZombies jugador enemigos
    
    Types.jugador  LMi..= jugadorGolpeado
    Types.enemigos LMi..= enemigosPostCombate

    vidaActual <- LMi.use (Types.jugador . PType.jugEnt . GType.entVid . GType.vidAct)
    CMo.when (vidaActual <= 0) detonarSecuenciaMuerte

moverJugadorM :: Types.Input -> CMS.State Types.GameState ()
moverJugadorM input = do
    jugador <- LMi.use Types.jugador
    mapa    <- LMi.use Types.mapa
    
    let jugadorMovido = PJ.moverJugadorP input jugador mapa

    Types.jugador LMi..= jugadorMovido

gestionarItemsM :: CMS.State Types.GameState ()
gestionarItemsM = do
    jugador <- LMi.use Types.jugador
    items   <- LMi.use Types.items
    
    let (jugadorFinal, itemsRestantes, tiempoGanado) = logicaGestionItems jugador items
    
    Types.jugador     LMi..= jugadorFinal
    Types.items       LMi..= itemsRestantes
    Types.tiempoJuego LMi.+= tiempoGanado

procesarDisparoM :: Types.Input -> CMS.State Types.GameState ()
procesarDisparoM input = do
    rng        <- LMi.use Types.rng
    jugador    <- LMi.use Types.jugador
    let disparando = input LMi.^. Types.disparar
    
    let (nuevasBalas, jugadorFinal, rngFinal) = FD.procesarDisparo dt disparando rng jugador
    
    Types.jugador    LMi..= jugadorFinal
    Types.particulas LMi.%= (++ nuevasBalas)
    Types.rng        LMi..= rngFinal

actualizarParticulasM :: CMS.State Types.GameState ()
actualizarParticulasM = do
    particulas <- LMi.use Types.particulas
    mapa       <- LMi.use Types.mapa
    let particulasVivas = OPart.actualizarParticulas dt mapa particulas
    Types.particulas LMi..= particulasVivas
moverZombiesM :: CMS.State Types.GameState ()
moverZombiesM = do
    mapa     <- LMi.use Types.mapa
    jugador  <- LMi.use Types.jugador
    enemigos <- LMi.use Types.enemigos
    
    let zombiesMovidos = PZ.moverZombies dt jugador enemigos mapa 
    Types.enemigos LMi..= zombiesMovidos

verificarColisionParticulasZombies :: CMS.State Types.GameState ()
verificarColisionParticulasZombies = do
    particulas <- LMi.use Types.particulas
    enemigos   <- LMi.use Types.enemigos
    
    let (particulasFinales, zombiesDañados) = FCom.choqueParticulasZombies particulas enemigos
    
    Types.particulas LMi..= particulasFinales
    Types.enemigos   LMi..= zombiesDañados

limpiarZombiesMuertosM :: CMS.State Types.GameState ()
limpiarZombiesMuertosM = do
    enemigosActuales <- LMi.use Types.enemigos
    rngActual        <- LMi.use Types.rng
    let (vivos, muertos) = DL.partition (\z -> (z LMi.^. PType.zmbEnt . GType.entVid . GType.vidAct) > 0) enemigosActuales
    if null muertos then return () else do
        let procesarMuerte (particulasAcum, rng) zombie = 
                let pos = zombie LMi.^. PType.zmbEnt . GType.entBox . GType.boxPos
                    (nuevasPart, nuevoRng) = OPart.generarSangre pos 15 rng
                in (particulasAcum ++ nuevasPart, nuevoRng)
        let (todasLasParticulas, rngFinal) = foldl procesarMuerte ([], rngActual) muertos

        Types.enemigos   LMi..= vivos
        Types.particulas LMi.%= (++ todasLasParticulas)
        Types.rng        LMi..= rngFinal

actualizarCamaraM :: Types.Input -> CMS.State Types.GameState ()
actualizarCamaraM input = do
    camara   <- LMi.use Types.camara
    jugador  <- LMi.use Types.jugador
    enemigos <- LMi.use Types.enemigos 
    fase     <- LMi.use Types.faseTutorial

    let controlesBloqueados = case fase of
            Types.FaseIntro  -> True
            Types.FaseArmas  -> True
            _                -> False
    
    let inputFiltrado = if controlesBloqueados 
                        then input 
                                LMi.& Types.zoomIn     LMi..~ False
                                LMi.& Types.zoomOut    LMi..~ False
                                LMi.& Types.increaseDZ LMi..~ False
                                LMi.& Types.decreaseDZ LMi..~ False
                        else input

    let camaraFinal = OC.actualizarCamara inputFiltrado jugador enemigos camara
    Types.camara LMi..= camaraFinal

verificarJugadorVivo :: CMS.State Types.GameState Bool
verificarJugadorVivo = do
    vida <- LMi.use (Types.jugador . PType.jugEnt . GType.entVid . GType.vidAct)
    return (vida > 0)

detonarSecuenciaMuerte :: CMS.State Types.GameState ()
detonarSecuenciaMuerte = do
    jugador <- LMi.use Types.jugador
    rng     <- LMi.use Types.rng
    
    let posMuerte = jugador LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
    let (nuevasParticulas, rngNuevo) = OPart.generarExplosion
                                            posMuerte 
                                            50 
                                            OPart.idParHumo
                                            (50.0, 150.0) 
                                            (1.0, 2.5) 
                                            (15.0, 40.0)
                                            rng
                                            
    Types.particulas LMi.%= (++ nuevasParticulas)
    Types.rng        LMi..= rngNuevo
    Types.jugador . PType.jugEnt . GType.entVid . GType.vidMrt LMi.+= 1

reaparecerJugador :: CMS.State Types.GameState ()
reaparecerJugador = do
    puntoSpawn <- LMi.use (Types.jugador . PType.spawnPoint)
    vidaMax    <- LMi.use (Types.jugador . PType.jugEnt . GType.entVid . GType.vidMax)

    Types.jugador . PType.jugEnt . GType.entVid . GType.vidAct LMi..= vidaMax
    Types.jugador . PType.jugEnt . GType.entBox . GType.boxPos LMi..= puntoSpawn
    Types.jugador . PType.jugEnt . GType.entEmp . GType.empVec LMi..= SDL.V2 0 0
    Types.jugador . PType.jugEnt . GType.entBuf LMi..= []

    Types.tiempoJuego LMi..= GType.tiempoInicial

logicaGestionItems :: PType.Jugador -> [GType.Item] -> (PType.Jugador, [GType.Item], Float)
logicaGestionItems jug items = 
    let boxJug = jug LMi.^. PType.jugEnt . GType.entBox
        itemsTocados = FCol.checkColisionsItems boxJug items

        getArmId :: GType.Item -> Maybe Int
        getArmId it = case it LMi.^. GType.iteTipo of
             GType.EsArma arma -> Just (arma LMi.^. GType.armID)
             _                 -> Nothing

        entidadJug = jug LMi.^. PType.jugEnt
        mano       = entidadJug LMi.^. GType.entHnd
        inv        = entidadJug LMi.^. GType.entInv
        
        idsPoseidos = DL.nub $ DM.catMaybes $ map getArmId (mano : inv)

        esItemUtil :: GType.Item -> Bool
        esItemUtil it = case getArmId it of
            Just aid -> not (aid `elem` idsPoseidos)
            Nothing  -> True

        (itemsRecogibles, itemsRechazados) = DL.partition esItemUtil itemsTocados

        itemsNoTocados = filter (\it -> not (it `elem` itemsTocados)) items
        itemsRestantes = itemsNoTocados ++ itemsRechazados

        (itemsTiempo, itemsOtros) = DL.partition OBuff.esItemTiempo itemsRecogibles
        
        tiempoTotal = sum $ map (\it -> 
            case it LMi.^. GType.iteTipo of 
                 GType.EsBuff b -> b LMi.^. GType.bufVlr
                 _ -> 0
            ) itemsTiempo
            
        (equipables, consumibles) = DL.partition (\it -> it LMi.^. GType.iteInv) itemsOtros
        
        jugConBuffs = foldr (\it j -> PJ.aplicarEfecto it j) jug consumibles
        jugFinal    = foldr PJ.equiparItem jugConBuffs equipables

    in (jugFinal, itemsRestantes, tiempoTotal)

actualizarTutorialM :: Types.Input -> CMS.State Types.GameState ()
actualizarTutorialM input = do
    fase  <- LMi.use Types.faseTutorial
    timer <- LMi.use Types.timerTutorial
    jug   <- LMi.use Types.jugador
    rng   <- LMi.use Types.rng
    
    Types.timerTutorial LMi.+= dt

    let next = (input LMi.^. Types.espacio) && (timer > 0.5)

    case fase of
        Types.FaseIntro -> do
            CMo.when next $ do
                Types.faseTutorial LMi..= Types.FaseArmas
                Types.timerTutorial LMi..= 0
                let arma1 = OArma.crearItemArma OArma.idArmGlock    (SDL.V2 0 0)
                let arma2 = OArma.crearItemArma OArma.idArmEscopeta (SDL.V2 0 0)
                Types.jugador LMi..= PJ.equiparItem arma1 jug
                newJug <- LMi.use Types.jugador
                Types.jugador LMi..= PJ.equiparItem arma2 newJug

        Types.FaseArmas -> do
            CMo.when next $ do
                Types.faseTutorial LMi..= Types.FaseCamara
                Types.timerTutorial LMi..= 0

        Types.FaseCamara -> do
            CMo.when next $ do
                Types.faseTutorial LMi..= Types.FaseZombieMsg
                Types.timerTutorial LMi..= 0
                cam <- LMi.use Types.camara

                let posJug = jug LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
                let zoom   = cam LMi.^. GType.zoomLevel
                let safeDist = (600.0 / zoom) / 2.0 + 100.0

                let idsZombies = [ PZ.idZmbCorredor
                                 , PZ.idZmbTanque
                                 , PZ.idZmbBasico
                                 , PZ.idZmbBasico
                                 , PZ.idZmbBasico
                                 ]

                let generarZombie (r, listaZ) zId = 
                        let (ang, r1)  = SR.randomR (0.0, 2 * pi) r
                            (dist, r2) = SR.randomR (safeDist, safeDist + 400.0) r1
                            offset     = SDL.V2 (cos ang * dist) (sin ang * dist)
                            
                            zombieBase = PZ.crearZombie zId (posJug + offset)
                            zombieFinal = zombieBase 
                                            LMi.& PType.zmbRadVis LMi..~ 10000.0
                        in (r2, zombieFinal : listaZ)

                let (rngFinal, zombiesGenerados) = foldl generarZombie (rng, []) idsZombies

                Types.rng LMi..= rngFinal
                Types.enemigos LMi.%= (++ zombiesGenerados)

        Types.FaseZombieMsg -> do
            time    <- LMi.use Types.timerTutorial
            enemigos <- LMi.use Types.enemigos
            
            let pasoTiempo    = time > 10.0
                murioZombie   = length enemigos < 5
                debeFinalizar = pasoTiempo || murioZombie

            CMo.when debeFinalizar $ do
                Types.faseTutorial LMi..= Types.FaseCombate
                Types.timerTutorial LMi..= 0

        Types.FaseCombate -> do
                Types.faseTutorial LMi..= Types.FaseBuffsMsg
                Types.timerTutorial LMi..= 0
                
                let posJug = jug LMi.^. PType.jugEnt . GType.entBox . GType.boxPos
                let (rx1, r1) = SR.randomR (-300.0, 300.0) rng
                let (ry1, r2) = SR.randomR (-300.0, 300.0) r1
                let (rx2, r3) = SR.randomR (-300.0, 300.0) r2
                let (ry2, r4) = SR.randomR (-300.0, 300.0) r3

                Types.rng LMi..= r4

                let bVida = OBuff.crearItemBuff OBuff.idBuffVidA (posJug + SDL.V2 rx1 ry1)
                let bVel  = OBuff.crearItemBuff OBuff.idBuffVelA (posJug + SDL.V2 rx2 ry2)
                Types.items LMi.%= (++ [bVida, bVel])

                Types.spawners LMi.%= map (\sp -> sp { Types._tiempoActual = 0 })

        Types.FaseBuffsMsg -> do
            CMo.when next $ do
                Types.faseTutorial LMi..= Types.FaseTiempoMsg
                Types.timerTutorial LMi..= 0
                let posJug = jug LMi.^. PType.jugEnt . GType.entBox . GType.boxPos

                let (rx, r1) = SR.randomR (-500.0, 500.0) rng
                let (ry, r2) = SR.randomR (-500.0, 500.0) r1
                Types.rng LMi..= r2
                
                let bTiempo = OBuff.crearItemBuff OBuff.idBuffTiempo (posJug + SDL.V2 rx ry)
                Types.items LMi.%= (++ [bTiempo])

        Types.FaseTiempoMsg -> do
            CMo.when next $ do
                Types.faseTutorial LMi..= Types.FaseSobrevive
                Types.timerTutorial LMi..= 0

        Types.FaseSobrevive -> do
            CMo.when (timer > 2.0) $ do
                Types.faseTutorial LMi..= Types.FaseFin
        _ -> return ()