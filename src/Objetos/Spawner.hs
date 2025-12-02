module Objetos.Spawner where
-- Módulos del sistema
import qualified SDL
import qualified System.Random  as SR
import qualified Lens.Micro     as LMi
import qualified Linear.Vector  as LV
import qualified Linear.Metric  as LM
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
-- import qualified Graficos.Dibujado  as GD

crearSpawner :: SDL.V2 Float -> Float -> Types.SpawnType -> (Float, Float) -> Types.Spawner
crearSpawner pos radio tipo (tMin, tMax) = Types.Spawner
    { Types._spaBox = GType.Box
        { GType._boxPos = pos
        , GType._boxTam = SDL.V2 32 32
        , GType._boxAng = 0
        , GType._boxRad = 16
        }
    , Types._areaSpawn    = radio
    , Types._tipoSpawn    = tipo
    , Types._rangoTiempo  = (tMin, tMax)
    , Types._tiempoActual = tMin
    }

posicionAleatoria :: SDL.V2 Float -> Float -> SR.StdGen -> (SDL.V2 Float, SR.StdGen)
posicionAleatoria centro radio gen0 =
    let (u, gen1) = SR.randomR (-1.0, 1.0 :: Float) gen0
        (v, gen2) = SR.randomR (-1.0, 1.0 :: Float) gen1
        dirVector = SDL.V2 u v
        distCuadrada = (u*u) + (v*v)
        finalOffset = if distCuadrada > 1.0 || distCuadrada == 0
                      then (SDL.normalize dirVector) LV.^* (radio * 0.9)
                      else dirVector LV.^* radio
                      
    in (centro + finalOffset, gen2)

actualizarSpawners :: Float -> SR.StdGen -> [Types.Spawner] -> [GType.Item] -> ([Types.Spawner], [PType.Zombie], [GType.Item], SR.StdGen)
actualizarSpawners dt genInicial listaSpawners itemsExistentes = 
    foldr procesar ([], [], [], genInicial) listaSpawners
  where
    procesar spawner (sAcc, eAcc, iAcc, genActual) =
        let nuevoTiempo = (spawner LMi.^. Types.tiempoActual) - dt
        in if nuevoTiempo <= 0
           then 
               -- Lógica común de tiempo y RNG
               let (tMin, tMax) = spawner LMi.^. Types.rangoTiempo
                   (proxTiempo, genTemp) = SR.randomR (tMin, tMax) genActual
                   
                   -- Datos del spawner
                   posCentro = spawner LMi.^. Types.spaBox . GType.boxPos
                   radio     = spawner LMi.^. Types.areaSpawn

               in case spawner LMi.^. Types.tipoSpawn of
                    -- LOGICA NUEVA: Spawner Fijo
                    Types.SpawnItemFijo itemModelo ->
                        let 
                            estaOcupado = any (\it -> LM.distance (it LMi.^. GType.iteBox . GType.boxPos) posCentro < 20.0) itemsExistentes
                        in if estaOcupado
                           then 
                               let spawnerWait = spawner LMi.& Types.tiempoActual LMi..~ proxTiempo
                               in (spawnerWait : sAcc, eAcc, iAcc, genTemp)
                           else
                               let newItem = itemModelo 
                                        LMi.& GType.iteBox . GType.boxPos LMi..~ posCentro
                                        LMi.& GType.iteAct LMi..~ True
                                   spawnerReiniciado = spawner LMi.& Types.tiempoActual LMi..~ proxTiempo
                               in (spawnerReiniciado : sAcc, eAcc, newItem : iAcc, genTemp)
                    Types.SpawnEnemigo enemigoModelo -> 
                        let (posSpawn, genPos) = posicionAleatoria posCentro radio genTemp
                            enemigoFinal = enemigoModelo 
                                     LMi.& PType.zmbEnt . GType.entBox . GType.boxPos LMi..~ posSpawn
                            spawnerReiniciado = spawner LMi.& Types.tiempoActual LMi..~ proxTiempo
                        in (spawnerReiniciado : sAcc, enemigoFinal : eAcc, iAcc, genPos)

                    Types.SpawnItem itemModelo -> 
                        let (posSpawn, genPos) = posicionAleatoria posCentro radio genTemp
                            newItem = itemModelo 
                                     LMi.& GType.iteBox . GType.boxPos LMi..~ posSpawn
                                     LMi.& GType.iteAct LMi..~ True
                            spawnerReiniciado = spawner LMi.& Types.tiempoActual LMi..~ proxTiempo
                        in (spawnerReiniciado : sAcc, eAcc, newItem : iAcc, genPos)

           else 
               let spawnerActualizado = spawner LMi.& Types.tiempoActual LMi..~ nuevoTiempo
               in (spawnerActualizado : sAcc, eAcc, iAcc, genActual)
{-
dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> Types.Spawner -> IO ()
dibujar renderer texture camPos zoom spawner = do
    let pos = spawner LMi.^. Types.spaBox . GType.boxPos
    let tam = spawner LMi.^. Types.spaBox . GType.boxTam
    let ang = spawner LMi.^. Types.spaBox . GType.boxAng
    
    if GD.esVisible pos tam ang camPos zoom 
        then do
            GD.dibujarTextura renderer texture camPos zoom pos tam ang (SDL.V4 128 0 128 255)
        else return()
-}