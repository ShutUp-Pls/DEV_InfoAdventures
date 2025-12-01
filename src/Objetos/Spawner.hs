module Objetos.Spawner where
-- Módulos del sistema
import qualified SDL
import qualified System.Random  as SR
import qualified Lens.Micro     as LMi
-- Módulos propios
import qualified Types
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
import qualified Graficos.Dibujado  as GD

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
    let (u, gen1) = SR.randomR (0.0, 1.0 :: Float) gen0
        (v, gen2) = SR.randomR (0.0, 2 * pi :: Float) gen1
        r = radio * sqrt u
        x = r * cos v
        y = r * sin v
    in (centro + SDL.V2 x y, gen2)

actualizarSpawners :: Float -> SR.StdGen -> [Types.Spawner] -> ([Types.Spawner], [PType.Zombie], [GType.Item], SR.StdGen)
actualizarSpawners dt genInicial listaSpawners = 
    foldr procesar ([], [], [], genInicial) listaSpawners
  where
    procesar spawner (sAcc, eAcc, iAcc, genActual) =
        let nuevoTiempo = (spawner LMi.^. Types.tiempoActual) - dt
        in if nuevoTiempo <= 0
           then 
               let 
                   posCentro = spawner LMi.^. Types.spaBox . GType.boxPos
                   radio     = spawner LMi.^. Types.areaSpawn
                   (posSpawn, gen1) = posicionAleatoria posCentro radio genActual
                   
                   (tMin, tMax) = spawner LMi.^. Types.rangoTiempo
                   (proxTiempo, gen2) = SR.randomR (tMin, tMax) gen1

                   spawnerReiniciado = spawner LMi.& Types.tiempoActual LMi..~ proxTiempo
                   (nuevosEnemigos, nuevosItems) = case spawner LMi.^. Types.tipoSpawn of

                       Types.SpawnEnemigo enemigoModelo -> 
                           let enemigoFinal = enemigoModelo 
                                    LMi.& PType.zmbEnt . GType.entBox . GType.boxPos LMi..~ posSpawn
                           in ([enemigoFinal], [])

                       Types.SpawnItem itemModelo -> 
                           let newItem = itemModelo 
                                    LMi.& GType.iteBox . GType.boxPos LMi..~ posSpawn
                                    LMi.& GType.iteAct LMi..~ True
                           in ([], [newItem])

               in (spawnerReiniciado : sAcc, nuevosEnemigos ++ eAcc, nuevosItems ++ iAcc, gen2)
           else 
               let spawnerActualizado = spawner LMi.& Types.tiempoActual LMi..~ nuevoTiempo
               in (spawnerActualizado : sAcc, eAcc, iAcc, genActual)

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> Types.Spawner -> IO ()
dibujar renderer texture camPos zoom spawner = do
    let pos = spawner LMi.^. Types.spaBox . GType.boxPos
    let tam = spawner LMi.^. Types.spaBox . GType.boxTam
    let ang = spawner LMi.^. Types.spaBox . GType.boxAng
    
    GD.dibujarTextura renderer texture camPos zoom pos tam ang (SDL.V4 128 0 128 255)