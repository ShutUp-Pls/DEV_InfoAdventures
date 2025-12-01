module Objetos.Spawner where
-- Módulos del sistema
import qualified SDL
import qualified System.Random  as SR
import qualified Lens.Micro     as LMi
-- Módulos propios
import qualified Objetos.Types      as OType
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
import qualified Graficos.Dibujado  as GD

crearSpawner :: SDL.V2 Float -> Float -> OType.SpawnType -> (Float, Float) -> OType.Spawner
crearSpawner pos radio tipo (tMin, tMax) = OType.Spawner
    { OType._spaBox = GType.Box
        { GType._boxPos = pos
        , GType._boxTam = SDL.V2 32 32
        , GType._boxAng = 0
        , GType._boxRad = 16
        }
    , OType._areaSpawn    = radio
    , OType._tipoSpawn    = tipo
    , OType._rangoTiempo  = (tMin, tMax)
    , OType._tiempoActual = tMin
    }

posicionAleatoria :: SDL.V2 Float -> Float -> SR.StdGen -> (SDL.V2 Float, SR.StdGen)
posicionAleatoria centro radio gen0 =
    let (u, gen1) = SR.randomR (0.0, 1.0 :: Float) gen0
        (v, gen2) = SR.randomR (0.0, 2 * pi :: Float) gen1
        r = radio * sqrt u
        x = r * cos v
        y = r * sin v
    in (centro + SDL.V2 x y, gen2)

actualizarSpawners :: Float -> SR.StdGen -> [OType.Spawner] -> ([OType.Spawner], [PType.Zombie], [GType.Item], SR.StdGen)
actualizarSpawners dt genInicial listaSpawners = 
    foldr procesar ([], [], [], genInicial) listaSpawners
  where
    procesar spawner (sAcc, eAcc, iAcc, genActual) =
        let nuevoTiempo = (spawner LMi.^. OType.tiempoActual) - dt
        in if nuevoTiempo <= 0
           then 
               let 
                   posCentro = spawner LMi.^. OType.spaBox . GType.boxPos
                   radio     = spawner LMi.^. OType.areaSpawn
                   (posSpawn, gen1) = posicionAleatoria posCentro radio genActual
                   
                   (tMin, tMax) = spawner LMi.^. OType.rangoTiempo
                   (proxTiempo, gen2) = SR.randomR (tMin, tMax) gen1

                   spawnerReiniciado = spawner LMi.& OType.tiempoActual LMi..~ proxTiempo
                   (nuevosEnemigos, nuevosItems) = case spawner LMi.^. OType.tipoSpawn of

                       OType.SpawnEnemigo enemigoModelo -> 
                           let enemigoFinal = enemigoModelo 
                                    LMi.& PType.zmbEnt . GType.entBox . GType.boxPos LMi..~ posSpawn
                           in ([enemigoFinal], [])

                       OType.SpawnItem itemModelo -> 
                           let newItem = itemModelo 
                                    LMi.& GType.iteBox . GType.boxPos LMi..~ posSpawn
                                    LMi.& GType.iteAct LMi..~ True
                           in ([], [newItem])

               in (spawnerReiniciado : sAcc, nuevosEnemigos ++ eAcc, nuevosItems ++ iAcc, gen2)
           else 
               let spawnerActualizado = spawner LMi.& OType.tiempoActual LMi..~ nuevoTiempo
               in (spawnerActualizado : sAcc, eAcc, iAcc, genActual)

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> OType.Spawner -> IO ()
dibujar renderer texture camPos zoom spawner = do
    let pos = spawner LMi.^. OType.spaBox . GType.boxPos
    let tam = spawner LMi.^. OType.spaBox . GType.boxTam
    let ang = spawner LMi.^. OType.spaBox . GType.boxAng
    
    GD.dibujarTextura renderer texture camPos zoom pos tam ang (SDL.V4 128 0 128 255)