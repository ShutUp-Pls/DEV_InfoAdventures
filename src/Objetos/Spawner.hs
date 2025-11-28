module Objetos.Spawner where

-- Módulos del sistema
import qualified SDL
import qualified System.Random as SR

-- Módulos propios
import qualified Types
import qualified Graficos.Dibujado as GD

-- Función para crear un Spawner nuevo
crearSpawner :: SDL.V2 Float -> Float -> Types.SpawnType -> (Float, Float) -> Types.Spawner
crearSpawner pos radio tipo (tMin, tMax) = Types.Spawner
    { Types.posSpawner   = pos
    , Types.tamSpawner   = SDL.V2 32 32
    , Types.angSpawner   = 0
    , Types.areaSpawn    = radio
    , Types.tipoSpawn    = tipo
    , Types.rangoTiempo  = (tMin, tMax)
    , Types.tiempoActual = tMin
    }

-- Lógica para obtener una posición aleatoria dentro del radio
-- Usa coordenadas polares para distribución uniforme en el círculo
posicionAleatoria :: SDL.V2 Float -> Float -> SR.StdGen -> (SDL.V2 Float, SR.StdGen)
posicionAleatoria centro radio gen0 =
    let (u, gen1) = SR.randomR (0.0, 1.0 :: Float) gen0
        (v, gen2) = SR.randomR (0.0, 2 * pi :: Float) gen1
        r = radio * sqrt u -- sqrt para distribución uniforme de área
        x = r * cos v
        y = r * sin v
    in (centro + SDL.V2 x y, gen2)

-- Función principal que actualiza todos los spawners
actualizarSpawners :: Float -> SR.StdGen -> [Types.Spawner] -> ([Types.Spawner], [Types.Enemigo], [Types.Item], SR.StdGen)
actualizarSpawners dt genInicial listaSpawners = 
    foldr procesar ([], [], [], genInicial) listaSpawners
  where
    procesar spawner (sAcc, eAcc, iAcc, genActual) =
        let nuevoTiempo = Types.tiempoActual spawner - dt
        in if nuevoTiempo <= 0
           then 
               -- TIEMPO DE SPAWNEAR
               let (posSpawn, gen1) = posicionAleatoria (Types.posSpawner spawner) (Types.areaSpawn spawner) genActual
                   (tMin, tMax) = Types.rangoTiempo spawner
                   (proxTiempo, gen2) = SR.randomR (tMin, tMax) gen1
                   
                   spawnerReiniciado = spawner { Types.tiempoActual = proxTiempo }
                   
                   -- Generamos la entidad según el tipo
                   (nuevosEnemigos, nuevosItems) = case Types.tipoSpawn spawner of
                       -- AHORA: Capturamos el 'enemigoModelo' que viene dentro del constructor
                       Types.SpawnEnemigo enemigoModelo -> 
                           -- Usamos el modelo base, pero sobrescribimos su posición con la aleatoria (posSpawn)
                           let enemigoFinal = enemigoModelo { Types.posEnemigo = posSpawn }
                           in ([enemigoFinal], [])
                       
                       Types.SpawnItem tItem -> 
                           ([], [Types.Item posSpawn (SDL.V2 20 20) 0 tItem True])

               in (spawnerReiniciado : sAcc, nuevosEnemigos ++ eAcc, nuevosItems ++ iAcc, gen2)
           else 
               -- Solo actualizar tiempo
               (spawner { Types.tiempoActual = nuevoTiempo } : sAcc, eAcc, iAcc, genActual)

-- Renderizado (Visualización Debug o Editor)
dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> Types.Spawner -> IO ()
dibujar renderer texture camPos zoom spawner = do
    let pos = Types.posSpawner spawner
    let tam = Types.tamSpawner spawner
    let ang = Types.angSpawner spawner
    
    GD.dibujarTextura renderer texture camPos zoom pos tam ang (SDL.V3 128 0 128)