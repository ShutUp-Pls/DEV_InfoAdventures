module Fisica.MovEnemigo where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Utils
import qualified Fisica.Colisiones as FC

friccion :: Float
friccion = 0.90

umbralParada :: Float
umbralParada = 0.5

calcularDirEnemigo :: Types.Enemigo -> Types.Jugador -> SDL.V2 Float
calcularDirEnemigo enemigo player =
    let posE = Types.posEnemigo enemigo
        posJ = Types.posJugador player
        dist = LM.distance posE posJ
        rango = Types.rangoVision enemigo
    in if dist < rango
       then Utils.vectorHacia posE posJ (Types.velEnemigo enemigo)
       else SDL.V2 0 0

-- Las fisicas son, basicamente, pasar de un estado a otro
-- Una tarea ideal para una (monadeState) que controle esto
actFisicasMovEnemigo :: SDL.V2 Float -> [Types.Obstaculo] -> CMS.State Types.Enemigo ()
actFisicasMovEnemigo delta mapObstaculos = do
    enemigo <- CMS.get
    let esEmpujado = LM.norm (Types.velGolpeE enemigo) > umbralParada
        
        -- Decidimos vector de movimiento (Golpe o Delta IA)
        (SDL.V2 dx dy) = if esEmpujado 
                         then Types.velGolpeE enemigo
                         else delta

        -- Calculamos la fricción para el siguiente frame
        nuevoVelGolpe = if esEmpujado
                        then Types.velGolpeE enemigo LV.^* friccion
                        else SDL.V2 0 0
    
    -- Resolvemos X (Intentamos mover y revertimos si hay choque)
    posOriginal <- CMS.gets Types.posEnemigo
    CMS.modify $ \e -> e { Types.posEnemigo = posOriginal + SDL.V2 dx 0 }
    chocaX <- CMS.gets (`FC.checkColision` mapObstaculos)
    CMS.when chocaX $ CMS.modify $ \e -> e { Types.posEnemigo = posOriginal }

    -- Resolvemos Y (A partir de donde quedamos en X)
    posPostX <- CMS.gets Types.posEnemigo
    CMS.modify $ \e -> e { Types.posEnemigo = posPostX + SDL.V2 0 dy }
    chocaY <- CMS.gets (`FC.checkColision` mapObstaculos)
    CMS.when chocaY $ CMS.modify $ \e -> e { Types.posEnemigo = posPostX }

    -- Aplicamos las modificaciones
    CMS.modify $ \e -> e { Types.velGolpeE = nuevoVelGolpe }