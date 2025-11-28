module Fisica.Movimiento where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Control.Monad.State as CMS

-- Modulos propios
import qualified Types
import qualified Fisica.Colisiones as FC

friccion :: Float
friccion = 0.90
umbralParada :: Float
umbralParada = 0.5
rocePared :: Float
rocePared = 0.50

-- Esta función devuelve la velocidad final resultante para que el caller haga lo que quiera con ella
resolverFisica :: (Types.EntidadFisica a) => SDL.V2 Float -> [Types.Obstaculo] -> CMS.State a (SDL.V2 Float)
resolverFisica velocidadInput mapObstaculos = do
    entidad <- CMS.get
    
    -- Selección de Velocidad
    let velGolpe = Types.getVelGolpe entidad
        esEmpujado = LM.norm velGolpe > umbralParada
        velocidadActual = if esEmpujado then velGolpe else velocidadInput

    -- Tentativa de Movimiento
    let posOriginal = Types.getPos entidad
    CMS.modify $ \e -> Types.setPos (posOriginal + velocidadActual) e

    -- Colisión y Slicing
    maybeMTV <- CMS.gets (`FC.checkColision` mapObstaculos)
    velocidadFinal <- case maybeMTV of
        Nothing -> return velocidadActual
        Just mtv -> do
            let posActual = posOriginal + velocidadActual
            let mtvCorregido = mtv + (LM.normalize mtv LV.^* 0.01)
            
            -- Push out
            CMS.modify $ \e -> Types.setPos (posActual + mtvCorregido) e
            
            -- Slide
            let normal = LM.normalize mtv
                proyeccion = velocidadActual `LM.dot` normal
            
            return $ if proyeccion < 0 
                     then (velocidadActual - (normal LV.^* proyeccion)) LV.^* rocePared
                     else velocidadActual LV.^* rocePared

    -- Fricción y Actualización de Golpe
    let nuevoVelGolpe = if esEmpujado
                        then velocidadFinal LV.^* friccion
                        else SDL.V2 0 0
    
    CMS.modify $ \e -> Types.setVelGolpe nuevoVelGolpe e
    return velocidadFinal