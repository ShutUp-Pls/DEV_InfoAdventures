module Fisica.Movimiento where

-- Módulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Control.Monad.State as CMS

-- Módulos propios
import qualified Types
import qualified Fisica.Colisiones as FC

-- (0.0 a 1.0) Conservación de velocidad tras un empuje (frenado)
factorFriccion :: Float
factorFriccion = 0.90      
-- Velocidad mínima: por debajo de esto, se considera detenido (0)
umbralParada :: Float
umbralParada = 0.5         
-- (0.0 a 1.0) Velocidad conservada al raspar una pared (Sliding)
factorRocePared :: Float
factorRocePared = 0.50     
-- Pequeño extra al resolver colisión para evitar errores de float (epsilon)
margenAntiAtasco :: Float
margenAntiAtasco = 0.01    

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
            
            -- Aplicamos corrección con el margen de seguridad extra
            let mtvCorregido = mtv + (LM.normalize mtv LV.^* margenAntiAtasco)
            
            -- Push out (Sacar al objeto del obstáculo)
            CMS.modify $ \e -> Types.setPos (posActual + mtvCorregido) e
            
            -- Slide (Deslizar contra la pared)
            let normal = LM.normalize mtv
                proyeccion = velocidadActual `LM.dot` normal
            
            return $ if proyeccion < 0 
                     then (velocidadActual - (normal LV.^* proyeccion)) LV.^* factorRocePared
                     else velocidadActual LV.^* factorRocePared

    -- Fricción y Actualización de Golpe
    let nuevoVelGolpe = if esEmpujado
                        then velocidadFinal LV.^* factorFriccion
                        else SDL.V2 0 0
    
    CMS.modify $ \e -> Types.setVelGolpe nuevoVelGolpe e
    return velocidadFinal