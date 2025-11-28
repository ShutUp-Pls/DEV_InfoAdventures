{-# LANGUAGE OverloadedStrings #-}
module Objetos.Items where

-- Modulos del sistema
import qualified SDL
import qualified Data.List as DL

-- Modulos propios
import qualified Types
import qualified Graficos.Dibujado as GD

-- Helper para gestionar la lista de buffs
agregarBuff :: Types.Buff -> Bool -> [Types.Buff] -> [Types.Buff]
agregarBuff nuevoBuff acumulaTiempo listaActual =
    let (existentes, otros) = DL.partition (\b -> Types.buffEtiqueta b == Types.buffEtiqueta nuevoBuff) listaActual
    in case existentes of
        [] -> nuevoBuff : listaActual
        (b:_) -> 
            let nuevoTiempo = if acumulaTiempo 
                              then Types.buffTiempo b + Types.buffTiempo nuevoBuff
                              else max (Types.buffTiempo b) (Types.buffTiempo nuevoBuff)
                buffActualizado = b { Types.buffTiempo = nuevoTiempo, Types.buffValor = Types.buffValor nuevoBuff }
            in buffActualizado : otros

aplicarEfecto :: Types.TipoItem -> Types.Jugador -> Types.Jugador
aplicarEfecto tipo j = case tipo of
    Types.Vida cantidad -> 
        j { Types.vidJugador = min 100 (Types.vidJugador j + cantidad) }
        
    Types.Velocidad factor duracion seAcumula -> 
        let nuevoBuff = Types.Buff 
                { Types.buffNombre = "Velocidad"
                , Types.buffEtiqueta = "SPEED"
                , Types.buffTiempo = duracion
                , Types.buffValor = factor 
                }
            nuevosBuffs = agregarBuff nuevoBuff seAcumula (Types.buffsActivos j)
        in j { Types.buffsActivos = nuevosBuffs }

    Types.Puntos _ -> j

procesarBuffs :: Float -> Types.Jugador -> Types.Jugador
procesarBuffs dt jug = 
    let 
        buffsRestantes = map (\b -> b { Types.buffTiempo = Types.buffTiempo b - dt }) (Types.buffsActivos jug)
        buffsVivos = filter (\b -> Types.buffTiempo b > 0) buffsRestantes

        velBuff = filter (\b -> Types.buffEtiqueta b == "SPEED") buffsVivos
        nuevoFactor = case velBuff of
            [] -> 1.0
            (b:_) -> Types.buffValor b
            
    in jug { Types.buffsActivos = buffsVivos, Types.velFactorJ = nuevoFactor }

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Types.Item -> IO ()
dibujar renderer texture camPos item = do
    let posI = Types.posItem item
    let tamI = Types.tamItem item
    let angI = Types.angItem item 
    
    -- La lógica de color vive aquí, donde pertenece
    let color = case Types.tipoItem item of
            Types.Vida _       -> SDL.V3 255 0 0     -- Rojo
            Types.Velocidad {} -> SDL.V3 0 0 255     -- Azul
            Types.Puntos _     -> SDL.V3 255 255 0   -- Amarillo

    GD.dibujarTextura renderer texture camPos posI tamI angI color