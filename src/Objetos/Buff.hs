{-# LANGUAGE OverloadedStrings #-}
module Objetos.Buff where
-- Modulos del sistema
import qualified SDL
import qualified Data.List          as DL
import qualified Data.Text          as DT
import qualified Lens.Micro         as LMi
-- Modulos propios
import qualified Globals.Types      as GType
import qualified Personajes.Types   as PType
import qualified Graficos.Dibujado  as GD

idBuffVelA :: Int
idBuffVelA = 1
idItemBuffVelA  :: Int
idItemBuffVelA  = 1001
nomItemBuffVelA :: DT.Text
nomItemBuffVelA = "Bebida energetica"

idBuffVidA :: Int
idBuffVidA = 2
idItemBuffVidA :: Int
idItemBuffVidA = 1002
nomItemBuffVidA :: DT.Text
nomItemBuffVidA = "Pocion de Vida I"

crearStatsBuffVidA :: GType.Buff
crearStatsBuffVidA = GType.Buff 
    { GType._bufID  = idBuffVidA     
    , GType._bufTmp = 0.0
    , GType._bufVlr = 10.0
    , GType._bufNom = ""
    }

crearBoxBuffVidA :: SDL.V2 Float -> GType.Box
crearBoxBuffVidA pos = GType.Box 
    { GType._boxPos = pos
    , GType._boxTam = SDL.V2 20 20
    , GType._boxAng = 0.0
    , GType._boxRad = 10.0
    }

crearItemBuffVidA :: SDL.V2 Float -> GType.Item
crearItemBuffVidA pos = GType.Item
    { GType._iteId  = idItemBuffVidA
    , GType._iteNom = nomItemBuffVidA
    , GType._iteTipo = GType.EsBuff crearStatsBuffVidA
    , GType._iteBox  = crearBoxBuffVidA pos
    , GType._iteInv = False
    , GType._iteAct = True 
    }

crearStatsBuffVelA :: GType.Buff
crearStatsBuffVelA = GType.Buff 
    { GType._bufID  = idBuffVelA     
    , GType._bufTmp = 10.0
    , GType._bufVlr = 1.5
    , GType._bufNom = ""
    }

crearItemBuffVelA :: SDL.V2 Float -> GType.Item
crearItemBuffVelA pos = GType.Item
    { GType._iteId  = idItemBuffVelA
    , GType._iteNom = nomItemBuffVelA
    , GType._iteTipo = GType.EsBuff crearStatsBuffVelA
    , GType._iteBox  = crearBoxBuffVidA pos
    , GType._iteInv = False
    , GType._iteAct = True 
    }


agregarBuff :: GType.Buff -> [GType.Buff] -> [GType.Buff]
agregarBuff nuevoBuff listaActual =
    let (existentes, otros) = DL.partition (\b -> (b LMi.^. GType.bufID) == (nuevoBuff LMi.^. GType.bufID)) listaActual
    in case existentes of
        [] -> nuevoBuff : listaActual
        (b:_) -> 
            let tActual = b LMi.^. GType.bufTmp
                tNuevo  = nuevoBuff LMi.^. GType.bufTmp
                vActual = b LMi.^. GType.bufVlr
                vNuevo  = nuevoBuff LMi.^. GType.bufVlr
                nomNuevo = nuevoBuff LMi.^. GType.bufNom 
                buffActualizado = b LMi.& GType.bufTmp LMi..~ (max tActual tNuevo)
                                    LMi.& GType.bufVlr LMi..~ (max vActual vNuevo)
                                    LMi.& GType.bufNom LMi..~ nomNuevo
            in buffActualizado : otros

aplicarEfecto :: GType.Item -> PType.Jugador -> PType.Jugador
aplicarEfecto item j = 
    let maybeBuff = item LMi.^? GType.iteTipo . GType._EsBuff
    in case maybeBuff of
        Nothing -> j
        Just buffOriginal ->
            let 
                nombreItem = item LMi.^. GType.iteNom
                buffFinal  = buffOriginal LMi.& GType.bufNom LMi..~ nombreItem
                
                bid = buffFinal LMi.^. GType.bufID
                val = buffFinal LMi.^. GType.bufVlr
            in 
                if bid == idBuffVidA
                then 
                    j LMi.& PType.jugEnt . GType.entVid . GType.vidAct LMi.%~ (\v -> min 100 (v + val))
                else 
                    let currentBuffs = j LMi.^. PType.jugEnt . GType.entBuf
                        nuevosBuffs  = agregarBuff buffFinal currentBuffs
                    in j LMi.& PType.jugEnt . GType.entBuf LMi..~ nuevosBuffs

procesarBuffs :: Float -> PType.Jugador -> PType.Jugador
procesarBuffs dt jug = 
    let 
        buffsRestantes = map (\b -> b LMi.& GType.bufTmp LMi.%~ (\t -> t - dt)) (jug LMi.^. PType.jugEnt . GType.entBuf)
        buffsVivos = filter (\b -> (b LMi.^. GType.bufTmp) > 0) buffsRestantes
        buffsVel = filter (\b -> (b LMi.^. GType.bufID) == idBuffVelA) buffsVivos
        factorVel = case buffsVel of
            [] -> 1.0
            (b:_) -> b LMi.^. GType.bufVlr
    in jug 
        LMi.& PType.jugEnt . GType.entBuf LMi..~ buffsVivos
        LMi.& PType.jugEnt . GType.entMov . GType.movFac LMi..~ factorVel

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Item -> IO ()
dibujar renderer texture camPos zoom item = do
    let posI = item LMi.^. GType.iteBox . GType.boxPos
    let tamI = item LMi.^. GType.iteBox . GType.boxTam
    let angI = item LMi.^. GType.iteBox . GType.boxAng

    let maybeBuff = item LMi.^? GType.iteTipo . GType._EsBuff
    
    case maybeBuff of
        Nothing -> return ()
        Just buff -> do
            let bid = buff LMi.^. GType.bufID
            
            let color = if bid == idBuffVidA
                        then SDL.V4 255 0 0 255
                        else SDL.V4 0 0 255 255

            GD.dibujarTextura renderer texture camPos zoom posI tamI angI color

-- Constantes de Tiempo
tiempoInicial :: Float
tiempoInicial = 30.0 -- Empiezas con 30 segundos

tiempoMaximo :: Float
tiempoMaximo = 999.0

-- Definición del Buff/Item de Tiempo
idBuffTiempo :: Int
idBuffTiempo = 999 

idItemTiempo :: Int
idItemTiempo = 9999

valorRecarga :: Float
valorRecarga = 10.0 -- Segundos que otorga el item

nomItemTiempo :: DT.Text
nomItemTiempo = "Reloj de Arena"

-- Función para crear el Item (Buff) que aparecerá en el mapa
crearItemTiempo :: SDL.V2 Float -> GType.Item
crearItemTiempo pos = GType.Item
    { GType._iteId   = idItemTiempo
    , GType._iteNom  = nomItemTiempo
    , GType._iteTipo = GType.EsBuff statsBuffTiempo
    , GType._iteBox  = boxTiempo pos
    , GType._iteInv  = False -- Se consume al tocarlo (pickup)
    , GType._iteAct  = True
    }

statsBuffTiempo :: GType.Buff
statsBuffTiempo = GType.Buff 
    { GType._bufID  = idBuffTiempo     
    , GType._bufTmp = 0.0           -- No es temporal en el jugador
    , GType._bufVlr = valorRecarga  -- Valor a sumar al reloj global
    , GType._bufNom = "Tiempo Extra"
    }

boxTiempo :: SDL.V2 Float -> GType.Box
boxTiempo pos = GType.Box 
    { GType._boxPos = pos
    , GType._boxTam = SDL.V2 20 20
    , GType._boxAng = 0.0
    , GType._boxRad = 10.0
    }

esItemTiempo :: GType.Item -> Bool
esItemTiempo item = 
    case item LMi.^. GType.iteTipo of
        GType.EsBuff b -> (b LMi.^. GType.bufID) == idBuffTiempo
        _              -> False