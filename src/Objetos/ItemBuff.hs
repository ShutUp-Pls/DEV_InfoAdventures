{-# LANGUAGE OverloadedStrings #-}
module Objetos.ItemBuff where
-- Modulos del sistema
import qualified SDL
import qualified Data.List  as DL
import qualified Lens.Micro as LMi
-- Modulos propios
import qualified Globals.Types    as GType
import qualified Personajes.Types as PType
import qualified Objetos.Types    as OType
import qualified Graficos.Dibujado as GD

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

aplicarEfecto :: OType.ItemBuff -> PType.Jugador -> PType.Jugador
aplicarEfecto item j = 
    let 
        buffOriginal = item LMi.^. OType.iteBuf
        nombreItem   = item LMi.^. OType.iteNom
        buffFinal    = buffOriginal LMi.& GType.bufNom LMi..~ nombreItem
        bid  = buffFinal LMi.^. GType.bufID
        val  = buffFinal LMi.^. GType.bufVlr
    in 
        if bid == OType.idBuffVid
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
        buffsVel = filter (\b -> (b LMi.^. GType.bufID) == OType.idBuffVel) buffsVivos
        factorVel = case buffsVel of
            [] -> 1.0
            (b:_) -> b LMi.^. GType.bufVlr
    in jug 
        LMi.& PType.jugEnt . GType.entBuf LMi..~ buffsVivos
        LMi.& PType.jugEnt . GType.entMov . GType.movFac LMi..~ factorVel

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> OType.ItemBuff -> IO ()
dibujar renderer texture camPos zoom item = do
    let posI = item LMi.^. OType.iteBox . GType.boxPos
    let tamI = item LMi.^. OType.iteBox . GType.boxTam
    let angI = item LMi.^. OType.iteBox . GType.boxAng
    let bid  = item LMi.^. OType.iteBuf . GType.bufID
    
    let color = if bid == OType.idBuffVid
                then SDL.V4 255 0 0 255
                else SDL.V4 0 0 255 255

    GD.dibujarTextura renderer texture camPos zoom posI tamI angI color