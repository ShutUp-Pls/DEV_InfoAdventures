{-# LANGUAGE OverloadedStrings #-}
module Objetos.Buff where
-- Módulos del sistema
import qualified SDL
import qualified Data.List          as DL
import qualified Data.Text          as DT
import qualified Lens.Micro         as LMi
import qualified Linear.Vector      as LV
-- Módulos propios
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD


idBuffVelA, idBuffVidA, idBuffTiempo :: Int
idBuffVelA   = 1
idBuffVidA   = 2
idBuffTiempo = 999 

nomBuffVelA, nomBuffVidA, nomBuffTiempo :: DT.Text
nomBuffVelA   = "Bebida Energética"
nomBuffVidA   = "Poción de Vida"
nomBuffTiempo = "Reloj de Arena"

idBuffVelB, idBuffVelC, idBuffVidB, idBuffVidC, idBuffTiempoB, idBuffTiempoC :: Int
idBuffVelB    = 3
idBuffVelC    = 4
idBuffVidB    = 5
idBuffVidC    = 6
idBuffTiempoB = 998
idBuffTiempoC = 997

nomBuffVelB, nomBuffVelC, nomBuffVidB, nomBuffVidC, nomBuffTiempoB, nomBuffTiempoC :: DT.Text
nomBuffVelB    = "Bebida Energética Turbo"
nomBuffVelC    = "Bebida Energética Nuclear"
nomBuffVidB    = "Poción Mayor"
nomBuffVidC    = "Elixir Completo"
nomBuffTiempoB = "Reloj de Arena Místico"
nomBuffTiempoC = "Crono-Cristal"

idsVelocidad, idsVida, idsTiempo :: [Int]
idsVelocidad = [idBuffVelA, idBuffVelB, idBuffVelC]
idsVida      = [idBuffVidA, idBuffVidB, idBuffVidC]
idsTiempo    = [idBuffTiempo, idBuffTiempoB, idBuffTiempoC]

crearStatsBuff :: Int -> GType.Buff
crearStatsBuff bId
    | bId == idBuffVelA     = mkBuff idBuffVelA     15.0 1.5  nomBuffVelA
    | bId == idBuffVidA     = mkBuff idBuffVidA     0.0  25.0 nomBuffVidA
    | bId == idBuffTiempo   = mkBuff idBuffTiempo   0.0  10.0 nomBuffTiempo 
    | bId == idBuffVelB     = mkBuff idBuffVelB     10.0 2.0  nomBuffVelB
    | bId == idBuffVelC     = mkBuff idBuffVelC     7.5 2.5  nomBuffVelC
    | bId == idBuffVidB     = mkBuff idBuffVidB     0.0  50.0 nomBuffVidB
    | bId == idBuffVidC     = mkBuff idBuffVidC     0.0  100.0 nomBuffVidC
    | bId == idBuffTiempoB  = mkBuff idBuffTiempoB  0.0  20.0 nomBuffTiempoB
    | bId == idBuffTiempoC  = mkBuff idBuffTiempoC  0.0  30.0 nomBuffTiempoC
    | otherwise             = error $ "crearStatsBuff: ID desconocido " ++ show bId
  where
    mkBuff i t v n = GType.Buff 
        { GType._bufID = i, GType._bufTmp = t, GType._bufVlr = v, GType._bufNom = n }

crearBoxBuff :: Int -> SDL.V2 Float -> GType.Box
crearBoxBuff bId pos
    | bId == idBuffVelA     = mkBox pos 20.0 
    | bId == idBuffVidA     = mkBox pos 20.0 
    | bId == idBuffTiempo   = mkBox pos 20.0 
    | bId == idBuffVelB     = mkBox pos 20.0
    | bId == idBuffVelC     = mkBox pos 20.0
    | bId == idBuffVidB     = mkBox pos 20.0
    | bId == idBuffVidC     = mkBox pos 20.0
    | bId == idBuffTiempoB  = mkBox pos 20.0
    | bId == idBuffTiempoC  = mkBox pos 20.0
    | otherwise             = mkBox pos 20.0
  where
    mkBox p rad = GType.Box
        { GType._boxPos = p, GType._boxTam = SDL.V2 (rad*2) (rad*2), GType._boxAng = 0.0, GType._boxRad = rad }

crearItemBuff :: Int -> SDL.V2 Float -> GType.Item
crearItemBuff bId pos = 
    let (itemId, itemNom) = case bId of
            _ | bId == idBuffVelA       -> (idBuffVelA, nomBuffVelA)
              | bId == idBuffVidA       -> (idBuffVidA, nomBuffVidA)
              | bId == idBuffTiempo     -> (idBuffTiempo,   nomBuffTiempo)
              | bId == idBuffVelB       -> (idBuffVelB,    nomBuffVelB)
              | bId == idBuffVelC       -> (idBuffVelC,    nomBuffVelC)
              | bId == idBuffVidB       -> (idBuffVidB,    nomBuffVidB)
              | bId == idBuffVidC       -> (idBuffVidC,    nomBuffVidC)
              | bId == idBuffTiempoB    -> (idBuffTiempoB, nomBuffTiempoB)
              | bId == idBuffTiempoC    -> (idBuffTiempoC, nomBuffTiempoC)
              | otherwise               -> (0, "Desconocido")
        
        esInventario = False
        esActivo     = True
    in GType.Item
        { GType._iteId   = itemId
        , GType._iteNom  = itemNom
        , GType._iteTipo = GType.EsBuff (crearStatsBuff bId)
        , GType._iteBox  = crearBoxBuff bId pos
        , GType._iteInv  = esInventario
        , GType._iteAct  = esActivo 
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
                nomNew  = nuevoBuff LMi.^. GType.bufNom
                buffActualizado = b LMi.& GType.bufTmp LMi..~ (max tActual tNuevo)
                                    LMi.& GType.bufVlr LMi..~ (max vActual vNuevo)
                                    LMi.& GType.bufNom LMi..~ nomNew
            in buffActualizado : otros

esItemTiempo :: GType.Item -> Bool
esItemTiempo item = 
    case item LMi.^. GType.iteTipo of
        GType.EsBuff b -> (b LMi.^. GType.bufID) == idBuffTiempo
        _              -> False

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Item -> IO ()
dibujar renderer texture camPos zoom item = do
    let pos = item LMi.^. GType.iteBox . GType.boxPos
    let tam = item LMi.^. GType.iteBox . GType.boxTam
    let ang = item LMi.^. GType.iteBox . GType.boxAng

    if GD.esVisible pos tam ang camPos zoom 
        then do
            let maybeBuff = item LMi.^? GType.iteTipo . GType._EsBuff
            
            case maybeBuff of
                Nothing -> return ()
                Just buff -> do
                    let bid = buff LMi.^. GType.bufID
                    let partes = damePartesBuff bid
                    let maxW_Original = if null partes 
                                        then 1.0 
                                        else maximum $ map (\(p, s, _) -> (p LMi.^. SDL._x) + (s LMi.^. SDL._x)) partes

                    let targetW = tam LMi.^. SDL._x
                    let escala = if maxW_Original > 0 then targetW / maxW_Original else 1.0

                    mapM_ (\(offsetLocal, sizeLocal, color) -> do
                        let finalPos  = pos + (offsetLocal LV.^* escala)
                        let finalSize = sizeLocal LV.^* escala
                        GD.dibujarTextura renderer texture camPos zoom finalPos finalSize ang color
                        ) partes
        else return()

dibujarIcono :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> SDL.V2 Float -> Int -> IO ()
dibujarIcono renderer texture posHUD sizeHUD bId = do
    let partes = damePartesBuff bId
    if null partes then return () else do
        let maxX = maximum $ map (\(p, s, _) -> (p LMi.^. SDL._x) + (s LMi.^. SDL._x)) partes
        let maxY = maximum $ map (\(p, s, _) -> (p LMi.^. SDL._y) + (s LMi.^. SDL._y)) partes

        let padding    = 5.0
        let targetSize = sizeHUD - SDL.V2 padding padding
        let safeMaxX   = if maxX == 0 then 1 else maxX
        let safeMaxY   = if maxY == 0 then 1 else maxY
        
        let scaleX     = (targetSize LMi.^. SDL._x) / safeMaxX
        let scaleY     = (targetSize LMi.^. SDL._y) / safeMaxY
        let scale      = min scaleX scaleY

        let contentW = safeMaxX * scale
        let contentH = safeMaxY * scale

        let offsetX  = ((sizeHUD LMi.^. SDL._x) - contentW) / 2
        let offsetY  = ((sizeHUD LMi.^. SDL._y) - contentH) / 2
        let startPos = posHUD + SDL.V2 offsetX offsetY

        mapM_ (\(offsetLocal, sizeLocal, color) -> do
            let pFinal = startPos + (offsetLocal LV.^* scale)
            let sFinal = sizeLocal LV.^* scale
            GD.dibujarOverlay renderer texture pFinal sFinal color
            ) partes

damePartesBuff :: Int -> [GD.ParteDibujo]
damePartesBuff bId
    | bId == idBuffVelA = 
        [ (SDL.V2 2 4,     SDL.V2 26 42, SDL.V4 20 20 25 255)
        , (SDL.V2 2 2,     SDL.V2 26 3,  SDL.V4 192 192 192 255) 
        , (SDL.V2 2 45,    SDL.V2 26 3,  SDL.V4 192 192 192 255) 
        , (SDL.V2 8 15,    SDL.V2 14 20, SDL.V4 57 255 20 255)
        , (SDL.V2 10 0,    SDL.V2 10 4,  SDL.V4 160 160 160 255)
        ]
    | bId == idBuffVidA =
        [ (SDL.V2 5 15,    SDL.V2 30 25, SDL.V4 220 20 60 255)
        , (SDL.V2 12 5,    SDL.V2 16 10, SDL.V4 220 20 60 255)
        , (SDL.V2 10 3,    SDL.V2 20 4,  SDL.V4 200 200 255 150) 
        , (SDL.V2 13 0,    SDL.V2 14 4,  SDL.V4 139 69 19 255)
        , (SDL.V2 10 20,   SDL.V2 5 5,   SDL.V4 255 255 255 200) 
        , (SDL.V2 25 30,   SDL.V2 3 3,   SDL.V4 255 255 255 200) 
        ]
    | bId == idBuffTiempo = 
        [ (SDL.V2 0 0,     SDL.V2 40 6,  SDL.V4 101 67 33 255)
        , (SDL.V2 0 44,    SDL.V2 40 6,  SDL.V4 101 67 33 255)
        , (SDL.V2 2 6,     SDL.V2 4 38,  SDL.V4 101 67 33 255)
        , (SDL.V2 34 6,    SDL.V2 4 38,  SDL.V4 101 67 33 255)
        , (SDL.V2 8 6,     SDL.V2 24 10, SDL.V4 238 210 2 255)
        , (SDL.V2 12 16,   SDL.V2 16 8,  SDL.V4 238 210 2 255)
        , (SDL.V2 18 24,   SDL.V2 4 4,   SDL.V4 238 210 2 255)
        , (SDL.V2 12 28,   SDL.V2 16 6,  SDL.V4 180 220 240 50)
        , (SDL.V2 8 34,    SDL.V2 24 10, SDL.V4 238 210 2 255)
        ]
    | bId == idBuffVelB =
        [ (SDL.V2 2 4,     SDL.V2 26 42, SDL.V4 0 50 180 255)
        , (SDL.V2 2 2,     SDL.V2 26 3,  SDL.V4 255 165 0 255)
        , (SDL.V2 2 45,    SDL.V2 26 3,  SDL.V4 255 165 0 255)
        , (SDL.V2 8 15,    SDL.V2 14 20, SDL.V4 255 215 0 255)
        , (SDL.V2 10 0,    SDL.V2 10 4,  SDL.V4 200 200 200 255)
        ]
    | bId == idBuffVelC =
        [ (SDL.V2 2 4,     SDL.V2 26 42, SDL.V4 20 20 20 255)
        , (SDL.V2 2 2,     SDL.V2 26 3,  SDL.V4 255 0 0 255)
        , (SDL.V2 2 45,    SDL.V2 26 3,  SDL.V4 255 0 0 255)
        , (SDL.V2 8 15,    SDL.V2 14 20, SDL.V4 0 255 255 255)
        , (SDL.V2 10 0,    SDL.V2 10 4,  SDL.V4 100 100 100 255)
        ]
    | bId == idBuffVidB =
        [ (SDL.V2 4 12,    SDL.V2 32 30, SDL.V4 180 20 180 255)
        , (SDL.V2 11 5,    SDL.V2 18 10, SDL.V4 180 20 180 255)
        , (SDL.V2 9 3,     SDL.V2 22 4,  SDL.V4 220 220 255 150)
        , (SDL.V2 12 0,    SDL.V2 16 4,  SDL.V4 100 50 10 255)
        , (SDL.V2 10 20,   SDL.V2 6 6,   SDL.V4 255 255 255 200)
        ]
    | bId == idBuffVidC =
        [ (SDL.V2 2 10,    SDL.V2 36 34, SDL.V4 255 215 0 255)
        , (SDL.V2 10 5,    SDL.V2 20 12, SDL.V4 255 215 0 255)
        , (SDL.V2 8 3,     SDL.V2 24 4,  SDL.V4 255 255 255 100)
        , (SDL.V2 11 0,    SDL.V2 18 5,  SDL.V4 80 0 0 255)
        , (SDL.V2 12 22,   SDL.V2 8 8,   SDL.V4 255 255 255 220)
        ]
    | bId == idBuffTiempoB =
        [ (SDL.V2 0 0,     SDL.V2 40 6,  SDL.V4 192 192 192 255)
        , (SDL.V2 0 44,    SDL.V2 40 6,  SDL.V4 192 192 192 255)
        , (SDL.V2 2 6,     SDL.V2 4 38,  SDL.V4 160 160 160 255) 
        , (SDL.V2 34 6,    SDL.V2 4 38,  SDL.V4 160 160 160 255) 
        , (SDL.V2 8 6,     SDL.V2 24 10, SDL.V4 200 200 255 255) 
        , (SDL.V2 12 16,   SDL.V2 16 8,  SDL.V4 200 200 255 255)
        , (SDL.V2 18 24,   SDL.V2 4 4,   SDL.V4 200 200 255 255)
        , (SDL.V2 8 34,    SDL.V2 24 10, SDL.V4 200 200 255 255)
        ]
    | bId == idBuffTiempoC = 
        [ (SDL.V2 0 0,     SDL.V2 40 6,  SDL.V4 40 40 60 255)
        , (SDL.V2 0 44,    SDL.V2 40 6,  SDL.V4 40 40 60 255)
        , (SDL.V2 2 6,     SDL.V2 4 38,  SDL.V4 80 80 100 255)
        , (SDL.V2 34 6,    SDL.V2 4 38,  SDL.V4 80 80 100 255) 
        , (SDL.V2 8 6,     SDL.V2 24 10, SDL.V4 0 255 200 255)   
        , (SDL.V2 12 16,   SDL.V2 16 8,  SDL.V4 0 255 200 255)
        , (SDL.V2 18 24,   SDL.V2 4 4,   SDL.V4 0 255 200 255)
        , (SDL.V2 8 34,    SDL.V2 24 10, SDL.V4 0 255 200 255)
        , (SDL.V2 15 20,   SDL.V2 10 10, SDL.V4 255 255 255 100) 
        ]
    | otherwise = []