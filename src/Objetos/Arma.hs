{-# LANGUAGE OverloadedStrings #-}
module Objetos.Arma where
-- Módulos del sistema
import qualified SDL
import qualified Lens.Micro         as LMi
import qualified Linear.Vector      as LV
import qualified Data.Text          as DT
-- Módulos propios
import qualified Graficos.Dibujado  as GD
import qualified Globals.Types      as GType

idArmPuño :: Int
idArmPuño = 0

idArmGlock :: Int
idArmGlock = 101
idItemArmGlock :: Int
idItemArmGlock = 1101
nomItemArmGlock :: DT.Text
nomItemArmGlock = "Glock 17"

idArmEscopeta :: Int
idArmEscopeta = 102
idItemArmEscopeta :: Int
idItemArmEscopeta = 1102
nomItemArmEscopeta :: DT.Text
nomItemArmEscopeta = "Escopeta 12G"

idArmFusil :: Int
idArmFusil = 103
idItemArmFusil :: Int
idItemArmFusil = 1103
nomItemArmFusil :: DT.Text
nomItemArmFusil = "Fusil de Asalto M52"

idArmLanzallamas :: Int
idArmLanzallamas = 104
idItemArmLanzallamas :: Int
idItemArmLanzallamas = 1104
nomItemArmLanzallamas :: DT.Text
nomItemArmLanzallamas = "Lanzallamas MX-7"

idArmSniper :: Int
idArmSniper = 105
idItemArmSniper :: Int
idItemArmSniper = 1105
nomItemArmSniper :: DT.Text
nomItemArmSniper = "Rifle M24 SWS"

idArmSMG :: Int
idArmSMG = 106
idItemArmSMG :: Int
idItemArmSMG = 1106
nomItemArmSMG :: DT.Text
nomItemArmSMG = "Subfusil Uzi"

idArmRPG :: Int
idArmRPG = 107
idItemArmRPG :: Int
idItemArmRPG = 1107
nomItemArmRPG :: DT.Text
nomItemArmRPG = "RPG-7"

idArmPlasma :: Int
idArmPlasma = 108
idItemArmPlasma :: Int
idItemArmPlasma = 1108
nomItemArmPlasma :: DT.Text
nomItemArmPlasma = "Rifle de Plasma XV"

idArmMinigun :: Int
idArmMinigun = 109
idItemArmMinigun :: Int
idItemArmMinigun = 1109
nomItemArmMinigun :: DT.Text
nomItemArmMinigun = "Minigun M134"


crearStatsArma :: Int -> GType.Arma
crearStatsArma armId
    | armId == idArmPuño        = mkArma idArmPuño          10.0 80.0 0.1
    | armId == idArmGlock       = mkArma idArmGlock         10.0 30.0 0.1
    | armId == idArmEscopeta    = mkArma idArmEscopeta      25.0 20.0 1.0
    | armId == idArmFusil       = mkArma idArmFusil         6.0 35.0 0.05
    | armId == idArmLanzallamas = mkArma idArmLanzallamas   3.0 15.0 0.02
    | armId == idArmSniper      = mkArma idArmSniper        45.0 15.0 1.5   
    | armId == idArmSMG         = mkArma idArmSMG           2.5  45.0 0.06  
    | armId == idArmRPG         = mkArma idArmRPG           90.0 10.0 2.0   
    | armId == idArmPlasma      = mkArma idArmPlasma        7.0  40.0 0.15  
    | armId == idArmMinigun     = mkArma idArmMinigun       1.8  60.0 0.04  
    | otherwise = error "crearStatsArma: id de arma desconocido"
  where
    mkArma :: Int -> Float -> Float -> Float -> GType.Arma
    mkArma armaId heatPerShot coolRate fireRate =
        GType.Arma
            { GType._armID          = armaId
            , GType._eaHeat         = 0
            , GType._eaCool         = 0
            , GType._eaJammed       = False
            , GType._eaMaxHeat      = 100.0
            , GType._eaHeatPerShot  = heatPerShot
            , GType._eaCoolRate     = coolRate
            , GType._eaFireRate     = fireRate
            }

crearBoxArma :: Int -> SDL.V2 Float -> GType.Box
crearBoxArma armId pos
    | armId == idArmGlock       = mkBox (SDL.V2  50  50) 50.0
    | armId == idArmEscopeta    = mkBox (SDL.V2 100 100) 50.0
    | armId == idArmFusil       = mkBox (SDL.V2 90  35)  70.0
    | armId == idArmLanzallamas = mkBox (SDL.V2 120 40)  80.0
    | armId == idArmSniper      = mkBox (SDL.V2 130 30) 85.0
    | armId == idArmSMG         = mkBox (SDL.V2 50  40) 45.0
    | armId == idArmRPG         = mkBox (SDL.V2 110 35) 75.0
    | armId == idArmPlasma      = mkBox (SDL.V2 95  45) 70.0
    | armId == idArmMinigun     = mkBox (SDL.V2 100 50) 80.0
    | otherwise                 = error "crearBoxArma: arma sin Box definido"
  where
    mkBox tam rad = GType.Box
        { GType._boxPos = pos
        , GType._boxTam = tam
        , GType._boxAng = 0.0
        , GType._boxRad = rad
        }

crearItemArma :: Int -> SDL.V2 Float -> GType.Item
crearItemArma armId pos
    | armId == idArmGlock        = mkItem idItemArmGlock        nomItemArmGlock
    | armId == idArmEscopeta     = mkItem idItemArmEscopeta     nomItemArmEscopeta
    | armId == idArmFusil        = mkItem idItemArmFusil        nomItemArmFusil
    | armId == idArmLanzallamas  = mkItem idItemArmLanzallamas  nomItemArmLanzallamas
    | armId == idArmSniper      = mkItem idItemArmSniper      nomItemArmSniper
    | armId == idArmSMG         = mkItem idItemArmSMG         nomItemArmSMG
    | armId == idArmRPG         = mkItem idItemArmRPG         nomItemArmRPG
    | armId == idArmPlasma      = mkItem idItemArmPlasma      nomItemArmPlasma
    | armId == idArmMinigun     = mkItem idItemArmMinigun     nomItemArmMinigun
    | otherwise                  = error "crearItemArma: id de arma desconocido o sin item asociado"
  where
    mkItem itemId itemNom = GType.Item
        { GType._iteId   = itemId
        , GType._iteNom  = itemNom
        , GType._iteTipo = GType.EsArma (crearStatsArma armId)
        , GType._iteBox  = crearBoxArma armId pos
        , GType._iteInv  = True
        , GType._iteAct  = False
        }

dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Item -> IO ()
dibujar renderer texture camPos zoom item = do
    let pos = item LMi.^. GType.iteBox . GType.boxPos
    let tam = item LMi.^. GType.iteBox . GType.boxTam
    let ang = item LMi.^. GType.iteBox . GType.boxAng

    if GD.esVisible pos tam ang camPos zoom 
        then do
            let maybeArma = item LMi.^? GType.iteTipo . GType._EsArma
            
            case maybeArma of
                Nothing -> return ()
                Just arma -> do
                    let bid = arma LMi.^. GType.armID
                    let partes = damePartesArma bid
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
dibujarIcono renderer texture posHUD sizeHUD armId = do
    let partes = damePartesArma armId
    if null partes then return () else do
        let maxX = maximum $ map (\(p, s, _) -> (p LMi.^. SDL._x) + (s LMi.^. SDL._x)) partes
        let maxY = maximum $ map (\(p, s, _) -> (p LMi.^. SDL._y) + (s LMi.^. SDL._y)) partes

        let padding    = 10.0
        let targetSize = sizeHUD - SDL.V2 padding padding
        let scaleX     = (targetSize LMi.^. SDL._x) / maxX
        let scaleY     = (targetSize LMi.^. SDL._y) / maxY
        let scale      = min scaleX scaleY

        let contentW = maxX * scale
        let contentH = maxY * scale
        let offsetX  = ((sizeHUD LMi.^. SDL._x) - contentW) / 2
        let offsetY  = ((sizeHUD LMi.^. SDL._y) - contentH) / 2
        let startPos = posHUD + SDL.V2 offsetX offsetY

        mapM_ (\(offsetLocal, sizeLocal, color) -> do
            let pFinal = startPos + (offsetLocal LV.^* scale)
            let sFinal = sizeLocal LV.^* scale
            GD.dibujarOverlay renderer texture pFinal sFinal color
            ) partes

damePartesArma :: Int -> [GD.ParteDibujo]
damePartesArma armId 
    | armId == idArmGlock = 
        [ (SDL.V2 0 0,   SDL.V2 70 11, SDL.V4 40 40 40 255)
        , (SDL.V2 8 11,  SDL.V2 52 10, SDL.V4 20 20 20 255)
        , (SDL.V2 43 20, SDL.V2 14 26, SDL.V4 20 20 20 255)
        , (SDL.V2 22 16, SDL.V2 16 9,  SDL.V4 20 20 20 255)
        ]
    | armId == idArmEscopeta = 
        [ (SDL.V2 0 10,    SDL.V2 40 25, SDL.V4 101 67 33 255) 
        , (SDL.V2 40 5,    SDL.V2 30 20, SDL.V4 30 30 30 255)
        , (SDL.V2 70 5,    SDL.V2 110 8, SDL.V4 60 60 60 255)
        , (SDL.V2 70 15,   SDL.V2 100 8, SDL.V4 30 30 30 255)
        , (SDL.V2 90 14,   SDL.V2 40 10, SDL.V4 101 67 33 255)
        , (SDL.V2 50 25,   SDL.V2 12 8,  SDL.V4 30 30 30 255)
        ]
    | armId == idArmFusil =
        [ (SDL.V2 0 5,     SDL.V2 50 20, SDL.V4 35 35 35 255)
        , (SDL.V2 50 0,    SDL.V2 90 28, SDL.V4 50 50 55 255)
        , (SDL.V2 55 28,   SDL.V2 18 25, SDL.V4 30 30 30 255)
        , (SDL.V2 95 28,   SDL.V2 25 40, SDL.V4 20 20 20 255)
        , (SDL.V2 140 2,   SDL.V2 70 24, SDL.V4 60 65 60 255)
        , (SDL.V2 210 10,  SDL.V2 15 6,  SDL.V4 50 50 55 255)
        , (SDL.V2 70 (-8), SDL.V2 40 8,  SDL.V4 10 10 10 255)
        , (SDL.V2 75 0,    SDL.V2 10 5,  SDL.V4 10 10 10 255)
        ]
    | armId == idArmLanzallamas =
        [ (SDL.V2 40 30,   SDL.V2 100 35, SDL.V4 180 40 40 255)
        , (SDL.V2 20 10,   SDL.V2 140 20, SDL.V4 100 100 100 255)
        , (SDL.V2 0 10,    SDL.V2 20 40,  SDL.V4 100 100 100 255) 
        , (SDL.V2 100 65,  SDL.V2 10 15,  SDL.V4 160 140 40 255)
        , (SDL.V2 60 25,   SDL.V2 10 10,  SDL.V4 160 140 40 255)
        , (SDL.V2 160 5,   SDL.V2 30 30,  SDL.V4 40 30 30 255)
        , (SDL.V2 185 15,  SDL.V2 5 5,    SDL.V4 255 100 0 255)
        ]
    | armId == idArmSniper = 
        [ (SDL.V2 0 10,    SDL.V2 60 20, SDL.V4 40 40 40 255)
        , (SDL.V2 60 5,    SDL.V2 80 25, SDL.V4 30 30 30 255)
        , (SDL.V2 140 12,  SDL.V2 90 8,  SDL.V4 10 10 10 255)
        , (SDL.V2 230 10,  SDL.V2 10 12, SDL.V4 10 10 10 255)
        , (SDL.V2 70 (-5), SDL.V2 50 8,  SDL.V4 20 20 20 255)
        , (SDL.V2 65 (-8), SDL.V2 60 8,  SDL.V4 10 10 10 255)
        , (SDL.V2 90 30,   SDL.V2 15 20, SDL.V4 20 20 20 255)
        ]
    | armId == idArmSMG = 
        [ (SDL.V2 10 15,   SDL.V2 20 35, SDL.V4 30 30 30 255)
        , (SDL.V2 0 0,     SDL.V2 70 30, SDL.V4 50 50 55 255)
        , (SDL.V2 70 10,   SDL.V2 20 10, SDL.V4 20 20 20 255)
        , (SDL.V2 40 30,   SDL.V2 15 30, SDL.V4 15 15 15 255)
        , (SDL.V2 5 5,     SDL.V2 60 5,  SDL.V4 70 70 75 255)
        ]
    | armId == idArmRPG = 
        [ (SDL.V2 0 15,    SDL.V2 40 20, SDL.V4 139 69 19 255)
        , (SDL.V2 40 10,   SDL.V2 120 30, SDL.V4 60 70 60 255)
        , (SDL.V2 60 40,   SDL.V2 15 25, SDL.V4 40 40 40 255)
        , (SDL.V2 110 40,  SDL.V2 15 25, SDL.V4 40 40 40 255)
        , (SDL.V2 160 5,   SDL.V2 50 40, SDL.V4 50 60 50 255)
        , (SDL.V2 210 15,  SDL.V2 10 20, SDL.V4 180 180 180 255)
        , (SDL.V2 80 5,    SDL.V2 40 5,  SDL.V4 30 30 30 255)
        ]
    | armId == idArmPlasma = 
        [ (SDL.V2 0 10,    SDL.V2 50 30, SDL.V4 200 200 210 255) 
        , (SDL.V2 50 5,    SDL.V2 80 40, SDL.V4 50 50 60 255)
        , (SDL.V2 130 15,  SDL.V2 40 20, SDL.V4 200 200 210 255) 
        , (SDL.V2 135 20,  SDL.V2 35 10, SDL.V4 0 255 255 255)
        , (SDL.V2 60 15,   SDL.V2 40 10, SDL.V4 0 255 255 255)
        , (SDL.V2 60 45,   SDL.V2 20 20, SDL.V4 40 40 50 255)
        ]
    | armId == idArmMinigun = 
        [ (SDL.V2 0 20,    SDL.V2 60 40, SDL.V4 40 40 40 255)
        , (SDL.V2 60 30,   SDL.V2 20 50, SDL.V4 30 30 30 255)
        , (SDL.V2 10 (-10),SDL.V2 40 15, SDL.V4 30 30 30 255)
        , (SDL.V2 60 10,   SDL.V2 100 60, SDL.V4 20 20 20 255)
        , (SDL.V2 160 15,  SDL.V2 10 50, SDL.V4 60 60 60 255)
        , (SDL.V2 60 35,   SDL.V2 120 10, SDL.V4 50 50 50 255)
        , (SDL.V2 60 15,   SDL.V2 120 10, SDL.V4 40 40 40 255)
        , (SDL.V2 60 55,   SDL.V2 120 10, SDL.V4 40 40 40 255)
        ]
    | otherwise = []