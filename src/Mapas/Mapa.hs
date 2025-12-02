module Mapas.Mapa where
-- Modulos del sistema
import qualified SDL
import qualified Lens.Micro         as LMi
-- Modulos propios
import qualified Globals.Types      as GType
import qualified Graficos.Dibujado  as GD

mapaIni :: [GType.Box]
mapaIni = 
    [ GType.Box (SDL.V2 0 0)     (SDL.V2 800 20)  0  0
    , GType.Box (SDL.V2 0 580)   (SDL.V2 800 20)  0  0
    , GType.Box (SDL.V2 0 0)     (SDL.V2 20 600)  0  0
    , GType.Box (SDL.V2 780 0)   (SDL.V2 20 600)  0  0
    , GType.Box (SDL.V2 300 200) (SDL.V2 100 100) 70 0
    ]

mapaTest :: [GType.Box]
mapaTest = limites ++ plataformas ++ obstaculosRotados ++ estructuras
  where
    limites = 
        [ GType.Box (SDL.V2 0 0)      (SDL.V2 2000 20) 0 0
        , GType.Box (SDL.V2 0 980)    (SDL.V2 2000 20) 0 0
        , GType.Box (SDL.V2 0 0)      (SDL.V2 20 1000) 0 0
        , GType.Box (SDL.V2 1980 0)   (SDL.V2 20 1000) 0 0
        ]

    plataformas =
        [ GType.Box (SDL.V2 200 850)  (SDL.V2 150 20) 0 0
        , GType.Box (SDL.V2 400 750)  (SDL.V2 150 20) 0 0
        , GType.Box (SDL.V2 600 650)  (SDL.V2 150 20) 0 0
        , GType.Box (SDL.V2 200 550)  (SDL.V2 400 20) 0 0
        ]

    obstaculosRotados =
        [ GType.Box (SDL.V2 900 800)  (SDL.V2 100 100) 45 0 
        , GType.Box (SDL.V2 1100 700) (SDL.V2 80 80)   30 0
        , GType.Box (SDL.V2 1250 850) (SDL.V2 100 20)  90 0 
        , GType.Box (SDL.V2 1000 400) (SDL.V2 300 20)  15 0
        ]

    estructuras =
        [ GType.Box (SDL.V2 1400 200) (SDL.V2 50 600)  0 0
        , GType.Box (SDL.V2 1600 600) (SDL.V2 200 20)  0 0
        , GType.Box (SDL.V2 1650 900) (SDL.V2 50 50)   0 0
        ]

mapaBox :: [GType.Box]
mapaBox = 
    [ GType.Box (SDL.V2 (-293.9) (-4549.1)) (SDL.V2 20.0 10000.0) 0.0 0
    , GType.Box (SDL.V2 (-294.4) (-4552.7)) (SDL.V2 10000.0 20.0) 0.0 0
    , GType.Box (SDL.V2 9687.0 (-4553.5)) (SDL.V2 20.0 10000.0) 0.0 0
    , GType.Box (SDL.V2 (-293.5) 5429.9) (SDL.V2 10000.0 20.0) 0.0 0
    , GType.Box (SDL.V2 3585.7 (-522.2)) (SDL.V2 20.0 900.0) 0.0 0
    , GType.Box (SDL.V2 3586.1 680.5) (SDL.V2 20.0 900.0) 0.0 0
    , GType.Box (SDL.V2 3585.3 1562.5) (SDL.V2 900.0 20.0) 0.0 0
    , GType.Box (SDL.V2 3584.9 (-523.8)) (SDL.V2 900.0 20.0) 0.0 0
    , GType.Box (SDL.V2 5716.3 679.4) (SDL.V2 20.0 900.0) 0.0 0
    , GType.Box (SDL.V2 5712.2 (-491.8)) (SDL.V2 20.0 900.0) 0.0 0
    , GType.Box (SDL.V2 4837.1 1562.8) (SDL.V2 900.0 20.0) 0.0 0
    , GType.Box (SDL.V2 4828.8 (-520.3)) (SDL.V2 900.0 20.0) 0.0 0
    , GType.Box (SDL.V2 3499.6 153.2) (SDL.V2 600.0 20.0) 315.0 0
    , GType.Box (SDL.V2 4675.9 910.2) (SDL.V2 600.0 20.0) 45.0 0
    , GType.Box (SDL.V2 4175.3 1270.0) (SDL.V2 600.0 20.0) 90.0 0
    , GType.Box (SDL.V2 4756.4 1259.0) (SDL.V2 600.0 20.0) 315.0 0
    , GType.Box (SDL.V2 4131.1 (-194.5)) (SDL.V2 350.0 20.0) 0.0 0
    , GType.Box (SDL.V2 4661.1 (-356.8)) (SDL.V2 350.0 20.0) 270.0 0
    , GType.Box (SDL.V2 5381.6 388.1) (SDL.V2 350.0 20.0) 0.0 0
    , GType.Box (SDL.V2 4826.5 387.4) (SDL.V2 350.0 20.0) 0.0 0
    , GType.Box (SDL.V2 5259.9 1044.4) (SDL.V2 350.0 20.0) 0.0 0
    , GType.Box (SDL.V2 3585.4 678.7) (SDL.V2 350.0 20.0) 0.0 0
    , GType.Box (SDL.V2 4054.9 794.2) (SDL.V2 350.0 20.0) 210.0 0
    , GType.Box (SDL.V2 4299.2 (-360.0)) (SDL.V2 350.0 20.0) 270.0 0
    , GType.Box (SDL.V2 4820.7 58.7) (SDL.V2 20.0 350.0) 0.0 0
    , GType.Box (SDL.V2 3765.7 1407.0) (SDL.V2 523.9 54.1) 0.0 0
    , GType.Box (SDL.V2 3760.8 1288.8) (SDL.V2 61.5 172.2) 0.0 0
    , GType.Box (SDL.V2 4218.2 1291.3) (SDL.V2 71.3 164.8) 0.0 0
    , GType.Box (SDL.V2 4070.3 1095.2) (SDL.V2 150.0 80.0) 205.0 0
    , GType.Box (SDL.V2 4249.1 898.9) (SDL.V2 86.1 78.7) 30.0 0
    , GType.Box (SDL.V2 3699.4 836.1) (SDL.V2 108.2 88.5) 0.0 0
    , GType.Box (SDL.V2 4013.2 770.4) (SDL.V2 86.1 78.7) 30.0 0
    , GType.Box (SDL.V2 4123.1 838.8) (SDL.V2 86.1 78.7) 30.0 0
    , GType.Box (SDL.V2 3805.1 1092.8) (SDL.V2 150.0 80.0) 340.0 0
    , GType.Box (SDL.V2 4953.8 1363.6) (SDL.V2 300.0 20.0) 0.0 0
    , GType.Box (SDL.V2 5115.1 1196.8) (SDL.V2 300.0 20.0) 0.0 0
    , GType.Box (SDL.V2 5441.8 1315.0) (SDL.V2 147.0 142.4) 65.0 0
    , GType.Box (SDL.V2 (-280.9) (-4103.9)) (SDL.V2 1000.0 20.0) 320.0 0
    , GType.Box (SDL.V2 (-338.0) (-3413.5)) (SDL.V2 1000.0 20.0) 230.0 0
    , GType.Box (SDL.V2 408.8 (-4041.9)) (SDL.V2 1000.0 20.0) 50.0 0
    , GType.Box (SDL.V2 854.1 (-3537.3)) (SDL.V2 425.0 20.0) 320.0 0
    , GType.Box (SDL.V2 417.3 (-3170.2)) (SDL.V2 425.0 20.0) 320.0 0
    , GType.Box (SDL.V2 928.0 (-3949.1)) (SDL.V2 1000.0 20.0) 270.0 0
    , GType.Box (SDL.V2 1416.2 (-4439.1)) (SDL.V2 1000.0 20.0) 180.0 0
    , GType.Box (SDL.V2 1905.5 (-3947.6)) (SDL.V2 1000.0 20.0) 270.0 0
    , GType.Box (SDL.V2 1991.7 (-3456.1)) (SDL.V2 425.0 20.0) 0.0 0
    , GType.Box (SDL.V2 1417.7 (-3458.8)) (SDL.V2 425.0 20.0) 0.0 0
    , GType.Box (SDL.V2 2204.0 (-3945.0)) (SDL.V2 1000.0 20.0) 270.0 0
    , GType.Box (SDL.V2 2694.3 (-4435.4)) (SDL.V2 1000.0 20.0) 180.0 0
    , GType.Box (SDL.V2 3184.7 (-3945.3)) (SDL.V2 1000.0 20.0) 270.0 0
    , GType.Box (SDL.V2 3270.4 (-3456.3)) (SDL.V2 425.0 20.0) 0.0 0
    , GType.Box (SDL.V2 2694.6 (-3453.5)) (SDL.V2 425.0 20.0) 0.0 0
    , GType.Box (SDL.V2 2550.2 (-973.1)) (SDL.V2 100.0 100.0) 355.0 0
    , GType.Box (SDL.V2 2829.6 (-651.8)) (SDL.V2 100.0 100.0) 45.0 0
    , GType.Box (SDL.V2 3111.1 (-571.2)) (SDL.V2 100.0 100.0) 330.0 0
    , GType.Box (SDL.V2 2508.2 (-1175.1)) (SDL.V2 100.0 100.0) 335.0 0
    , GType.Box (SDL.V2 2648.4 (-791.2)) (SDL.V2 100.0 100.0) 35.0 0
    , GType.Box (SDL.V2 2380.7 (-1578.6)) (SDL.V2 100.0 100.0) 115.0 0
    , GType.Box (SDL.V2 279.9 (-2785.8)) (SDL.V2 100.0 100.0) 30.0 0
    , GType.Box (SDL.V2 2460.6 (-1365.7)) (SDL.V2 100.0 100.0) 25.0 0
    , GType.Box (SDL.V2 2296.4 (-1732.1)) (SDL.V2 100.0 100.0) 100.0 0
    , GType.Box (SDL.V2 5721.8 1483.0) (SDL.V2 3975.0 100.0) 0.0 0
    , GType.Box (SDL.V2 1696.7 3456.5) (SDL.V2 3880.0 100.0) 90.0 0
    , GType.Box (SDL.V2 501.3 (-1717.3)) (SDL.V2 400.0 100.0) 245.0 0
    , GType.Box (SDL.V2 1291.7 (-2111.9)) (SDL.V2 100.0 100.0) 25.0 0
    , GType.Box (SDL.V2 994.8 (-2146.8)) (SDL.V2 100.0 100.0) 245.0 0
    , GType.Box (SDL.V2 687.5 (-2192.5)) (SDL.V2 100.0 100.0) 295.0 0
    , GType.Box (SDL.V2 450.3 (-2298.0)) (SDL.V2 100.0 100.0) 30.0 0
    , GType.Box (SDL.V2 310.9 (-2557.4)) (SDL.V2 100.0 100.0) 295.0 0
    , GType.Box (SDL.V2 412.8 (-2983.7)) (SDL.V2 100.0 100.0) 10.0 0
    , GType.Box (SDL.V2 1550.6 (-2048.7)) (SDL.V2 100.0 100.0) 230.0 0
    , GType.Box (SDL.V2 1792.5 (-2016.0)) (SDL.V2 100.0 100.0) 20.0 0
    , GType.Box (SDL.V2 3463.0 (-551.0)) (SDL.V2 100.0 100.0) 285.0 0
    , GType.Box (SDL.V2 75.5 (-1616.2)) (SDL.V2 400.0 100.0) 335.0 0
    , GType.Box (SDL.V2 185.1 (-1409.0)) (SDL.V2 400.0 100.0) 335.0 0
    , GType.Box (SDL.V2 398.4 (-988.7)) (SDL.V2 400.0 100.0) 335.0 0
    , GType.Box (SDL.V2 313.1 (-1183.6)) (SDL.V2 400.0 100.0) 335.0 0
    , GType.Box (SDL.V2 (-242.3) (-1378.0)) (SDL.V2 400.0 100.0) 245.0 0
    , GType.Box (SDL.V2 772.8 (-1158.9)) (SDL.V2 400.0 100.0) 245.0 0
    , GType.Box (SDL.V2 27.6 (-844.2)) (SDL.V2 400.0 100.0) 245.0 0
    ]


dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> GType.Box -> IO ()
dibujar renderer skinTexture camPos zoom caja = do
    let posJ = caja LMi.^. GType.boxPos
    let tamJ = caja LMi.^. GType.boxTam
    let angJ = caja LMi.^. GType.boxAng
    
    if GD.esVisible posJ tamJ angJ camPos zoom 
        then do GD.dibujarTextura renderer skinTexture camPos zoom posJ tamJ angJ (SDL.V4 195 195 195 255)
        else return ()