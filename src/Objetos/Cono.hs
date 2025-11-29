module Objetos.Cono where

import qualified SDL
import qualified Linear.Vector as LV
import qualified Graficos.Dibujado as GD

degToRad :: Float -> Float
degToRad d = d * (pi / 180)

calcularVerticesCono :: SDL.V2 Float -> Float -> Float -> Float -> [SDL.V2 Float]
calcularVerticesCono origen anguloCentral longitud apertura =
    let
        radCentral = degToRad anguloCentral
        radApertura = degToRad (apertura / 2)

        angIzq = radCentral - radApertura
        angDer = radCentral + radApertura

        vecIzq = SDL.V2 (cos angIzq) (sin angIzq)
        vecDer = SDL.V2 (cos angDer) (sin angDer)

        puntoIzq = origen + (vecIzq LV.^* longitud)
        puntoDer = origen + (vecDer LV.^* longitud)
    in
        [origen, puntoIzq, puntoDer]

dibujarConoOutline :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> Float -> Float -> Float -> IO ()
dibujarConoOutline renderer texture camPos zoom origen angulo longitud apertura = do
    let verticesMundo = calcularVerticesCono origen angulo longitud apertura
    
    let color = SDL.V4 255 255 0 255
    let grosor = 2

    let conectarPuntos [] = return ()
        conectarPuntos [_] = return ()
        conectarPuntos (p1:p2:ps) = do
            GD.dibujarLinea renderer texture camPos zoom p1 p2 grosor color 
            conectarPuntos (p2:ps)

    conectarPuntos verticesMundo

    case verticesMundo of
        (pOrigin:rest) -> 
            case reverse rest of
               (pLast:_) -> GD.dibujarLinea renderer texture camPos zoom pLast pOrigin grosor color
               [] -> return ()
        [] -> return ()