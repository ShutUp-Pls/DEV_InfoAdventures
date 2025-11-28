module Objetos.Cono where

import qualified SDL
import qualified Linear.Vector as LV
import qualified Graficos.Dibujado as GD

degToRad :: Float -> Float
degToRad d = d * (pi / 180)

calcularVerticesCono :: SDL.V2 Float -> Float -> Float -> Float -> [SDL.V2 Float]
calcularVerticesCono origen anguloCentral longitud apertura =
    let
        -- Convertimos a radianes
        radCentral = degToRad anguloCentral
        radApertura = degToRad (apertura / 2)

        -- Ángulos de los extremos
        angIzq = radCentral - radApertura
        angDer = radCentral + radApertura

        -- Vectores direccionales
        vecIzq = SDL.V2 (cos angIzq) (sin angIzq)
        vecDer = SDL.V2 (cos angDer) (sin angDer)

        -- Puntos finales
        puntoIzq = origen + (vecIzq LV.^* longitud)
        puntoDer = origen + (vecDer LV.^* longitud)
    in
        [origen, puntoIzq, puntoDer]

dibujarConoOutline :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> SDL.V2 Float -> Float -> Float -> Float -> IO ()
dibujarConoOutline renderer texture camPos zoom origen angulo longitud apertura = do
    -- Obtenemos vertices en coordenadas de MUNDO
    let verticesMundo = calcularVerticesCono origen angulo longitud apertura
    
    let color = SDL.V3 255 255 0 -- Amarillo
    let grosor = 2

    -- Función recursiva para conectar puntos
    let conectarPuntos [] = return ()
        conectarPuntos [_] = return ()
        conectarPuntos (p1:p2:ps) = do
            -- Pasamos camPos a la linea
            GD.dibujarLinea renderer texture camPos zoom p1 p2 grosor color 
            conectarPuntos (p2:ps)

    conectarPuntos verticesMundo
    
    -- Cerrar el cono (conectar último con el origen)
    case verticesMundo of
        (pOrigin:rest) -> 
            case reverse rest of
               (pLast:_) -> GD.dibujarLinea renderer texture camPos zoom pLast pOrigin grosor color
               [] -> return ()
        [] -> return ()