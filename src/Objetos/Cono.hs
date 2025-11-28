module Objetos.Cono where

import qualified SDL
import qualified Linear.Vector as LV

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