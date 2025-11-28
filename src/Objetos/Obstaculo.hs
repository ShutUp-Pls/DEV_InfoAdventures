module Objetos.Obstaculo where

import qualified SDL
import Types

import qualified Graficos.Dibujado as GD
dibujar :: SDL.Renderer -> SDL.Texture -> SDL.V2 Float -> Float -> Types.Obstaculo -> IO ()
dibujar renderer texture camPos zoom obst = do
    let posO = Types.posObstaculo obst
    let tamO = Types.tamObstaculo obst
    let angO = Types.angObstaculo obst

    GD.dibujarTextura renderer texture camPos zoom posO tamO angO (SDL.V3 100 100 100)