module Items where

import Types

-- Esta función toma un item y el jugador, y devuelve al jugador modificado
aplicarEfecto :: TipoItem -> Jugador -> Jugador
aplicarEfecto tipo j = case tipo of
    Vida cantidad -> 
        -- Aquí asumimos que tienes un campo 'vidaJugador'
        j { vidJugador = vidJugador j + cantidad }
        
    Velocidad factor -> 
        j { velFactorJ = factor }
        
    Puntos _ -> j -- Aún no implementado