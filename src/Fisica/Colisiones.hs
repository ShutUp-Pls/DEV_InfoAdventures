module Fisica.Colisiones where

-- Modulos del sistema
import qualified SDL
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV

-- Modulos propios
import qualified Types
import qualified Fisica.SAT as FS

-- Verifica si algo con Hitbox choca contra algún obstáculo de la lista
checkColision :: Types.Hitbox a => a -> [Types.Obstaculo] -> Maybe (SDL.V2 Float)
checkColision entidad obstaculos =
    let colisiones = map getMTV obstaculos
        validos = filter (/= Nothing) colisiones
    in case validos of
        (Just mtv : _) -> Just mtv
        _              -> Nothing
  where
    posE = Types.getPos entidad
    tamE = Types.getTam entidad
    angE = Types.getAng entidad
    
    getMTV (Types.Obstaculo posObj tamObj angObj) =
        FS.satCollision posE tamE angE posObj tamObj angObj

-- Verifica si algo con Hitbox choca contra algún item de la lista
checkColisionsItems :: Types.Hitbox a => a -> [Types.Item] -> [Types.Item]
checkColisionsItems entidad listaItems =
    filter estaTocando listaItems
  where
    posE = Types.getPos entidad
    tamE = Types.getTam entidad
    angE = Types.getAng entidad
    
    estaTocando :: Types.Item -> Bool
    estaTocando it = 
        case FS.satCollision posE tamE angE (Types.posItem it) (Types.tamItem it) 0 of
            Just _  -> True
            Nothing -> False

-- Esta función acumula los enemigos procesados y actualiza al jugador acumulado
logicaColision :: Types.Enemigo -> (Types.Jugador, [Types.Enemigo]) -> (Types.Jugador, [Types.Enemigo])
logicaColision enem (jActual, enemigosProcesados) =
    -- Usamos pattern matching directamente sobre el resultado del SAT
    case FS.satCollision (Types.posJugador jActual) (Types.tamJugador jActual) 0 (Types.posEnemigo enem) (Types.tamEnemigo enem) 0 of
        
        -- Si hay colisión, recibimos el vector 'mtv'
        Just mtv ->
            let
                -- El MTV ya apunta en la dirección de salida perfecta
                -- Lo normalizamos para tener solo la dirección del golpe
                dir = LM.normalize mtv 
                
                -- Aplicamos el empuje usando esa dirección precisa
                nuevoVelJ = dir LV.^* Types.empujeE enem
                nuevoVelE = (dir LV.^* (-1)) LV.^* Types.empujeJ jActual
                
                jDañado = jActual { Types.vidJugador = Types.vidJugador jActual - 5.0
                                  , Types.velGolpeJ  = nuevoVelJ
                                  }
                enemGolpeado = enem { Types.velGolpeE = nuevoVelE }
            in
                (jDañado, enemGolpeado : enemigosProcesados)

        -- Si es Nothing, no hubo choque
        Nothing ->
            (jActual, enem : enemigosProcesados)

resolverCombate :: Types.Jugador -> [Types.Enemigo] -> (Types.Jugador, [Types.Enemigo])
resolverCombate jug enemigosList = 
    foldr logicaColision (jug, []) enemigosList

aplicarSeparacion :: [Types.Enemigo] -> Types.Enemigo -> Types.Enemigo
aplicarSeparacion todos miEnemigo =
    let 
        miPos   = Types.posEnemigo miEnemigo
        miRad   = Types.radInterno miEnemigo
        rechazo = Types.rechazoE miEnemigo
        miRadio = (let (SDL.V2 w h) = Types.tamEnemigo miEnemigo in (w + h) / 2) * miRad
        
        -- Calculamos el vector de empuje acumulado de todos los vecinos cercanos
        empujeTotal = foldr (\otroEnemigo acc -> 
            if otroEnemigo == miEnemigo 
            then acc 
            else
                let 
                    otroPos = Types.posEnemigo otroEnemigo
                    otroRadio = (let (SDL.V2 w h) = Types.tamEnemigo otroEnemigo in (w + h) / 2) * miRad
                    
                    distanciaMinima = miRadio + otroRadio
                    vectorDif = miPos - otroPos
                    distancia = LM.norm vectorDif
                in
                    -- Si están demasiado cerca (pero no son el mismo punto exacto)
                    if distancia < distanciaMinima && distancia > 0
                    then 
                        let 
                            -- Cuanto más cerca, más fuerte el empuje
                            penetracion = distanciaMinima - distancia
                            direccion = LM.normalize vectorDif
                        in 
                            acc + (direccion LV.^* penetracion)
                    else acc
            ) (SDL.V2 0 0) todos
        -- Aplicamos el empuje suavemente a la posición actual
    in 
        miEnemigo { Types.posEnemigo = miPos + (empujeTotal LV.^* rechazo) }

resolverColisionesEnemigos :: [Types.Enemigo] -> [Types.Enemigo]
resolverColisionesEnemigos listaEnemigos =
    map (aplicarSeparacion listaEnemigos) listaEnemigos