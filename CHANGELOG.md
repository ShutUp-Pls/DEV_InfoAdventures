# Revision history for DEV-InfoAdventures

## 0.1.0.0 -- 2025-11-25
* Primera versión.
* Se levanta el ambiente grafico con un jugador placeholder para definir la logica de movimiento.
* Se integra la primera monadeState para gestionar los estados del juego en general.
* Se añaden los archivo `.hs` bajo la siguiente visión:
    * `Juego.hs`    : Define la monadeState del juego.
    * `Types.hs`    : Define tipos para no ensuciar codigo con lógica.
    * `Jugador.hs`  : Logica de comportamiento para el jugador.
    * `Mapa.hs`     : Placeholder para una futura logica de mapa.
    * `Main.hs`     : Inicializa el entorno gráfico, carga los módulos principales y ejecuta el bucle inicial.

>*Nota*: Digo 'primera' porque me di cuenta que los monadeState son muy buenos para gestionar estados, pero malisimos para definir logicas. Por ejemplo, intenté aplicar monadeState al movimiento del jugador y generó muchos enredo a la hora de usarlo.

## 0.1.1.0 -- 2025-11-25
* Se corrige el movimiento acelerado diagonal. Ahora se usa un vector normalizado.
* Se añade la posibilidad de correr presionando 'Shift'.
* El movimiento pasa de ser manejado con CInt a Float puro.

>*Nota*: El renderizado exige manejarse en CInt, pero la logica detrás la haremos con más precisión.

## 0.1.2.0 -- 2025-11-25
* Se añade una logica de mapas basadas de rectangulos de colisiones.
* Se añade `Colisiones.hs` para manejar las logica anexa a las colisiones.
* Se empieza a usar `qualified` en las importaciones para tener control de donde viene cada función.
* Se añade el dibujado de los obstaculos de colisión.

>*Nota*: Me estaba perdiendo mucho cuando salian errores de compilación por desconocer el origen de las funciones.