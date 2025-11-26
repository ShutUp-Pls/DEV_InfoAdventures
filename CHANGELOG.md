# Revision history for DEV-InfoAdventures

## 0.1.0.0 -- 2025-11-25
* Primera versión.
* Se levanta el ambiente grafico con un jugador placeholder para definir la logica de movimiento.
* Se integra la primera `monadeState` para gestionar los estados del juego en general.
* Se añaden los archivo `.hs` bajo la siguiente visión:
    * `Juego.hs`    : Define la `monadeState` del juego.
    * `Types.hs`    : Define tipos para no ensuciar codigo con lógica.
    * `Jugador.hs`  : Logica de comportamiento para el jugador.
    * `Mapa.hs`     : Placeholder para una futura logica de mapa.
    * `Main.hs`     : Inicializa el entorno gráfico, carga los módulos principales y ejecuta el bucle inicial.

>*Nota*: Digo 'primera' porque me di cuenta que los `monadeState` son muy buenos para gestionar estados, pero malisimos para definir logicas. Por ejemplo, intenté aplicar `monadeState` al movimiento del jugador y generó muchos enredo a la hora de usarlo.

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

## 0.1.2.1 -- 2025-11-25
 * Se quita la gestión de tipos desde la posición al dibujado. Ahora se usa la función `toSDLRect`.
 * Se añade `tamJugador` como atributo del tipo `Jugador` para evitar tener que hardcodearlo en el codigo.

## 0.1.3.0 -- 2025-11-25
 * Se implementa 'slicing' en el jugador para para soportar colisiones sin que estas te detengan en seco.

## 0.1.4.0 -- 2025-11-25
 * Se implementa 'Deadzone' para la camara, ajustable mediante tecla 'O' para disminuir su area y tecla 'P' para aumentarla.
 * `monadeState` ahora maneja la posición de la camara, la posición de la camara y el tamaño de la 'Deadzone' de la camra.

## 0.1.5.0 -- 2025-11-26
 * Se implementa `Enemigo.hs`. Prototipo de enemigo con atributos propios y logica propia.

## 0.1.5.1 -- 2025-11-26
 * Refactorización de codigo.
 * Ahora las logicas de movimiento de cada personaje viven en `MovEnemigo.hs` y `MovJugador.hs`
 * Ahora las colisión caracteriza la clase 'Hitbox' para cada personaje.
 * Se modifican nombres de variables y comentarios para mejor seguimiento del codigo.
 * Se crea un tipo 'Camara' y su respectivo `Camara.hs` con toda la carga logica detras.
 * Aliviamos la carga del monadeState del juego principal, delegando toda la logica de calculo a modulos externos, dejando en la monadeState solo la aplicación de estos mediante funciones.

 >*Nota*: Poco a poco se me olvida que estamos ante un lenguaje de paradigma funcional. El manejo del lenguaje empieza a sentirse familiar.

 ## 0.1.6.0 -- 2025-11-26
  * Se añaden enemigos en lista. Ahora puede haber multiples enemigos en pantalla
  * Se añaden los items con su respectiva logica en `Items.hs`. Sus colisiones se añaden a `Colisiones.hs`
  * Se quitan alias de data types innecesarios. No hay tipos muy complejos que lo requieran.
  * Ahora jugador y enemigos tienen vida.
  * Se añade HUD para que el jugador pueda ver como los items afectas a sus stats.
  * Se separa la logica de renderizado del `Main.hs` y se centraliza en `Render.hs`
  * La velocidad pasa a ser un dato dentro de 'Jugador'.
  * Se añade rebote al chocar con un enemigo para evitar spam en el daño a la vida.

   >*Nota*: Contra más grande el commit, más toca refactorizar despues.

 ## 0.1.6.1 -- 2025-11-26
  * Se separa la logica de la aplicación de Buffos con la adquisición de PowerUps.
  * Se separa la logica de dibujado con la de renderizado.
  * Se añade el tipo Buffos para manejar nombre en pantalla, tipo, valor y acumulabilidad.
  * Se refactoriza el orden del repositorio:
    * Directorio 'Objetos' para `Camara.hs` y `Items.hs`.
    * Directorio 'Graficos' para `Dibujado.hs` y `Render.hs`.
    * Archivo `Inicio.hs` para definir estados de inicio al iniciar el juego.
    * Archivo `Utils.hs` para definir calculos utiles pero fuerza de contexto.
  * Se estandariza la importación `import qualified Modulo` para modulos simples.
  * Se estandariza la importación `import qualified Modulo.Sub as MS` para modulos con submodulos.

   >*Nota*: Ya estaba empezando a ser un festival de nombres sin saber de donde venía que cosa. `Jugador.hs` quedó vacío tra desplegar la logica en distintos componentes.