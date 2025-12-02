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
 * Aliviamos la carga del `monadeState` del juego principal, delegando toda la logica de calculo a modulos externos, dejando en la `monadeState` solo la aplicación de estos mediante funciones.

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

 ## 0.1.7.0 -- 2025-11-26
  * Se añade un 'mapeador' hecho en python para facilitar la integración de nuevos mapas.
  * Se añade la capacidad a los rectangulos obstaculo de tener rotación y así tener diagonales en los mapas.
  * Se cambia la logica de colisión simple el Teorema del Eje de Separación (SAT - Separating Axis Theorem) en `SAT.hs`. Esto para gestionar las colisiones con diagonales.
  * Se añadé 2 `monadeState` en `MovJugador.hs` y `MovEnemigo.hs`
  * Se vuelve a llenar `Jugador.hs` con los wrappers de las `monadeState`. Lo mismo con `Enemigo.hs`, expone una forma facil de acceder a una logica de estado.
  * Se desacopla el calculo de un vector para seguir al jugador.

   >*Nota*: Viendo las `monadeState` como orquestador de estados *("Me entregas algo en un estado y te lo devuelvo en otro")*, entonces son logicas totalmente encapsulables en su proposito propio y reunibles en archivos que pretendan reuinir todas las que sirvan para un proposito conjunto. JSAJDASJ ni yo me entendí pero en mi cabeza está genial.

 ## 0.1.8.0 -- 2025-11-26
  * Aplicamos 'Resolución por Vector de Traslación Mínima (MTV)' para la logica de 'slicing' que antes estaba enfocada a colisiones verticales y horizontales, para ahora estar enofada a colisiones diagonales también a traves del SAT.
  * Se unifica el movimiento en una única `monadeState` en `Movimiento.hs` y vuelven a `Jugador.hs` `Enemigo.hs` la logica especifica sobre los calculos previos (Que también son una `monadeState` - Esto está interesante para ser explicado en el README.md).
  * Se eliminan `MovEnemigo.hs` y `MovJugador.hs`

 ## 0.1.8.1 -- 2025-11-27
  * Se añade la renderización y calculo de conos de visión en el archivo `Conos.hs`.
  * Se dota a Enemigos y Jugador de angulo de dirección.

 ## 0.1.8.2 -- 2025-11-27
  * Ahora los enemigos y el jugador rotan su cuadrado a donde están mirando.
  * La dirección del jugador cambia de un paradigma estatico a uno direccional.
  * Ahora los enemigos colisionan entre ellos.

 ## 0.1.8.3 -- 2025-11-27
  * Refactorización de `src`. Ahora no se encuentra al interior de app.

  >*Nota*: Puede parecer una actualización sin importancia, pero fue el resultado de intentar generar un motor grafico por si solo. No resultó, pero me quedo con el movimiento de la carpeta.

 ## 0.1.9.0 -- 2025-11-28
  * Se generaliza el uso de `copyEx` para el renderizado de rectangulos con textura por su eficiencia grafica.
  * Se refactoriza el dibujado y se le devuelve a cada archivo que pretende describir un objeto
    * Se crear `HUD.hs` y `Obstaculo.hs` para cumplir con lo anterior
  * Ahora los enemigos no te siguen con la mirada en todo momento.
  * Ahora los enemigos tienen las misma gestión de movimiento estilo tanque del jugador (Rotar -> Mover)

 ## 0.1.10.0 -- 2025-11-28
  * Se añade el objeto `Spawner.hs` a fin de centralizar la generación de enemigo en ciertas areas.
  * Se añade la posibilidad de hacer 'zoom-in' y 'zoom-out' con la tecla "+" y "-" respectivamente.
  * Se añade la posibilidad de activar y desactivar un 'zoom-in' y 'zoom-out' cuando un enemigo te visualiza.
  * Se refactorizan las constantes dentro de logicas del juego para conservarla en el header del codigo.

 ## 0.1.11.0 -- 2025-11-29
  * Se añade la posibilidad de morir y reaparecer al jugador.
  * Se añaden particulas explosivas.
  * Se refactoriza `Juego.hs` en varias `monadeStates` con el fin de manejar distintos estados del juego.
  * Se refactoriza `Types.hs` para facilitar lectura y se estructura para mejorar logica y escalabilidad.
  * Cada submodulo implementa su propio `Types.hs`.
  * Se integra la carpeta 'Globals' donde se posicionan los tipos y utilidades que solo beben del sistema, dedicados a alimentar los modulos.
  * "Enemigo" ahora es "Zombie".
  * Se agregan 'Lenses' a los `Types.hs` para aumentar escalabilidad.
  * Se eliminan monadeState redundante en `Jugador.hs` y `Zombie.hs`.
  * Se introduce el modulo `IA.hs` y `Control.hs` en 'Personajes', además `Angulos.hs` y `Vectores.hs` en 'Fisicas'.
  * Modulos `Utils.hs` eliminados.
  * Se separa la logica de buffos, con los items y como lo almacena el jugador.
  * Se ordena el dibujado y se añade un overlay a la pantalla de muerte.
  * Cambiado los graficos de `SDL.V3` a `SDL.V4`.
  * ¡Nuevo mapa! Ya era hora.
  * Se ajusta el SAT para que ahora calcule basandose en el centro del jugador sumado a pasos simulados ¡Ya no se atraviesan paredes! (Este bug lo tenía desde que cambié a SAT)

  >*Nota*: El añadido de particulas me sirvió para testear los limites del renderizar de esta forma tantos dibujos en movimiento. (10.000 particulas y aún así el movimiento fue fluido, solo disminuyeron un pocos los FPS)
  >*Nota*: La refactorización fue un infierno que me llevó todo el día.

 ## 0.1.11.0 -- 2025-11-29
  * Se corrige el `CHANGELOG.md` anterior. Mentí, no se agregó nuevo enemigo.
  * Se agrega el primer arma y la capacidad de matar enemigos.
    * Se añade `Disparo.hs` en 'Fisca' para gestionar la logica de disparo.
    * Se añade `Bala.hs` en 'Objetos'. Altamente basado en `Particula.hs` para generar balas.
    * Se añade `Arma.hs`' enfocado en gestionar los distintos tipos de arma.
    * Se introduce el uso de 'Lens' completo para usar 'makePrism' en el patternMatch de Data Types.
    * Se cambia de `ItemsBuff.hs` a `Buff.hs` y se equivale la logica de los items.

## 1.0.0.0 -- 2025-11-30
  * Otra gran refactorización en fisicas, particulas y demases para asegurar un sistema de combate escalable.
  * Se añade el tiempo como mecanica principal y objetivo del juego, surival contra reloj.
  * Se añade un mapa mucho más grande.
  * Se añade launcher con la posibilidad de activar tutorial y elegir mapa (Aún no implementado).
  * Se añaden mas armas y más enemigos.

    >*Nota*: Se puede decir que se puede jugar, el gameplay es muy pobre, pero al menos ya no es un intento de juego, ahora si tiene un objetivo y variedad de formas de llevarlo a cabo. Se hicieron muchas cosas, entré en flujo y me olvidé de actualizar el changelog.

## 1.0.1.0 -- 2025-11-01
 * Refactorización de codigo, se eliminan varías monadeState que no tenían ninguna razón de ser.
 * Se elimina `Objetos.Types` y se delega todo a una relación entre `Personajes.Types`, `Globals.Types` y `Types`
 * Se utiliza una logica de 'pipeline' en los modulos de fisica para ayudar a la legibilidad.
 * Se implementa la defición in situ de particulas por ID para manejar de forma localizada sus datos.
 * Logica de 'procesaBala' totalmente diseccionada.
 * Ya no son los personajes los que gestionan sus combates, ahora está todo en `Fisica.Combates`.
 * Se repatría la idea de movimientos particulares en sus modulos y los generales en `Fisica.MovEntidad`, ya que por ahora todo lo que se mueve es una entidad.
 * Se comienza a esribir el README.md con formato de informe.

 >*Nota*: Escribiendo el readme me di cuenta que he estado no aprovechando del todo las `Fisica.Combates`, por lo que habrá una siguiente refactorización que utilizaré para ejemplificar en el informa

 ## 1.0.1.1 -- 2025-11-02
  * Un ejemplo muy ilustrativo de la  aplicación de `monadeStates` en el codigo real.

 ## 2.0.0.0 -- 2025-11-02
  * Se implementa tutorial de juego.
  * Se arregla el estado de las `monadeStates` en `Juego.hs` para que se aproveche el estilo monadico.
  * Se ajustan parametros del juego para mejorar gameplay.
  * Se elimina codigo redundate y duplicado.
  * Se optimiza el checkeo de colisiones que realintazaba el juego.
  * ¿Armas invisibles?
  * Versión funcional "balanceada".

## 2.0.0.1 -- 2025-11-02
  * Mapa ahora si completo.
  * Arreglo de bugs y balanceo de armas.
  * Se añade Makefile
  * Se completa el `README.md
  * Se corrige la fecha anterior del Changelog para correlación con la realidad de los 'commits'.

 >*Nota*: ¡Por fin!

 ## 2.0.0.2 -- 2025-11-02
  * Rebalanceo: Se aumenta el rating de aparición de items de tiempo.
  * Rebalanceo: Se aumenta el tiempo inicial al comenzar el juego.
  * Bugfix: El contador no se detenía al morir el jugador.

 ## 2.0.0.3 -- 2025-11-02
  * Hotfix: Push para corregir la versión del commit.