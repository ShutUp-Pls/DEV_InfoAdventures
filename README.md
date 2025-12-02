**Estudiante responsable:** Marco Antonio Delgado Saldaña
# Instalación y Ejecución

### Requisitos Previos
Necesitas tener **Make** y **GHCup** (Haskell) instalados. Puedes descargarlos desde sus sitios oficiales:

* **Haskell (GHCup):** [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/)
    
    *(Este instalador gestionará GHC, Cabal y HLS automáticamente).*
* **Make:** [https://www.gnu.org/software/make/](https://www.gnu.org/software/make/)
    
    *(En Windows se recomienda instalarlo vía [Chocolatey](https://community.chocolatey.org/packages/make) o usar el entorno MSYS2).*

### Paso 1: Diagnóstico
Una vez instalados, **abre una terminal en la carpeta del proyecto** y ejecuta:

```bash
make check
```

El sistema verificará automáticamente si estás en Windows o Linux y te dirá qué te falta.

  * **Si ves (X) Error:** El terminal te dará el comando exacto que debes copiar y pegar para instalar la librería faltante (SDL2, Image o TTF).
  * **Si ves (✔) Todo correcto:** Pasa al siguiente paso.

### Paso 2: Compilación y Juego

Una vez que `make check` pase sin errores, simplemente ejecuta:

```bash
make run
```
Esto descargará automáticamente las librerías de Haskell necesarias, compilará el juego y lo iniciará.

> *El repositorio cuenta con archivo `.py` de Python, más no tienen ningún efecto funcional en el juego. Solo fueron herramientas generadas para facilitar el desarrollo. Puedes eliminarlos sin ningún problema una vez desargado el repositorio.*

# Introducción: Del Paradigma a la Práctica

Este proyecto implementa un videojuego de supervivencia 2D estilo *Action RPG* bajo una arquitectura puramente funcional.

El objetivo central no fue solo crear un bucle de juego interactivo con enemigos y gestión de recursos, sino investigar y aplicar el concepto de ***Mónada*** como una estructura de control para gestionar la complejidad creciente del estado mutable.

En clases se definió que las Mónadas aplican una función a un valor en un contexto para entregar un nuevo valor en el mismo contexto. Identificamos que nuestro "contexto" principal era la ***transformación del estado del juego*** (`GameState`).

> *Forma recomendada por la misma [documentación de *"monadeState"*](https://wiki.haskell.org/State_Monad) entregada para la tarea, especificamente en [*Complete and Concrete Example 1*](https://wiki.haskell.org/State_Monad#Complete_and_Concrete_Example_1).*

El desarrollo del proyecto siguió una ***curva de complejidad ascendente***. Como se evidencia en el historial de cambios, partimos de un prototipo básico de movimiento hasta llegar a sistemas complejos como colisiones SAT, partículas y gestión de inventarios.

> *Para más información sobre la evolución cronológica de estas mecánicas, revisar el **[CHANGELOG.md](CHANGELOG.md)**.*

Esta escalabilidad técnica evidenció rápidamente las limitaciones de encadenar funciones puras manualmente. La necesidad de "pasar el estado" explícitamente entre cientos de líneas de lógica de física y renderizado nos llevó a adoptar la **Monad State** y la notación `do`no solo como un requisito académico, sino como una solución arquitectónica necesaria para evitar la "cascada de declaraciones" y permitir una sintaxis imperativa dentro de la pureza de Haskell.

# 🛠️ Refactorización y Uso de Monad State: El Caso del Jugador.

Durante el desarrollo y hasta la versión `v1.0.0.0`, no se estaba usando `monadeState` de una forma "*Ad Hoc*", y esta se limitaba a manejar el `GameLoop`.

> *El **[Historial de commits](https://github.com/ShutUp-Pls/DEV_InfoAdventures/commits/main/)** apunta a la versión donde se hicieron los cambios comentados (Revisar el **[CHANGELOG.md](CHANGELOG.md)** para mas información de un commit especifico).*

No fue hasta la refactorización `v1.0.1.0` → `v1.0.1.1` donde identificamos un área crítica en la programación puramente funcional clásica que estaba generando código difícil de mantener y extender.

1. **El Problema: "Cascada de declaraciones dependientes en un lenguaje fuertemente tipado"**

En la implementación de la versión `v1.0.1.0` del movimiento del jugador (`Personajes.Jugador.moverJugador`), nos encontramos con un patrón de "cascada" de *let bindings*. Cada línea de lógica transformaba el dato y generaba una nueva variable temporal que debía ser pasada con cuidado a la siguiente función.

```haskell
moverJugador :: Types.Input -> PType.Jugador -> [GType.Box] -> PType.Jugador
moverJugador input jugadorActual mapObstaculos = 
    let 
        entidadInicial  = jugadorActual LMi.^. PType.jugEnt
        runFactor       = jugadorActual LMi.^. PType.factCorrer
        entidadRotada   = FMen.girarEntidadPorTeclado input entidadInicial
        velBase         = entidadRotada LMi.^. GType.entMov . GType.movVel
        anguloActual    = entidadRotada LMi.^. GType.entBox . GType.boxAng
        magnitud        = FAng.magnitudPorTeclado input velBase runFactor anguloActual
        vecDir          = FAng.anguloAVector anguloActual
        velIntencion    = vecDir LV.^* magnitud
        jugadorFinal    = FMen.moverEntidad velIntencion mapObstaculos entidadRotada
    in  jugadorActual LMi.& PType.jugEnt LMi..~ jugadorFinal
```
Además de la enorme ***Verbosidad***, si una función cambiaba su tipo de salida (por ejemplo: `FMen.girarEntidadPorTeclado`), entonces ***todas las operaciones hacía abajo debían ser revisadas y modificadas*** para coincidir con este tipo (*Propio de ser un lenguaje fuertemente tipado y de tipos estaticos.*). Esto, pensando en el futuro, ***reducía enormemente la escalabilidad*** y la posibilidad de hacer grandes cambios sin preocuparse de dañar por completo el código.

---

2. **Candidato para Monad State**

Basándonos en lo visto en clases y en la documentación entregada, donde las Mónadas introducen un contexto para aplicar funciones, identificamos que aquí un contexto necesario y *diferente al loop del juego* era → ***el estado mutable del tipo Jugador***.

También, la notación `do` de las monadas permite ***encadenar operaciones*** secuenciales sobre ese contexto, pudiendo evitar la *cascada de declaraciones*.

Sabiendo esto, la pregunta era: *¿Podemos aprovechar estas caracteristicas para nuestro `moverJugador`?*

- `moverJugador` transforma `PType.Jugador` → `PType.Jugador`.✅
- Dentro de `moverJugador`:
    - `FMen.moverEntidad`
    - `FMen.girarEntidadPorTeclado`
    Son también `PType.Jugador` → `PType.Jugador`.✅

Esto encaja con la Mónada State, que permite encadenar transformaciones sobre un estado implícito sin cargar manualmente el valor actualizado en cada paso.

---

3. **Aplicación de la Mónada**

*Repatriamos* las funciones puras de `Fisica.MovEntidad` al modulo `Personajes.Jugador` y las convertimos en acciones monádicas ***transformamdo sus firmas como:***

**Antes (Pura)**
```haskell
girarEntidadPorTeclado :: Types.Input -> GType.Entidad -> GType.Entidad
moverEntidad :: SDL.V2 Float -> [GType.Box] -> GType.Entidad -> GType.Entidad
```

**Después (Monádica)**
```haskell
girarJugadorM :: Types.Input -> CMS.State PType.Jugador ()
desplazarJugadorM :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
```

Esto permite:

- Leer partes del jugador con `CMS.gets`.
- Modificar el estado con `CMS.modify` (En nuestro caso, lenses: `.=`, `%=`).
- Eliminar variables temporales y cascadas de `=`.
---

4. **Resultado Final**

La nueva función `moverJugador` (ahora monádica) expresa ***acciones secuenciales en lugar de transformaciones de datos explícitas***, reduciendo **exageradamente** el flujo de los datos sin cambiar realmente la logica de las funciones que la componen, solo sus firmas y notación.

```haskell
-- Código Refactorizado (Estilo Monádico)
moverJugador :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
moverJugador input mapObstaculos = do
    girarJugadorM input
    desplazarJugadorM input mapObstaculos
```
Así, ***la posibilidad de extender y escalar el movimiento*** hacía otro estado del jugador (por ejemplo *"Saltar"*) se vería ***ampliamente facilitado*** al no necesitar preocuparnos de la entrada o la salida de las acciones que le subyancen. A diferencia de la cascada de declaraciones que teníamos al principio donde, ***si no eramos cuidadosos*** con los tipos de las entradas y salidas de las acciones circundantes, ***se nos caía todo***.

En resumen, al hacer este cambio:

- ***Eliminamos el "Pipeline"*** del paso de datos.
- ***Aumentamos la legibilidad*** mediante notación `do`.
- Permite un ***estilo casi imperativo***, sin renunciar a la pureza funcional y la seguridad de tipos de Haskell.

---

~~5.-***¿Y la extracción de datos?***~~

Antes, manejo de datos ***explicito***:
```haskell
...     velBase         = entidadRotada LMi.^. GType.entMov . GType.movVel
        anguloActual    = entidadRotada LMi.^. GType.entBox . GType.boxAng
        magnitud        = FAng.magnitudPorTeclado input velBase runFactor anguloActual
        vecDir          = FAng.anguloAVector anguloActual
        velIntencion    = vecDir LV.^* magnitud
...     jugadorFinal    = FMen.moverEntidad velIntencion mapObstaculos entidadRotada
```
Ahora, manejo de datos ***implicito***:
```haskell
desplazarJugadorM :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
desplazarJugadorM input mapObstaculos = do
    jugador <- CMS.get
    let entidad      = jugador LMi.^. PType.jugEnt
        runFactor    = jugador LMi.^. PType.factCorrer
        anguloActual = entidad LMi.^. GType.entBox . GType.boxAng
        velBase      = entidad LMi.^. GType.entMov . GType.movVel
...
```
Cómo cada `monadeState` lo que hace es ***cambiar el estado***, llamese *"los datos del jugador"*, la siguiente `monadeState` simplemente ***recupera esos datos de estado ya mutado*** y opera con ellos desde el principio en su definición. Nos deshacemos de todo el manejo de datos intermedio gracias a esto.

# Conclusión: La Arquitectura del GameLoop

La refactorización detallada en el ***caso del Jugador*** no fue un hecho aislado, sino el ***modelo para la reestructuración completa del bucle principal del juego*** en la versión `v-2.0.0.0`.

En el módulo `Juego.hs`, la función `actualizarJuego` actúa como el gran orquestador, operando bajo la mónada principal `CMS.State Types.GameState ()`.

Al observar la función `ejecutarJugabilidad`, nos encontramos con una estructura que, a primera vista, ***se aleja mucho*** de lo que uno entiende por ***paradigma funcional*** y se puede identificar más con lo que es un ***paradigma imperativo*** (Has A → Has B → Has C...).

```haskell
ejecutarJugabilidad :: Types.Input -> CMS.State Types.GameState ()
ejecutarJugabilidad input = do
    gestionarCambioArma input
    procesarBuffsM
    verificarColisionJugadorZombies
    vivo <- verificarJugadorVivo

    CMo.when vivo $ do
        moverJugadorM input
        gestionarItemsM
        procesarDisparoM input
        actualizarParticulasM
        moverZombiesM
        verificarColisionParticulasZombies
        limpiarZombiesMuertosM
        actualizarCamaraM input
```

***¿Dondé estan las declaraciones? ¿De que tipo son?***. Estas dudas son precisamente la ventaja arquitectónica que buscábamos. Cada una de estas acciones es una ***sub-mónada*** independiente que encapsula un aspecto específico del **Contexto Global (`GameState`)**.

Si revisamos las firmas de estas funciones auxiliares definidas en el mismo módulo, notamos un patrón idéntico:

  * `moverJugadorM :: Types.Input -> CMS.State Types.GameState ()`
  * `gestionarItemsM :: CMS.State Types.GameState ()`
  * `procesarDisparoM :: Types.Input -> CMS.State Types.GameState ()`
  * `moverZombiesM :: CMS.State Types.GameState ()`
  * `actualizarCamaraM :: Types.Input -> CMS.State Types.GameState ()`

**¿Por qué esto garantiza la escalabilidad?**

Todas estas funciones ***comparten el mismo contexto:*** `CMS.State Types.GameState ()`. Esto significa que tienen acceso implícito a *todo* el estado del juego (mapa, enemigos, tiempo, RNG), pero ***solo modifican lo que les concierne***.

Gracias a esto, si mañana necesitamos añadir una nueva mecánica, como por ejemplo un sistema de **"Clima"** o **"Gravedad"**, basta con:

1.  Definir la nueva lógica con la misma firma: `aplicarClimaM :: CMS.State Types.GameState ()`.
2.  Insertarla en el bloque `do` de `ejecutarJugabilidad`.

***No sería necesario reescribir el flujo de datos, ni modificar los argumentos*** de `moverJugadorM` o `moverZombiesM` para que "transporten" los datos del clima. ***Hemos superado el problema de la "cascada de declaraciones"***, cumpliendo así con el objetivo académico y práctico de utilizar la `Monad State` para gestionar la complejidad de un sistema evolutivo.