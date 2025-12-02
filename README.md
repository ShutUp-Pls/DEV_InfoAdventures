# Informe de Desarrollo:

### 🛠️ Refactorización y Uso de Monad State: El Caso del Jugador.

Durante el desarrollo y hasta la versión `v1.0.0.0`, no se estaba usando `monadeState` de una forma "*Ad Hoc*" (Comprobable a traves del historial de *Pusheos* en GitHub). No fue hasta la refactorización `v1.0.1.0` donde identificamos un área crítica en la programación puramente funcional clásica estaba generando código difícil de mantener y extender.

1. **El Problema:
"Cascada de declaraciones dependientes en un lenguaje fuertemente tipado"**

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
Además de la enorme *Verbosidad*, si una función cambiaba su tipo de salida (por ejemplo: `FMen.girarEntidadPorTeclado`), entonces todas las operaciones hacía abajo debían ser revisadas y modificadas para coincidir con este tipo (*Propio de ser un lenguaje fuertemente tipado y de tipos estaticos.*). Esto, pensando en el futuro, reducía enormemente la escalabilidad y la posibilidad de hacer grandes cambios sin preocuparse de dañar por completo el código.

---

2. **Candidato para Monad State**

Basándonos en lo visto en clases, donde las Mónadas introducen un contexto para aplicar funciones, identificamos que aquí el contexto necesario era *el estado mutable del tipo Jugador*.

También, la notación `do` de las monadas permite encadenar operaciones secuenciales sobre ese contexto, pudiendo evitar la *cascada de declaraciones*.

Sabiendo esto, la pregunta era: *¿Podemos aprovechar estas caracteristicas para nuestro `moverJugador`?*

- `moverJugador` transforma un `PType.Jugador` en otro `PType.Jugador`.✅
- Tanto `FMen.moverEntidad` como `FMen.girarEntidadPorTeclado` devuelven un estado del jugador.✅

Esto encaja con la Mónada State, que permite encadenar transformaciones sobre un estado implícito sin cargar manualmente el valor actualizado en cada paso.

---

3. **Aplicación de la Mónada**

*Repatriamos* las funciones puras de `Fisica.MovEntidad` al modulo `Personajes.Jugador` y las convertimos en acciones monádicas. Convirtiendolas de la siguiente forma:

**Transformación de firmas:**

```haskell
-- Antes (Pura)
girarEntidadPorTeclado :: Types.Input -> GType.Entidad -> GType.Entidad
moverEntidad :: SDL.V2 Float -> [GType.Box] -> GType.Entidad -> GType.Entidad

-- Después (Monádica)
girarJugadorM :: Types.Input -> CMS.State PType.Jugador ()
desplazarJugadorM :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
```

Esto permite:

- Leer partes del jugador con `CMS.gets`.
- Modificar el estado con `CMS.modify` (En nuestro caso, lenses: `.=`, `%=`).
- Eliminar variables temporales y cascadas de `=`.

Además, es más parecido a un paradigma imperativo donde:
- "*Dado un input, modifica al jugado*".
- "*Dado un input y el mapa, modifica al jugador*"
---

4. **Resultado Final**

La nueva función `moverJugador` (ahora monádica) expresa acciones secuenciales en lugar de transformaciones de datos explícitas, reduciendo **exageradamente** el flujo de los datos sin cambiar realmente la logica de las funciones que la componen, solo sus firmas.

```haskell
-- Código Refactorizado (Estilo Monádico)
moverJugador :: Types.Input -> [GType.Box] -> CMS.State PType.Jugador ()
moverJugador input mapObstaculos = do
    girarJugadorM input
    desplazarJugadorM input mapObstaculos
```
Así, la posibilidad de extender y escalar el movimiento hacía otro estado del jugador (por ejemplo *"Saltar"*) se vería ampliamente facilitado al no necesitar preocuparnos de la entrada o la salida de las acciones que le subyancen. A diferencia de la cascada de declaraciones que teníamos al principio donde si no eramos cuidadoso con las entradas y saalidas de las acciones que subyacen a este nuevo estado del jugador, se nos caía todo.

En resumen, al hacer este cambio:

- Eliminamos el "Pipeline" del paso de datos.
- Aumentamos la legibilidad mediante notación `do`.
- Permite un estilo casi imperativo sin renunciar a la pureza funcional y la seguridad de tipos de Haskell.

---

~~5.-***¿Y la extracción de datos?***~~

Antes, manejo de datos explicito:
```haskell
...     velBase         = entidadRotada LMi.^. GType.entMov . GType.movVel
        anguloActual    = entidadRotada LMi.^. GType.entBox . GType.boxAng
        magnitud        = FAng.magnitudPorTeclado input velBase runFactor anguloActual
        vecDir          = FAng.anguloAVector anguloActual
        velIntencion    = vecDir LV.^* magnitud
...     jugadorFinal    = FMen.moverEntidad velIntencion mapObstaculos entidadRotada
```
Ahora, manejo de datos implicito:
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
Cómo cada `monadeState` lo que hace es cambiar el estado, llamese *"los datos del jugador"*, la siguiente `monadeState` simplemente recupera esos datos de estado ya mutado y opera con ellos desde el principio en su definición. Nos deshacemos de todo el manejo de datos intermedio gracias a esto.