## 🛠️ Refactorización y Uso de Monad State

Entre la versión `v1.0.0.0` y `v1.0.0.1`, tras un avance grande pero sin orden, tocó refactorizar el codigo. Durante este proceso hice 2 cosas:

### El Problema: "State Threading" Manual

Tras terminada la versión `v1.0.0.0`, algunas de las firmas del modulo (`Personajes.Zombie`) se veían algo así:

```haskell
-- Código original (threading manual)
updateEnemies :: PType.Jugador -> [PType.Zombie] -> [GType.Box] -> [PType.Zombie]
limpiarZombiesMuertos :: [PType.Zombie] -> [PType.Zombie]
limpiarMuertos :: [Zombie] -> [Zombie]
resolverColisionesEntreZombies :: [PType.Zombie] -> [PType.Zombie]
dañarZombieEnIndice :: Int -> Float -> [PType.Zombie] -> [PType.Zombie]
aplicarImpactoZombie :: Int -> Int -> Float -> [PType.Zombie] -> [PType.Zombie]
```
Y se usaban en todo el código de forma 
```haskell
-- Uso en el ciclo de juego:
```

Esto presentaba dos problemas:

1.  **Verbosidad:** Era necesario crear nombres de variables temporales para cada paso intermedio.
2.  **Propenso a errores:** Era fácil pasar `zombiesMovidos` en lugar de `zombiesColisionados` por error en una línea subsecuente.

### La Solución: Abstracción con Monad State

Basándonos en lo visto en clases, donde las Mónadas introducen un contexto para aplicar funciones, identificamos que aquí el contexto necesario era **el estado mutable de la lista de enemigos**.

Al igual que la notación `do` nos permite encadenar operaciones secuenciales evitando la anidación excesiva de `lambdas` o `case`, utilizamos `Control.Monad.State` para encapsular la lista de zombies.

**Refactorización Implementada:**

Definimos un tipo monádico para las operaciones de zombies:

```haskell
State [PType.Zombie] a
```

Esto transformó nuestras funciones de transformación (`[Zombie] -> [Zombie]`) en acciones monádicas (`ZombieM ()`):

```haskell
```

### Resultado: Composición Limpia

Gracias a la implementación de la instancia `Monad`, pude usar el operador `>>=` (bind) implícitamente a través de la notación `do`, permitiendo que el compilador se encargue de pasar el estado de una función a otra:

```haskell
```

Esta refactorización cumple con el requisito funcional de la tarea y demuestra el poder de las mónadas para abstraer la "fontanería" (plumbing) del paso de datos, permitiéndonos escribir código imperativo dentro de un lenguaje funcional puro.