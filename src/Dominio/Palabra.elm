-- El nombre del módulo debe coincidir con la ruta: src/Dominio/Palabra.elm
-- El bloque `exposing` es la lista de lo que sale al exterior.


module Dominio.Palabra exposing
    ( Error(..)
      -- el tipo Error Y sus constructores (eso son los ..)
    , Palabra
      -- el TIPO, pero NO su constructor (ver abajo)
    , aLetras
    , aTexto
    , descripcionError
    , desdeTexto
    , esLetra
    , longitudRequerida
    , porDefecto
    )

{-| Representa una palabra válida del juego: exactamente cinco letras
del alfabeto español, sin tildes.

La única forma de obtener una `Palabra` es a través de `desdeTexto`,
que valida la entrada. Si tienes una `Palabra`, es válida.

-}

-- Set es un conjunto: sirve para preguntar "¿está este elemento?" rápido.
-- `exposing (Set)` nos deja escribir `Set Char` en las anotaciones.

import Set exposing (Set)


{-| TIPO OPACO. Este es el concepto más importante del módulo.

`Palabra` envuelve una lista de caracteres. Pero como arriba expusimos
`Palabra` y NO `Palabra(..)`, el constructor queda privado: ningún otro
archivo del proyecto puede escribir `Palabra ['x']` y saltarse la validación.

Es el reemplazo de `private` en Elm.

-}
type Palabra
    = Palabra (List Char)


{-| Las dos razones por las que un texto puede no ser una palabra válida.
Cada variante lleva un dato: el número de letras que tenía, o el carácter
que sobraba.
-}
type Error
    = LongitudIncorrecta Int
    | CaracterNoValido Char


{-| Una constante. Se usa en todo el proyecto en lugar del número 5 suelto,
para que cambiar la longitud del juego sea tocar una sola línea.
-}
longitudRequerida : Int
longitudRequerida =
    5


{-| CONSTRUCTOR INTELIGENTE: la única puerta de entrada al tipo.

Recibe texto crudo (lo que el usuario escribió) y devuelve `Ok palabra`
si es válida, o `Err razon` si no lo es.

-}
desdeTexto : String -> Result Error Palabra
desdeTexto texto =
    let
        -- Normalizamos antes de validar, leyendo de arriba hacia abajo:
        letras =
            texto
                |> String.trim
                -- quita espacios de los extremos
                |> String.toLower
                -- "GATOS" pasa a "gatos"
                |> String.toList

        -- "gatos" pasa a ['g','a','t','o','s']
    in
    -- Primera comprobación: la longitud.
    -- `/=` es "distinto de".
    if List.length letras /= longitudRequerida then
        Err (LongitudIncorrecta (List.length letras))

    else
        -- Segunda comprobación: que todos los caracteres sean letras válidas.
        -- `List.filter` se queda con los que cumplen la condición.
        -- `not << esLetra` es "la negación de esLetra", es decir:
        -- nos quedamos con los caracteres que NO son letras válidas.
        case List.filter (not << esLetra) letras of
            -- Si la lista de inválidos tiene al menos un elemento,
            -- `invalida` es el primero y `_` es el resto (que ignoramos).
            invalida :: _ ->
                Err (CaracterNoValido invalida)

            -- Si la lista de inválidos está vacía, todo está bien.
            -- Aquí SÍ podemos usar el constructor `Palabra`, porque estamos
            -- dentro de su propio módulo.
            [] ->
                Ok (Palabra letras)


{-| Convierte de vuelta a texto.

Fíjate en el parámetro: `(Palabra letras)`. Eso es desempaquetado directo
en la firma: como `Palabra` tiene un solo constructor, Elm permite abrirlo
ahí mismo y quedarnos con la lista de dentro.

-}
aTexto : Palabra -> String
aTexto (Palabra letras) =
    String.fromList letras


{-| Devuelve las letras sueltas. Lo va a usar el módulo de evaluación.
-}
aLetras : Palabra -> List Char
aLetras (Palabra letras) =
    letras


{-| Traduce un error a un mensaje para el usuario.

El `case` cubre las dos variantes de `Error`. Si mañana agregamos una
tercera, el compilador va a exigir que la manejemos aquí.

-}
descripcionError : Error -> String
descripcionError error =
    case error of
        LongitudIncorrecta n ->
            "La palabra debe tener "
                ++ String.fromInt longitudRequerida
                ++ " letras, y tiene "
                ++ String.fromInt n
                ++ "."

        CaracterNoValido caracter ->
            "El carácter '"
                ++ String.fromChar caracter
                ++ "' no es una letra válida."


{-| Un valor de respaldo.

Hay sitios donde el sistema de tipos exige una `Palabra` y todavía no hay
ninguna disponible (al arrancar la aplicación, antes del sorteo). En vez de
inventar un `null` que no existe en Elm, damos una palabra real.

Este módulo es el único que puede construir una `Palabra` sin pasar por
`desdeTexto`, porque es el dueño del constructor.

-}
porDefecto : Palabra
porDefecto =
    Palabra (String.toList "gatos")


{-| ¿Es este carácter una letra del alfabeto que aceptamos?

La exponemos porque el módulo `Partida` la necesita para filtrar las teclas
que el usuario presiona.

-}
esLetra : Char -> Bool
esLetra caracter =
    Set.member (Char.toLower caracter) alfabeto



-- INTERNO
-- Todo lo que va debajo de aquí NO está en el `exposing` de arriba,
-- así que es privado del módulo.


{-| El alfabeto español sin tildes. La ñ sí está.

Se construye una sola vez y se reutiliza. Un `Set` responde
"¿está este elemento?" mucho más rápido que una lista.

-}
alfabeto : Set Char
alfabeto =
    "abcdefghijklmnñopqrstuvwxyz"
        |> String.toList
        |> Set.fromList
