module Dominio.Evaluacion exposing
    ( Estado(..)
      -- el tipo Y sus tres constructores
    , LetraEvaluada
    , evaluar
    )

{-| Compara un intento contra la palabra objetivo y decide el estado
de cada letra.
-}

-- Dict es un diccionario clave/valor. Lo usamos como inventario:
-- de la letra 'a' cuántas quedan disponibles.

import Dict exposing (Dict)
import Dominio.Palabra as Palabra exposing (Palabra)


{-| Los tres colores posibles de una casilla.

Al ser un tipo cerrado de tres variantes, es IMPOSIBLE que una letra quede
en un cuarto estado o sin estado.

-}
type Estado
    = Correcta -- verde
    | PosicionIncorrecta -- amarillo
    | Ausente -- gris


{-| Una letra junto con su color.

`type alias` sobre un registro: no es un tipo nuevo, es un nombre corto
para esa forma. Elm genera automáticamente un constructor con el mismo
nombre, así que `LetraEvaluada 'a' Correcta` construye el registro.

-}
type alias LetraEvaluada =
    { letra : Char
    , estado : Estado
    }


{-| La función principal.

Recibe un REGISTRO con dos campos en lugar de dos parámetros sueltos. Así
es imposible confundir el orden y pasar el intento donde va el objetivo:
el compilador exige los nombres.

Fíjate en el parámetro `{ objetivo, intento }`: eso es desestructuración.
Saca los dos campos del registro y los deja disponibles como nombres.

-}
evaluar : { objetivo : Palabra, intento : Palabra } -> List LetraEvaluada
evaluar { objetivo, intento } =
    let
        letrasObjetivo =
            Palabra.aLetras objetivo

        letrasIntento =
            Palabra.aLetras intento

        -- `List.map2` recorre DOS listas a la vez y combina cada par.
        -- `Tuple.pair` los junta en una tupla.
        -- Resultado: [ ('s','c'), ('a','a'), ('l','n'), ... ]
        -- Cada tupla es (letra del intento, letra del objetivo) en esa posición.
        parejas =
            List.map2 Tuple.pair letrasIntento letrasObjetivo

        -- EL INVENTARIO. Aquí está la primera pasada, de forma implícita:
        -- nos quedamos solo con las parejas que NO coinciden (`i /= o`),
        -- tomamos la letra del objetivo de cada una (`Tuple.second`),
        -- y las contamos.
        -- Las que sí coincidían ya se llevaron su verde y no entran aquí.
        inventario =
            parejas
                |> List.filter (\( i, o ) -> i /= o)
                |> List.map Tuple.second
                |> contar
    in
    segundaPasada inventario parejas


{-| La segunda pasada, recorriendo las parejas de izquierda a derecha.

Esta función es RECURSIVA: se llama a sí misma con una lista más corta.
En C esto sería un `while` con un índice; en Elm no hay ciclos.

-}
segundaPasada : Dict Char Int -> List ( Char, Char ) -> List LetraEvaluada
segundaPasada inventario parejas =
    case parejas of
        -- Caso base: no quedan parejas, devolvemos la lista vacía.
        -- Sin este caso, la recursión no terminaría nunca.
        [] ->
            []

        -- Caso recursivo: `(intento, objetivo)` es la primera pareja
        -- y `resto` son las demás.
        ( intento, objetivo ) :: resto ->
            if intento == objetivo then
                -- Verde. No consume inventario, porque esa letra ya se
                -- descontó al construirlo.
                -- El `::` pega este resultado al frente de lo que devuelva
                -- la llamada recursiva.
                LetraEvaluada intento Correcta
                    :: segundaPasada inventario resto

            else if disponible intento inventario then
                -- Amarillo. Y aquí está la clave: pasamos a la siguiente
                -- llamada un inventario CON UNA MENOS de esa letra.
                LetraEvaluada intento PosicionIncorrecta
                    :: segundaPasada (consumir intento inventario) resto

            else
                -- Gris. El inventario pasa igual.
                LetraEvaluada intento Ausente
                    :: segundaPasada inventario resto



-- INVENTARIO (funciones privadas)


{-| Cuenta cuántas veces aparece cada letra.

`List.foldl` recorre la lista acumulando un resultado. Es la iteración
definida: sabe exactamente cuántas vueltas va a dar.

La función que le pasamos recibe (elemento, acumulado) y devuelve
el acumulado nuevo. Empezamos con `Dict.empty`.

-}
contar : List Char -> Dict Char Int
contar letras =
    List.foldl
        (\letra acumulado ->
            -- `Dict.update` recibe la clave y una función que transforma
            -- el valor actual (que es un Maybe, porque puede no existir).
            -- `Maybe.withDefault 0` -> si no existe, cuenta 0
            -- `>> (+) 1`            -> le suma 1
            -- `>> Just`             -> lo vuelve a envolver en Maybe
            Dict.update letra (Maybe.withDefault 0 >> (+) 1 >> Just) acumulado
        )
        Dict.empty
        letras


{-| ¿Queda al menos una de esta letra en el inventario?
-}
disponible : Char -> Dict Char Int -> Bool
disponible letra inventario =
    Dict.get letra inventario
        -- devuelve Maybe Int
        |> Maybe.withDefault 0
        -- si no está, es 0
        |> (\n -> n > 0)


{-| Gasta una unidad de esa letra.

OJO: no modifica el inventario. Devuelve un diccionario NUEVO con una
unidad menos. El original queda intacto.

-}
consumir : Char -> Dict Char Int -> Dict Char Int
consumir letra inventario =
    Dict.update letra (Maybe.map (\n -> n - 1)) inventario
