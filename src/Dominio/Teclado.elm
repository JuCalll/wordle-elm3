module Dominio.Teclado exposing
    ( disposicion
    , estados
    )

{-| El estado visual del teclado.

Este módulo NO guarda nada: deriva el color de cada tecla a partir del
historial de intentos. Así el teclado no puede desincronizarse del tablero,
porque ambos leen la misma fuente.

-}

import Dict exposing (Dict)
import Dominio.Evaluacion exposing (Estado(..), LetraEvaluada)


{-| Distribución QWERTY en español, con la ñ en su lugar habitual.

Una lista de listas: cada sublista es una fila del teclado.

-}
disposicion : List (List Char)
disposicion =
    [ String.toList "qwertyuiop"
    , String.toList "asdfghjklñ"
    , String.toList "zxcvbnm"
    ]


{-| Calcula el color de cada tecla usada.

Recibe el historial completo de intentos y devuelve un diccionario
letra -> color. Las letras que nunca se usaron simplemente no aparecen.

-}
estados : List (List LetraEvaluada) -> Dict Char Estado
estados historial =
    historial
        -- `List.concat` aplana la lista de listas en una sola lista
        -- con todas las letras evaluadas de todos los intentos.
        |> List.concat
        -- Y las recorremos acumulando el diccionario.
        |> List.foldl acumular Dict.empty



-- INTERNO


{-| Agrega una letra evaluada al mapa, conservando el mejor estado.
-}
acumular : LetraEvaluada -> Dict Char Estado -> Dict Char Estado
acumular evaluada mapa =
    Dict.update evaluada.letra (conservarMejor evaluada.estado) mapa


{-| Decide qué estado queda cuando ya había uno.

El segundo parámetro es `Maybe Estado` porque la letra puede no estar
todavía en el diccionario.

-}
conservarMejor : Estado -> Maybe Estado -> Maybe Estado
conservarMejor nuevo anterior =
    case anterior of
        Nothing ->
            -- Primera vez que aparece esta letra.
            Just nuevo

        Just previo ->
            if prioridad nuevo > prioridad previo then
                Just nuevo

            else
                Just previo


{-| Orden de preferencia entre estados.

Al ser un `case` sobre un tipo cerrado y sin caso comodín, agregar un
estado nuevo en el futuro obliga al compilador a exigir su prioridad aquí.
Es el principio abierto/cerrado en su forma funcional.

-}
prioridad : Estado -> Int
prioridad estado =
    case estado of
        Ausente ->
            0

        PosicionIncorrecta ->
            1

        Correcta ->
            2
