module Vista.TecladoVirtual exposing (ver)

{-| El teclado en pantalla.

Recibe los manejadores como parámetros en lugar de conocer el tipo `Msg`
de la aplicación. Así este módulo es reutilizable y no depende de `Main`.

-}

import Dict exposing (Dict)
import Dominio.Evaluacion exposing (Estado)
import Dominio.Teclado as Teclado
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Vista.Estilos as Estilos


{-| Todo lo que este módulo necesita para pintarse.

Los tres últimos campos son MENSAJES, no funciones que hagan algo: cuando
el usuario haga clic, Elm le entregará ese mensaje a `Main`.

-}
type alias Config msg =
    { estados : Dict Char Estado
    , alPresionarLetra : Char -> msg
    , alBorrar : msg
    , alEnviar : msg
    }


ver : Config msg -> Html msg
ver config =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "8px"
        , style "align-items" "center"
        , style "margin-top" "24px"
        ]
        -- `List.indexedMap` es como `List.map` pero además pasa el índice
        -- (0, 1, 2...). Lo necesitamos para saber cuál es la última fila.
        (List.indexedMap (verFila config) Teclado.disposicion)


{-| Pinta una fila. La tercera lleva además ENVIAR y borrar.
-}
verFila : Config msg -> Int -> List Char -> Html msg
verFila config indice letras =
    let
        teclasLetra =
            List.map (verTeclaLetra config) letras

        contenido =
            if indice == 2 then
                -- `::` pone ENVIAR al principio, `++ [...]` pone el
                -- borrar al final.
                -- "\u{232B}" es el carácter Unicode de la flecha de borrado.
                (verTeclaAncha "ENVIAR" config.alEnviar :: teclasLetra)
                    ++ [ verTeclaAncha "⌫" config.alBorrar ]

            else
                teclasLetra
    in
    div
        [ style "display" "flex"
        , style "gap" "6px"
        ]
        contenido


{-| Una tecla de letra, con su color según el historial.
-}
verTeclaLetra : Config msg -> Char -> Html msg
verTeclaLetra config letra =
    let
        color =
            Dict.get letra config.estados
                -- Maybe Estado
                |> Maybe.map Estilos.colorDeEstado
                -- Si la letra no se ha usado, color neutro.
                |> Maybe.withDefault Estilos.acento
    in
    button
        -- `onClick` produce el mensaje cuando el usuario hace clic.
        -- `::` lo agrega a la lista de estilos.
        (onClick (config.alPresionarLetra letra) :: estiloTecla color 42)
        [ text (String.fromChar letra |> String.toUpper) ]


verTeclaAncha : String -> msg -> Html msg
verTeclaAncha etiqueta mensaje =
    button
        (onClick mensaje :: estiloTecla Estilos.acento 68)
        [ text etiqueta ]


{-| Los estilos comunes de todas las teclas, en un solo sitio.
-}
estiloTecla : String -> Int -> List (Html.Attribute msg)
estiloTecla color ancho =
    [ style "background-color" color
    , style "color" Estilos.colorTexto
    , style "border" "none"
    , style "border-radius" "6px"
    , style "width" (String.fromInt ancho ++ "px")
    , style "height" "54px"
    , style "font-size" "14px"
    , style "font-weight" "bold"
    , style "cursor" "pointer"
    , style "font-family" "inherit"
    ]
