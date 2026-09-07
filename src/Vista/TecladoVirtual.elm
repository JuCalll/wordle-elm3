module Vista.TecladoVirtual exposing (ver)

import Dict exposing (Dict)
import Dominio.Evaluacion exposing (Estado)
import Dominio.Teclado as Teclado
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Vista.Estilos as Estilos


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
        (List.indexedMap (verFila config) Teclado.disposicion)


verFila : Config msg -> Int -> List Char -> Html msg
verFila config indice letras =
    let
        teclasLetra =
            List.map (verTeclaLetra config) letras

        contenido =
            if indice == 2 then
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


verTeclaLetra : Config msg -> Char -> Html msg
verTeclaLetra config letra =
    let
        color =
            Dict.get letra config.estados
                |> Maybe.map Estilos.colorDeEstado
                |> Maybe.withDefault Estilos.acento
    in
    button
        (onClick (config.alPresionarLetra letra) :: estiloTecla color 42)
        [ text (String.fromChar letra |> String.toUpper) ]


verTeclaAncha : String -> msg -> Html msg
verTeclaAncha etiqueta mensaje =
    button
        (onClick mensaje :: estiloTecla Estilos.acento 68)
        [ text etiqueta ]


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
