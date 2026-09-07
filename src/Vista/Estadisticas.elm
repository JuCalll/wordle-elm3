module Vista.Estadisticas exposing (ver)

import Dict
import Dominio.Estadisticas as Estadisticas exposing (Estadisticas)
import Dominio.Partida as Partida
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Vista.Estilos as Estilos


ver : Estadisticas -> Html msg
ver estadisticas =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "gap" "16px"
        , style "margin-top" "8px"
        ]
        [ verResumen estadisticas
        , verDistribucion estadisticas
        ]


verResumen : Estadisticas -> Html msg
verResumen estadisticas =
    div
        [ style "display" "flex", style "gap" "22px" ]
        [ dato (Estadisticas.jugadas estadisticas) "Jugadas"
        , dato (Estadisticas.porcentajeVictorias estadisticas) "% Victorias"
        , dato (Estadisticas.rachaActual estadisticas) "Racha"
        , dato (Estadisticas.mejorRacha estadisticas) "Mejor"
        ]


dato : Int -> String -> Html msg
dato valor etiqueta =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ div [ style "font-size" "26px", style "font-weight" "bold" ]
            [ text (String.fromInt valor) ]
        , div [ style "font-size" "11px", style "color" Estilos.tenue ]
            [ text etiqueta ]
        ]


verDistribucion : Estadisticas -> Html msg
verDistribucion estadisticas =
    let
        conteos =
            Estadisticas.distribucion estadisticas

        maximo =
            Dict.values conteos
                |> List.maximum
                |> Maybe.withDefault 0
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "4px"
        , style "width" "260px"
        ]
        (List.range 1 Partida.maximoIntentos
            |> List.map
                (\intento ->
                    barra intento
                        (Dict.get intento conteos |> Maybe.withDefault 0)
                        maximo
                )
        )


barra : Int -> Int -> Int -> Html msg
barra intento cantidad maximo =
    let
        proporcion =
            if maximo == 0 then
                0

            else
                toFloat cantidad / toFloat maximo

        ancho =
            max 8 (round (proporcion * 100))
    in
    div
        [ style "display" "flex", style "align-items" "center", style "gap" "8px" ]
        [ div [ style "font-size" "13px", style "width" "12px" ]
            [ text (String.fromInt intento) ]
        , div
            [ style "background-color"
                (if cantidad > 0 then
                    "#538d4e"

                 else
                    Estilos.borde
                )
            , style "width" (String.fromInt ancho ++ "%")
            , style "padding" "2px 8px"
            , style "font-size" "13px"
            , style "font-weight" "bold"
            , style "text-align" "right"
            , style "border-radius" "3px"
            ]
            [ text (String.fromInt cantidad) ]
        ]
