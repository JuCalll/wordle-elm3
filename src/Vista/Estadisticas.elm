module Vista.Estadisticas exposing (ver)

{-| El panel de estadísticas que aparece al terminar una partida.
-}

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


{-| Los cuatro números de arriba.
-}
verResumen : Estadisticas -> Html msg
verResumen estadisticas =
    div
        [ style "display" "flex", style "gap" "22px" ]
        [ dato (Estadisticas.jugadas estadisticas) "Jugadas"
        , dato (Estadisticas.porcentajeVictorias estadisticas) "% Victorias"
        , dato (Estadisticas.rachaActual estadisticas) "Racha"
        , dato (Estadisticas.mejorRacha estadisticas) "Mejor"
        ]


{-| Un número grande con su etiqueta pequeña debajo.
-}
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


{-| Las seis barras: cuántas veces se ganó en cada intento.
-}
verDistribucion : Estadisticas -> Html msg
verDistribucion estadisticas =
    let
        conteos =
            Estadisticas.distribucion estadisticas

        -- El máximo se usa para escalar las barras: la más alta ocupa
        -- el 100% del ancho y las demás en proporción.
        -- `List.maximum` devuelve Maybe porque la lista puede estar vacía.
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
        -- `List.range 1 6` genera [1,2,3,4,5,6]: una barra por intento
        -- posible, aunque nunca se haya ganado en ese número.
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
            -- Sin este caso, sería una división por cero la primera vez.
            if maximo == 0 then
                0

            else
                toFloat cantidad / toFloat maximo

        -- `max 8` garantiza que la barra siempre se vea, aunque sea cero.
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
