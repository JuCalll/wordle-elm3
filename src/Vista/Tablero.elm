module Vista.Tablero exposing (ver)

import Dominio.Estadisticas exposing (Estadisticas)
import Dominio.Evaluacion exposing (Estado, LetraEvaluada)
import Dominio.Palabra as Palabra
import Dominio.Partida as Partida exposing (Estado(..), Partida)
import Dominio.Teclado as Teclado
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Vista.Estadisticas
import Vista.Estilos as Estilos
import Vista.TecladoVirtual


type alias Config msg =
    { partida : Partida
    , aviso : Maybe String
    , alPresionarLetra : Char -> msg
    , alBorrar : msg
    , alEnviar : msg
    , alReiniciar : msg
    , estadisticas : Estadisticas
    }


ver : Config msg -> Html msg
ver config =
    div
        [ style "background-color" Estilos.fondo
        , style "color" Estilos.colorTexto
        , style "min-height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "padding" "16px"
        ]
        [ h1
            [ style "letter-spacing" "4px"
            , style "font-size" "28px"
            , style "margin" "8px 0 20px"
            ]
            [ text "WORDLE" ]
        , verCuadricula config.partida
        , verMensaje config
        , Vista.TecladoVirtual.ver
            { estados = Teclado.estados (Partida.intentos config.partida)
            , alPresionarLetra = config.alPresionarLetra
            , alBorrar = config.alBorrar
            , alEnviar = config.alEnviar
            }
        ]



-- cuadrícula


verCuadricula : Partida -> Html msg
verCuadricula partida =
    let
        completadas =
            List.map filaCompletada (Partida.intentos partida)

        enCurso =
            if Partida.estado partida == EnCurso then
                [ filaActual (Partida.actual partida) ]

            else
                []

        vacias =
            List.repeat (filasVacias partida) filaVacia
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "6px"
        ]
        (completadas ++ enCurso ++ vacias)


filasVacias : Partida -> Int
filasVacias partida =
    let
        usadas =
            List.length (Partida.intentos partida)

        enCurso =
            if Partida.estado partida == EnCurso then
                1

            else
                0
    in
    max 0 (Partida.maximoIntentos - usadas - enCurso)


filaCompletada : List LetraEvaluada -> Html msg
filaCompletada evaluadas =
    fila (List.map celdaEvaluada evaluadas)


filaActual : List Char -> Html msg
filaActual letras =
    let
        escritas =
            List.map celdaEscrita letras

        pendientes =
            List.repeat
                (Palabra.longitudRequerida - List.length letras)
                celdaVacia
    in
    fila (escritas ++ pendientes)


filaVacia : Html msg
filaVacia =
    fila (List.repeat Palabra.longitudRequerida celdaVacia)


fila : List (Html msg) -> Html msg
fila celdas =
    div [ style "display" "flex", style "gap" "6px" ] celdas



-- celdas


celdaEvaluada : LetraEvaluada -> Html msg
celdaEvaluada evaluada =
    celda
        (Estilos.colorDeEstado evaluada.estado)
        (Estilos.colorDeEstado evaluada.estado)
        (String.fromChar evaluada.letra)


celdaEscrita : Char -> Html msg
celdaEscrita letra =
    celda "transparent" Estilos.acento (String.fromChar letra)


celdaVacia : Html msg
celdaVacia =
    celda "transparent" Estilos.borde ""


celda : String -> String -> String -> Html msg
celda fondo colorBorde contenido =
    div
        [ style "width" "58px"
        , style "height" "58px"
        , style "background-color" fondo
        , style "border" ("2px solid " ++ colorBorde)
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "30px"
        , style "font-weight" "bold"
        , style "text-transform" "uppercase"
        , style "box-sizing" "border-box"
        ]
        [ text contenido ]



-- mensajes


verMensaje : Config msg -> Html msg
verMensaje config =
    div
        [ style "min-height" "80px"
        , style "margin-top" "16px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "gap" "10px"
        ]
        (case Partida.estado config.partida of
            EnCurso ->
                [ verAviso config.aviso ]

            Ganada ->
                [ texto "¡Correcto!"
                , Vista.Estadisticas.ver config.estadisticas
                , botonReiniciar config.alReiniciar
                ]

            Perdida ->
                [ texto
                    ("La palabra era: "
                        ++ String.toUpper
                            (Palabra.aTexto (Partida.objetivo config.partida))
                    )
                , Vista.Estadisticas.ver config.estadisticas
                , botonReiniciar config.alReiniciar
                ]
        )


verAviso : Maybe String -> Html msg
verAviso aviso =
    case aviso of
        Nothing ->
            text ""

        Just mensaje ->
            texto mensaje


texto : String -> Html msg
texto contenido =
    div [ style "font-size" "18px" ] [ text contenido ]


botonReiniciar : msg -> Html msg
botonReiniciar mensaje =
    button
        [ onClick mensaje
        , style "background-color" Estilos.colorTexto
        , style "color" Estilos.fondo
        , style "border" "none"
        , style "border-radius" "6px"
        , style "padding" "10px 22px"
        , style "font-size" "15px"
        , style "font-weight" "bold"
        , style "cursor" "pointer"
        ]
        [ text "Jugar de nuevo" ]
