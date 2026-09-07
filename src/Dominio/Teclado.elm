module Dominio.Teclado exposing
    ( disposicion
    , estados
    )

import Dict exposing (Dict)
import Dominio.Evaluacion exposing (Estado(..), LetraEvaluada)


disposicion : List (List Char)
disposicion =
    [ String.toList "qwertyuiop"
    , String.toList "asdfghjklñ"
    , String.toList "zxcvbnm"
    ]


estados : List (List LetraEvaluada) -> Dict Char Estado
estados historial =
    historial
        |> List.concat
        |> List.foldl acumular Dict.empty



-- interno


acumular : LetraEvaluada -> Dict Char Estado -> Dict Char Estado
acumular evaluada mapa =
    Dict.update evaluada.letra (conservarMejor evaluada.estado) mapa


conservarMejor : Estado -> Maybe Estado -> Maybe Estado
conservarMejor nuevo anterior =
    case anterior of
        Nothing ->
            Just nuevo

        Just previo ->
            if prioridad nuevo > prioridad previo then
                Just nuevo

            else
                Just previo


prioridad : Estado -> Int
prioridad estado =
    case estado of
        Ausente ->
            0

        PosicionIncorrecta ->
            1

        Correcta ->
            2
