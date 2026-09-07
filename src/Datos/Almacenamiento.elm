module Datos.Almacenamiento exposing (codificar, decodificar)

import Dict
import Dominio.Estadisticas as Estadisticas exposing (Estadisticas)
import Json.Decode as Decode
import Json.Encode as Encode


codificar : Estadisticas -> Encode.Value
codificar estadisticas =
    Encode.object
        [ ( "jugadas", Encode.int (Estadisticas.jugadas estadisticas) )
        , ( "ganadas", Encode.int (Estadisticas.ganadas estadisticas) )
        , ( "rachaActual", Encode.int (Estadisticas.rachaActual estadisticas) )
        , ( "mejorRacha", Encode.int (Estadisticas.mejorRacha estadisticas) )
        , ( "distribucion"
          , Estadisticas.distribucion estadisticas
                |> Dict.toList
                |> Encode.list parejaCodificada
          )
        ]


decodificar : Decode.Value -> Estadisticas
decodificar valor =
    Decode.decodeValue decodificador valor
        |> Result.withDefault Estadisticas.vacias



-- interno


parejaCodificada : ( Int, Int ) -> Encode.Value
parejaCodificada ( intento, cantidad ) =
    Encode.object
        [ ( "intento", Encode.int intento )
        , ( "cantidad", Encode.int cantidad )
        ]


decodificador : Decode.Decoder Estadisticas
decodificador =
    Decode.map5
        (\j g r m d ->
            Estadisticas.desdePartes
                { jugadas = j
                , ganadas = g
                , rachaActual = r
                , mejorRacha = m
                , distribucion = d
                }
        )
        (Decode.field "jugadas" Decode.int)
        (Decode.field "ganadas" Decode.int)
        (Decode.field "rachaActual" Decode.int)
        (Decode.field "mejorRacha" Decode.int)
        (Decode.field "distribucion" (Decode.list parejaDecodificada))


parejaDecodificada : Decode.Decoder ( Int, Int )
parejaDecodificada =
    Decode.map2 Tuple.pair
        (Decode.field "intento" Decode.int)
        (Decode.field "cantidad" Decode.int)
