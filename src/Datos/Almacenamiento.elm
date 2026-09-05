module Datos.Almacenamiento exposing (codificar, decodificar)

{-| Traducción entre el dominio y el formato JSON del navegador.

Este módulo solo TRADUCE. Quién envía los datos y por dónde es decisión de
`Main`, que es el único que declara los ports.

(Nota práctica: los ports DEBEN vivir en el módulo raíz de la aplicación.
Si se ponen en un módulo interno, `elm-test` no puede compilar el proyecto
y falla con un error que no explica nada.)

-}

import Dict
import Dominio.Estadisticas as Estadisticas exposing (Estadisticas)
import Json.Decode as Decode
import Json.Encode as Encode


{-| Convierte las estadísticas en un valor JSON listo para salir.

`Encode.object` recibe una lista de parejas (nombre del campo, valor
codificado).

-}
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
                -- Dict a lista de parejas
                |> Encode.list parejaCodificada
            -- cada pareja a JSON
          )
        ]


{-| Lee unas estadísticas guardadas.

Los datos que vienen del navegador NO son de fiar: pueden faltar, estar
corruptos o haber sido editados a mano. Si algo no cuadra, empezamos de
cero en lugar de romper el programa.

`Result.withDefault` hace exactamente eso: si la decodificación falla,
devuelve `Estadisticas.vacias`.

-}
decodificar : Decode.Value -> Estadisticas
decodificar valor =
    Decode.decodeValue decodificador valor
        |> Result.withDefault Estadisticas.vacias



-- INTERNO


parejaCodificada : ( Int, Int ) -> Encode.Value
parejaCodificada ( intento, cantidad ) =
    Encode.object
        [ ( "intento", Encode.int intento )
        , ( "cantidad", Encode.int cantidad )
        ]


{-| Un decodificador describe CÓMO leer un JSON, no lo lee todavía.

`Decode.map5` combina cinco decodificadores en uno: lee los cinco campos y
le pasa los cinco valores a la función.

-}
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
