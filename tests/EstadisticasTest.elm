module EstadisticasTest exposing (suite)

import Dict
import Dominio.Estadisticas as Estadisticas exposing (Resultado(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dominio.Estadisticas"
        [ test "arrancan en cero" <|
            \_ ->
                Estadisticas.jugadas Estadisticas.vacias
                    |> Expect.equal 0
        , test "el porcentaje sin partidas es cero, no un error" <|
            \_ ->
                Estadisticas.porcentajeVictorias Estadisticas.vacias
                    |> Expect.equal 0
        , test "una victoria suma jugada, ganada y racha" <|
            \_ ->
                let
                    e =
                        Estadisticas.registrar (Victoria 3) Estadisticas.vacias
                in
                ( Estadisticas.jugadas e
                , Estadisticas.ganadas e
                , Estadisticas.rachaActual e
                )
                    |> Expect.equal ( 1, 1, 1 )
        , test "una derrota suma jugada pero no ganada" <|
            \_ ->
                let
                    e =
                        Estadisticas.registrar Derrota Estadisticas.vacias
                in
                ( Estadisticas.jugadas e, Estadisticas.ganadas e )
                    |> Expect.equal ( 1, 0 )
        , test "la derrota corta la racha actual" <|
            \_ ->
                Estadisticas.vacias
                    |> Estadisticas.registrar (Victoria 2)
                    |> Estadisticas.registrar (Victoria 4)
                    |> Estadisticas.registrar Derrota
                    |> Estadisticas.rachaActual
                    |> Expect.equal 0
        , test "la mejor racha se conserva tras una derrota" <|
            \_ ->
                Estadisticas.vacias
                    |> Estadisticas.registrar (Victoria 2)
                    |> Estadisticas.registrar (Victoria 4)
                    |> Estadisticas.registrar Derrota
                    |> Estadisticas.mejorRacha
                    |> Expect.equal 2
        , test "la distribución cuenta el intento de cada victoria" <|
            \_ ->
                Estadisticas.vacias
                    |> Estadisticas.registrar (Victoria 3)
                    |> Estadisticas.registrar (Victoria 3)
                    |> Estadisticas.registrar (Victoria 5)
                    |> Estadisticas.distribucion
                    |> Dict.toList
                    |> Expect.equal [ ( 3, 2 ), ( 5, 1 ) ]
        , test "el porcentaje redondea correctamente" <|
            \_ ->
                Estadisticas.vacias
                    |> Estadisticas.registrar (Victoria 1)
                    |> Estadisticas.registrar Derrota
                    |> Estadisticas.registrar Derrota
                    |> Estadisticas.porcentajeVictorias
                    |> Expect.equal 33
        , test "desdePartes ignora valores negativos" <|
            \_ ->
                Estadisticas.desdePartes
                    { jugadas = -5
                    , ganadas = 2
                    , rachaActual = -1
                    , mejorRacha = 3
                    , distribucion = []
                    }
                    |> Estadisticas.jugadas
                    |> Expect.equal 0
        ]
