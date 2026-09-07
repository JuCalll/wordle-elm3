module EvaluacionTest exposing (suite)

import Dominio.Evaluacion as Evaluacion exposing (Estado(..))
import Dominio.Palabra as Palabra
import Expect
import Test exposing (Test, describe, test)


estados : String -> String -> List Estado
estados objetivo intento =
    case ( Palabra.desdeTexto objetivo, Palabra.desdeTexto intento ) of
        ( Ok o, Ok i ) ->
            Evaluacion.evaluar { objetivo = o, intento = i }
                |> List.map .estado

        _ ->
            []


suite : Test
suite =
    describe "Dominio.Evaluacion"
        [ test "acierto total: todas correctas" <|
            \_ ->
                estados "gatos" "gatos"
                    |> Expect.equal
                        [ Correcta, Correcta, Correcta, Correcta, Correcta ]
        , test "sin coincidencias: todas ausentes" <|
            \_ ->
                estados "pluma" "corte"
                    |> Expect.equal
                        [ Ausente, Ausente, Ausente, Ausente, Ausente ]
        , test "letra presente en otra posición" <|
            \_ ->
                estados "gatos" "tigre"
                    |> List.take 1
                    |> Expect.equal [ PosicionIncorrecta ]
        , test "no marca de más una letra repetida en el intento" <|
            \_ ->
                estados "campo" "araña"
                    |> Expect.equal
                        [ PosicionIncorrecta
                        , Ausente
                        , Ausente
                        , Ausente
                        , Ausente
                        ]
        , test "el verde consume la letra y el amarillo se queda sin ella" <|
            \_ ->
                estados "canto" "banal"
                    |> Expect.equal
                        [ Ausente
                        , Correcta
                        , Correcta
                        , Ausente
                        , Ausente
                        ]
        ]
