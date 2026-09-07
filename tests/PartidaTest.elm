module PartidaTest exposing (suite)

import Dominio.Palabra as Palabra exposing (Palabra)
import Dominio.Partida as Partida exposing (Estado(..), Partida, Rechazo(..))
import Expect
import Test exposing (Test, describe, test)


objetivo : Palabra
objetivo =
    Palabra.desdeTexto "gatos"
        |> Result.withDefault Palabra.porDefecto


todoVale : Palabra -> Bool
todoVale _ =
    True


nadaVale : Palabra -> Bool
nadaVale _ =
    False


escribir : String -> Partida -> Partida
escribir texto partida =
    String.foldl Partida.escribirLetra partida texto


jugar : String -> Partida -> Partida
jugar texto partida =
    escribir texto partida
        |> Partida.enviar todoVale
        |> Result.withDefault partida


suite : Test
suite =
    describe "Dominio.Partida"
        [ describe "estado inicial"
            [ test "arranca en curso, sin intentos" <|
                \_ ->
                    Partida.nueva objetivo
                        |> Partida.estado
                        |> Expect.equal EnCurso
            , test "arranca con todos los intentos disponibles" <|
                \_ ->
                    Partida.nueva objetivo
                        |> Partida.intentosRestantes
                        |> Expect.equal Partida.maximoIntentos
            ]
        , describe "escritura"
            [ test "acumula las letras escritas" <|
                \_ ->
                    Partida.nueva objetivo
                        |> escribir "sol"
                        |> Partida.actual
                        |> Expect.equal [ 's', 'o', 'l' ]
            , test "no permite pasar del límite de letras" <|
                \_ ->
                    Partida.nueva objetivo
                        |> escribir "abcdefgh"
                        |> Partida.actual
                        |> List.length
                        |> Expect.equal Palabra.longitudRequerida
            , test "ignora caracteres que no son letras" <|
                \_ ->
                    Partida.nueva objetivo
                        |> escribir "a1b!c"
                        |> Partida.actual
                        |> Expect.equal [ 'a', 'b', 'c' ]
            , test "borrar quita la última letra" <|
                \_ ->
                    Partida.nueva objetivo
                        |> escribir "sol"
                        |> Partida.borrarLetra
                        |> Partida.actual
                        |> Expect.equal [ 's', 'o' ]
            , test "borrar con el buffer vacío no rompe nada" <|
                \_ ->
                    Partida.nueva objetivo
                        |> Partida.borrarLetra
                        |> Partida.actual
                        |> Expect.equal []
            ]
        , describe "envío"
            [ test "rechaza una palabra incompleta" <|
                \_ ->
                    Partida.nueva objetivo
                        |> escribir "sol"
                        |> Partida.enviar todoVale
                        |> Expect.equal (Err PalabraIncompleta)
            , test "rechaza una palabra fuera del diccionario" <|
                \_ ->
                    Partida.nueva objetivo
                        |> escribir "perro"
                        |> Partida.enviar nadaVale
                        |> Expect.equal (Err PalabraDesconocida)
            , test "un envío válido registra el intento" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "perro"
                        |> Partida.intentos
                        |> List.length
                        |> Expect.equal 1
            , test "un envío válido limpia el buffer" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "perro"
                        |> Partida.actual
                        |> Expect.equal []
            ]
        , describe "fin de la partida"
            [ test "acertar la palabra gana" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "gatos"
                        |> Partida.estado
                        |> Expect.equal Ganada
            , test "agotar los intentos pierde" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "perro"
                        |> jugar "casas"
                        |> jugar "libro"
                        |> jugar "mesas"
                        |> jugar "nubes"
                        |> jugar "pluma"
                        |> Partida.estado
                        |> Expect.equal Perdida
            , test "ganar antes del último intento no pierde" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "perro"
                        |> jugar "gatos"
                        |> Partida.estado
                        |> Expect.equal Ganada
            , test "no se puede enviar en una partida terminada" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "gatos"
                        |> escribir "perro"
                        |> Partida.enviar todoVale
                        |> Expect.equal (Err PartidaFinalizada)
            , test "no se puede escribir en una partida terminada" <|
                \_ ->
                    Partida.nueva objetivo
                        |> jugar "gatos"
                        |> escribir "perro"
                        |> Partida.actual
                        |> Expect.equal []
            ]
        ]
