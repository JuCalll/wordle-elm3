module PalabraTest exposing (suite)

import Dominio.Palabra as Palabra
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dominio.Palabra"
        [ describe "desdeTexto"
            [ test "acepta una palabra de cinco letras" <|
                \_ ->
                    Palabra.desdeTexto "gatos"
                        |> Result.map Palabra.aTexto
                        |> Expect.equal (Ok "gatos")
            , test "normaliza mayúsculas y espacios" <|
                \_ ->
                    Palabra.desdeTexto "  GATOS "
                        |> Result.map Palabra.aTexto
                        |> Expect.equal (Ok "gatos")
            , test "acepta la ñ" <|
                \_ ->
                    Palabra.desdeTexto "caños"
                        |> Result.map Palabra.aTexto
                        |> Expect.equal (Ok "caños")
            , test "rechaza palabras de longitud distinta" <|
                \_ ->
                    Palabra.desdeTexto "sol"
                        |> Expect.equal (Err (Palabra.LongitudIncorrecta 3))
            , test "rechaza tildes" <|
                \_ ->
                    Palabra.desdeTexto "cafés"
                        |> Expect.equal (Err (Palabra.CaracterNoValido 'é'))
            ]
        ]
