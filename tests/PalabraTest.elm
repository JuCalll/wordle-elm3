-- Los módulos de prueba viven en tests/ y su nombre termina en Test.
-- Exponen una sola cosa: `suite`, que es el conjunto de pruebas.


module PalabraTest exposing (suite)

-- las aserciones: Expect.equal, Expect.greaterThan...

import Dominio.Palabra as Palabra
import Expect
import Test exposing (Test, describe, test)



-- `describe` agrupa pruebas y les pone un título.
-- `test "nombre" <| \_ -> ...` define una prueba:
--   el `<|` aplica la función de la derecha,
--   y `\_ ->` es una función que ignora su argumento.


suite : Test
suite =
    describe "Dominio.Palabra"
        [ describe "desdeTexto"
            [ test "acepta una palabra de cinco letras" <|
                \_ ->
                    Palabra.desdeTexto "gatos"
                        -- desdeTexto da un Result; `Result.map` aplica
                        -- `aTexto` solo si el resultado fue Ok.
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
                    -- Esta prueba deja escrita en el código la decisión que
                    -- tomamos sobre las tildes. No es un comentario que
                    -- alguien pueda ignorar: si se rompe, falla la suite.
                    Palabra.desdeTexto "cafés"
                        |> Expect.equal (Err (Palabra.CaracterNoValido 'é'))
            ]
        ]
