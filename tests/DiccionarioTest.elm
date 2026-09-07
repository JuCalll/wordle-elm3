module DiccionarioTest exposing (suite)

import Datos.Diccionario as Diccionario
import Dominio.Palabra as Palabra
import Expect
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Datos.Diccionario"
        [ test "ninguna palabra se pierde al validar" <|
            \_ ->
                List.length Diccionario.soluciones
                    |> Expect.equal (List.length Diccionario.crudas)
        , test "no hay palabras repetidas" <|
            \_ ->
                Set.size (Set.fromList Diccionario.crudas)
                    |> Expect.equal (List.length Diccionario.crudas)
        , test "la lista de soluciones no está vacía" <|
            \_ ->
                Expect.greaterThan 0 (List.length Diccionario.soluciones)
        , test "esSolucion reconoce una palabra de la lista" <|
            \_ ->
                Palabra.desdeTexto "gatos"
                    |> Result.map Diccionario.esSolucion
                    |> Expect.equal (Ok True)
        , test "esSolucion rechaza una palabra que no está" <|
            \_ ->
                Palabra.desdeTexto "xkqzw"
                    |> Result.map Diccionario.esSolucion
                    |> Expect.equal (Ok False)
        , test "esAceptada admite palabras fuera de la lista de soluciones" <|
            \_ ->
                Palabra.desdeTexto "perro"
                    |> Result.map Diccionario.esAceptada
                    |> Expect.equal (Ok True)
        ]
