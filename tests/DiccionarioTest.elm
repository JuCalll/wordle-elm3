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
                -- Esta prueba protege los DATOS, no el código. Si alguien
                -- agrega una palabra con tilde o de seis letras, la lista
                -- validada queda más corta que la cruda y la prueba avisa.
                List.length Diccionario.soluciones
                    |> Expect.equal (List.length Diccionario.crudas)
        , test "no hay palabras repetidas" <|
            \_ ->
                -- Un Set descarta duplicados. Si el tamaño del conjunto es
                -- menor que el de la lista, había repetidas.
                Set.size (Set.fromList Diccionario.crudas)
                    |> Expect.equal (List.length Diccionario.crudas)
        , test "la lista de soluciones no está vacía" <|
            \_ ->
                -- `Expect.greaterThan` da mensajes de error mucho más claros
                -- que comparar booleanos con `Expect.equal`.
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
