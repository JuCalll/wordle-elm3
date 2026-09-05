module TecladoTest exposing (suite)

import Dict
import Dominio.Evaluacion exposing (Estado(..), LetraEvaluada)
import Dominio.Teclado as Teclado
import Expect
import Test exposing (Test, describe, test)


{-| Atajo para construir una letra evaluada sin escribir el registro
completo cada vez.
-}
letra : Char -> Estado -> LetraEvaluada
letra caracter estado =
    { letra = caracter, estado = estado }


{-| Atajo para consultar el estado de una tecla.
-}
estadoDe : Char -> List (List LetraEvaluada) -> Maybe Estado
estadoDe caracter historial =
    Teclado.estados historial
        |> Dict.get caracter


suite : Test
suite =
    describe "Dominio.Teclado"
        [ describe "disposición"
            [ test "tiene tres filas" <|
                \_ ->
                    List.length Teclado.disposicion
                        |> Expect.equal 3
            , test "incluye la ñ" <|
                \_ ->
                    List.concat Teclado.disposicion
                        |> List.member 'ñ'
                        |> Expect.equal True
            , test "tiene 27 teclas" <|
                \_ ->
                    List.concat Teclado.disposicion
                        |> List.length
                        |> Expect.equal 27
            ]
        , describe "estados"
            [ test "sin intentos, ninguna tecla tiene estado" <|
                \_ ->
                    Teclado.estados []
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "registra el estado de una letra usada" <|
                \_ ->
                    estadoDe 'g' [ [ letra 'g' Correcta ] ]
                        |> Expect.equal (Just Correcta)
            , test "una letra no usada no aparece" <|
                \_ ->
                    estadoDe 'z' [ [ letra 'g' Correcta ] ]
                        |> Expect.equal Nothing

            -- Los cuatro casos que siguen cubren todas las combinaciones
            -- que importan, incluyendo el mismo par en orden inverso: así
            -- comprobamos que la regla depende de la prioridad y no del
            -- orden de llegada.
            , test "el verde reemplaza al gris de un intento anterior" <|
                \_ ->
                    estadoDe 'a'
                        [ [ letra 'a' Ausente ]
                        , [ letra 'a' Correcta ]
                        ]
                        |> Expect.equal (Just Correcta)
            , test "el gris no degrada un verde ya obtenido" <|
                \_ ->
                    estadoDe 'a'
                        [ [ letra 'a' Correcta ]
                        , [ letra 'a' Ausente ]
                        ]
                        |> Expect.equal (Just Correcta)
            , test "el amarillo gana al gris" <|
                \_ ->
                    estadoDe 'a'
                        [ [ letra 'a' Ausente ]
                        , [ letra 'a' PosicionIncorrecta ]
                        ]
                        |> Expect.equal (Just PosicionIncorrecta)
            , test "el verde gana al amarillo" <|
                \_ ->
                    estadoDe 'a'
                        [ [ letra 'a' PosicionIncorrecta ]
                        , [ letra 'a' Correcta ]
                        ]
                        |> Expect.equal (Just Correcta)
            ]
        ]
