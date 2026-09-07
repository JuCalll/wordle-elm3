module Dominio.Palabra exposing
    ( Error(..)
    , Palabra
    , aLetras
    , aTexto
    , descripcionError
    , desdeTexto
    , esLetra
    , longitudRequerida
    , porDefecto
    )

import Set exposing (Set)


type Palabra
    = Palabra (List Char)


type Error
    = LongitudIncorrecta Int
    | CaracterNoValido Char


longitudRequerida : Int
longitudRequerida =
    5


desdeTexto : String -> Result Error Palabra
desdeTexto texto =
    let
        letras =
            texto
                |> String.trim
                |> String.toLower
                |> String.toList
    in
    if List.length letras /= longitudRequerida then
        Err (LongitudIncorrecta (List.length letras))

    else
        case List.filter (not << esLetra) letras of
            invalida :: _ ->
                Err (CaracterNoValido invalida)

            [] ->
                Ok (Palabra letras)


aTexto : Palabra -> String
aTexto (Palabra letras) =
    String.fromList letras


aLetras : Palabra -> List Char
aLetras (Palabra letras) =
    letras


descripcionError : Error -> String
descripcionError error =
    case error of
        LongitudIncorrecta n ->
            "La palabra debe tener "
                ++ String.fromInt longitudRequerida
                ++ " letras, y tiene "
                ++ String.fromInt n
                ++ "."

        CaracterNoValido caracter ->
            "El carácter '"
                ++ String.fromChar caracter
                ++ "' no es una letra válida."


porDefecto : Palabra
porDefecto =
    Palabra (String.toList "gatos")


esLetra : Char -> Bool
esLetra caracter =
    Set.member (Char.toLower caracter) alfabeto



-- interno


alfabeto : Set Char
alfabeto =
    "abcdefghijklmnñopqrstuvwxyz"
        |> String.toList
        |> Set.fromList
