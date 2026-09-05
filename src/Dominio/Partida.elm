module Dominio.Partida exposing
    ( Estado(..)
    , Partida
    , Rechazo(..)
    , actual
    , borrarLetra
    , descripcionRechazo
    , enviar
    , escribirLetra
    , estado
    , intentos
    , intentosRestantes
    , maximoIntentos
    , nueva
    , objetivo
    )

{-| El estado completo de una partida y sus transiciones.

Este módulo NO conoce el diccionario ni la interfaz gráfica. Para validar
que un intento sea una palabra aceptable recibe la función que hace esa
comprobación como parámetro, de modo que la lógica del juego no depende de
dónde salen los datos.

-}

import Dominio.Evaluacion as Evaluacion exposing (LetraEvaluada)
import Dominio.Palabra as Palabra exposing (Palabra)


{-| En qué punto está la partida. Tres estados, ni uno más.
-}
type Estado
    = EnCurso
    | Ganada
    | Perdida


{-| Razones por las que un intento no se puede registrar.

Fíjate en que son datos, no mensajes de texto. El texto se genera aparte
en `descripcionRechazo`, para que la lógica no cargue con el idioma.

-}
type Rechazo
    = PalabraIncompleta
    | PalabraDesconocida
    | PartidaFinalizada


{-| Tipo opaco, el tercero del proyecto. Nadie puede fabricar una partida
en un estado inconsistente.
-}
type Partida
    = Partida Interno


type alias Interno =
    { objetivo : Palabra -- la palabra a adivinar
    , intentos : List (List LetraEvaluada) -- historial ya evaluado
    , actual : List Char -- lo que el jugador va escribiendo
    , estado : Estado
    }


maximoIntentos : Int
maximoIntentos =
    6


{-| Arranca una partida con la palabra dada.
-}
nueva : Palabra -> Partida
nueva palabraObjetivo =
    Partida
        { objetivo = palabraObjetivo
        , intentos = []
        , actual = []
        , estado = EnCurso
        }



-- TRANSICIONES
-- Cada una recibe una Partida y devuelve OTRA Partida. La anterior no se
-- modifica: sigue existiendo mientras alguien la referencie.


{-| Agrega una letra a lo que el jugador está escribiendo.

Los tres `if` son guardas: si alguna condición no se cumple, devolvemos la
partida tal cual, sin cambios. Es la forma funcional de "no hacer nada".

-}
escribirLetra : Char -> Partida -> Partida
escribirLetra caracter (Partida interno) =
    if interno.estado /= EnCurso then
        -- La partida ya terminó: se ignora la tecla.
        Partida interno

    else if List.length interno.actual >= Palabra.longitudRequerida then
        -- Ya hay cinco letras escritas: no caben más.
        Partida interno

    else if not (Palabra.esLetra caracter) then
        -- No es una letra del alfabeto (un número, un signo): se ignora.
        Partida interno

    else
        Partida
            -- `++ [ x ]` agrega al FINAL de la lista.
            { interno | actual = interno.actual ++ [ Char.toLower caracter ] }


{-| Quita la última letra escrita.
-}
borrarLetra : Partida -> Partida
borrarLetra (Partida interno) =
    if interno.estado /= EnCurso then
        Partida interno

    else
        Partida
            { interno
                | actual =
                    interno.actual
                        -- `List.take n` se queda con los n primeros.
                        -- Con la longitud menos uno, quita el último.
                        -- Si la lista está vacía, `take -1` da [] sin error.
                        |> List.take (List.length interno.actual - 1)
            }


{-| Intenta registrar la palabra que el jugador tiene escrita.

EL PARÁMETRO MÁS IMPORTANTE DEL PROYECTO es el primero:
`(Palabra -> Bool)`. Es una FUNCIÓN que decide si una palabra es aceptable.

`Partida` no importa el diccionario: lo recibe. En producción le pasaremos
`Datos.Diccionario.esAceptada`; en las pruebas, cualquier función de dos
líneas. Eso es inversión de dependencias sin necesidad de interfaces.

Devuelve `Result Rechazo Partida`: o la partida avanzó, o hay una razón
por la que no.

-}
enviar : (Palabra -> Bool) -> Partida -> Result Rechazo Partida
enviar estaEnDiccionario (Partida interno) =
    if interno.estado /= EnCurso then
        Err PartidaFinalizada

    else
        -- Reusamos la validación de `Palabra`: si lo escrito no forma una
        -- palabra bien construida, es que faltan letras.
        case Palabra.desdeTexto (String.fromList interno.actual) of
            Err _ ->
                Err PalabraIncompleta

            Ok intento ->
                if not (estaEnDiccionario intento) then
                    Err PalabraDesconocida

                else
                    Ok (registrar intento interno)



-- CONSULTAS
-- La interfaz de solo lectura hacia el exterior.


estado : Partida -> Estado
estado (Partida interno) =
    interno.estado


objetivo : Partida -> Palabra
objetivo (Partida interno) =
    interno.objetivo


intentos : Partida -> List (List LetraEvaluada)
intentos (Partida interno) =
    interno.intentos


actual : Partida -> List Char
actual (Partida interno) =
    interno.actual


intentosRestantes : Partida -> Int
intentosRestantes (Partida interno) =
    maximoIntentos - List.length interno.intentos


{-| Traduce un rechazo a un mensaje para el jugador.
-}
descripcionRechazo : Rechazo -> String
descripcionRechazo rechazo =
    case rechazo of
        PalabraIncompleta ->
            "Faltan letras."

        PalabraDesconocida ->
            "Esa palabra no está en el diccionario."

        PartidaFinalizada ->
            "La partida ya terminó."



-- INTERNO


{-| Guarda el intento evaluado y decide cómo queda la partida.
-}
registrar : Palabra -> Interno -> Partida
registrar intento interno =
    let
        -- Aquí se usa el módulo del integrante A de la ronda anterior.
        evaluacion =
            Evaluacion.evaluar
                { objetivo = interno.objetivo, intento = intento }

        historial =
            interno.intentos ++ [ evaluacion ]

        -- Comparamos los textos porque `Palabra` no se puede comparar
        -- directamente con `==` de forma fiable en todos los casos.
        acerto =
            Palabra.aTexto intento == Palabra.aTexto interno.objetivo
    in
    Partida
        { interno
            | intentos = historial
            , actual = [] -- se limpia lo escrito
            , estado = siguienteEstado acerto (List.length historial)
        }


{-| La regla de fin de partida, aislada en una función propia para que se
pueda leer de un vistazo.
-}
siguienteEstado : Bool -> Int -> Estado
siguienteEstado acerto cantidadIntentos =
    if acerto then
        Ganada

    else if cantidadIntentos >= maximoIntentos then
        Perdida

    else
        EnCurso
