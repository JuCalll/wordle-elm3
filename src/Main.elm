-- `port module` en vez de `module`: este archivo declara canales hacia
-- JavaScript. Es el ÚNICO que puede hacerlo.


port module Main exposing (main)

{-| Punto de entrada de la aplicación.

Este módulo conecta el dominio con la interfaz. No contiene lógica de
juego: toda decisión sobre qué es válido o quién ganó vive en `Dominio`.
Aquí solo se traducen eventos del usuario en mensajes, y mensajes en
nuevos estados.

-}

import Browser
import Browser.Events
import Datos.Almacenamiento as Almacenamiento
import Datos.Diccionario as Diccionario
import Dominio.Estadisticas as Estadisticas exposing (Estadisticas)
import Dominio.Palabra as Palabra exposing (Palabra)
import Dominio.Partida as Partida exposing (Estado(..), Partida)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Vista.Tablero



-- PORTS


{-| Canal de salida hacia el navegador.

Un `port` es la única forma que tiene Elm de hablar con JavaScript. No es
una llamada a función: es un canal por el que se envían datos. Lo que
ocurra del otro lado, Elm no lo sabe ni lo controla.

Fíjate en el tipo de retorno: `Cmd msg`. Este port NO guarda nada. Produce
una DESCRIPCIÓN de "hay que guardar esto", que alguien más ejecutará.

-}
port guardarEstadisticas : Encode.Value -> Cmd msg



-- MODELO


{-| Todo el estado de la aplicación, en un solo valor.

Si quieres saber qué está pasando en el juego en cualquier momento, miras
esto y ya. No hay estado escondido en ningún otro sitio.

-}
type alias Model =
    { partida : Partida
    , aviso : Maybe String -- Nothing = no hay nada que avisar
    , estadisticas : Estadisticas
    }


{-| TODO lo que puede pasar en la aplicación, enumerado.

Esta lista es cerrada: no puede ocurrir nada que no esté aquí. Y el
compilador va a exigir que `update` maneje las seis variantes.

-}
type Msg
    = PalabraSorteada Palabra -- el runtime nos devuelve la palabra del día
    | LetraPresionada Char
    | BorrarPresionado
    | EnviarPresionado
    | PartidaReiniciada
    | TeclaIgnorada -- una tecla que no nos interesa


{-| El arranque.

Recibe los datos guardados del navegador (los "flags") y devuelve DOS
cosas: el modelo inicial y un comando.

Fíjate en que la partida arranca con `Palabra.porDefecto`. Todavía no
sabemos cuál es la palabra del día: eso llega después, cuando el runtime
responda al sorteo.

-}
init : Decode.Value -> ( Model, Cmd Msg )
init guardadas =
    ( { partida = Partida.nueva Palabra.porDefecto
      , aviso = Nothing
      , estadisticas = Almacenamiento.decodificar guardadas
      }
    , sortearPalabra
    )


{-| LA LÍNEA MÁS IMPORTANTE DEL PROYECTO PARA LA EXPOSICIÓN.

`Random.generate` no sortea nada. Construye un `Cmd Msg`: un valor que
describe "hay que hacer este sorteo y avisarme con este mensaje".

La cadena completa es:

1.  `init` devuelve el Cmd. -> nada se ha sorteado todavía
2.  El runtime de Elm recibe el Cmd
    y hace el sorteo. -> AQUÍ está el no determinismo
3.  El runtime nos llama de vuelta
    con `PalabraSorteada "cielo"`. -> ya es un dato normal
4.  Nuestro `update` construye un
    modelo nuevo. -> función pura, sin sorpresas

-}
sortearPalabra : Cmd Msg
sortearPalabra =
    Random.generate PalabraSorteada Diccionario.generador



-- ACTUALIZACIÓN


{-| El corazón del ciclo.

Recibe qué pasó y cómo estaban las cosas; devuelve cómo quedan y qué
efectos hay que pedirle al runtime.

Fíjate en que SIEMPRE devuelve una tupla `( Model, Cmd Msg )`. Cuando no
hay ningún efecto que pedir, se devuelve `Cmd.none`.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PalabraSorteada palabra ->
            -- Llegó la palabra del día: empieza la partida de verdad.
            ( { model | partida = Partida.nueva palabra, aviso = Nothing }
            , Cmd.none
            )

        LetraPresionada caracter ->
            -- Toda la decisión de si la letra cabe, si es válida o si la
            -- partida terminó está en `Dominio.Partida`. Aquí solo se
            -- delega.
            ( { model
                | partida = Partida.escribirLetra caracter model.partida
                , aviso = Nothing -- al escribir, se limpia el aviso
              }
            , Cmd.none
            )

        BorrarPresionado ->
            ( { model
                | partida = Partida.borrarLetra model.partida
                , aviso = Nothing
              }
            , Cmd.none
            )

        EnviarPresionado ->
            -- Aquí, y SOLO aquí, se conectan `Partida` y `Diccionario`.
            -- `Partida` nunca supo que el diccionario existe.
            case Partida.enviar Diccionario.esAceptada model.partida of
                Ok siguiente ->
                    let
                        ( estadisticas, efecto ) =
                            cerrarPartida siguiente model.estadisticas
                    in
                    ( { model
                        | partida = siguiente
                        , aviso = Nothing
                        , estadisticas = estadisticas
                      }
                    , efecto
                    )

                Err rechazo ->
                    -- El intento no se registra: solo mostramos el motivo.
                    ( { model
                        | aviso = Just (Partida.descripcionRechazo rechazo)
                      }
                    , Cmd.none
                    )

        PartidaReiniciada ->
            -- No creamos la partida nueva aquí: devolvemos el modelo TAL
            -- CUAL y pedimos otro sorteo. La partida llegará después, con
            -- `PalabraSorteada`.
            -- Es imposible reiniciar sin sortear: no hay forma de construir
            -- el estado nuevo sin pasar por el runtime.
            ( model, sortearPalabra )

        TeclaIgnorada ->
            ( model, Cmd.none )


{-| Si la partida acaba de terminar, acumula el resultado y pide guardarlo.

Devuelve una tupla igual que `update`: las estadísticas nuevas y el efecto
que hay que ejecutar.

-}
cerrarPartida : Partida -> Estadisticas -> ( Estadisticas, Cmd Msg )
cerrarPartida partida previas =
    let
        acumular resultado =
            let
                nuevas =
                    Estadisticas.registrar resultado previas
            in
            ( nuevas, guardarEstadisticas (Almacenamiento.codificar nuevas) )
    in
    case Partida.estado partida of
        EnCurso ->
            -- La partida sigue: no hay nada que acumular ni que guardar.
            ( previas, Cmd.none )

        Ganada ->
            acumular
                (Estadisticas.Victoria
                    (List.length (Partida.intentos partida))
                )

        Perdida ->
            acumular Estadisticas.Derrota



-- SUSCRIPCIONES


{-| Qué eventos del exterior nos interesan.

Aquí pedimos que nos avisen de cada tecla presionada. Es lo que hace que
el teclado físico funcione además del de pantalla.

-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        -- Un decodificador: del evento JSON que manda el navegador,
        -- sacamos el campo "key" como texto, y lo convertimos en un Msg.
        (Decode.map desdeTecla (Decode.field "key" Decode.string))


{-| Traduce el nombre de una tecla en un mensaje del juego.
-}
desdeTecla : String -> Msg
desdeTecla tecla =
    case tecla of
        "Enter" ->
            EnviarPresionado

        "Backspace" ->
            BorrarPresionado

        otra ->
            -- `String.uncons` parte un texto en (primer carácter, resto).
            -- Si el resto es "", la tecla era un solo carácter: una letra.
            -- Si no (F1, Shift, ArrowUp...), la ignoramos.
            case String.uncons otra of
                Just ( caracter, "" ) ->
                    LetraPresionada caracter

                _ ->
                    TeclaIgnorada



-- VISTA


{-| Le entrega al tablero todo lo que necesita, incluidos los mensajes que
debe producir cuando el usuario interactúe.
-}
view : Model -> Html Msg
view model =
    Vista.Tablero.ver
        { partida = model.partida
        , aviso = model.aviso
        , alPresionarLetra = LetraPresionada
        , alBorrar = BorrarPresionado
        , alEnviar = EnviarPresionado
        , alReiniciar = PartidaReiniciada
        , estadisticas = model.estadisticas
        }



-- PROGRAMA


{-| El punto de entrada.

`Browser.element` monta la aplicación dentro de un nodo del HTML.
`Program Decode.Value Model Msg` significa: recibe flags de tipo
Decode.Value, su estado es Model y sus mensajes son Msg.

-}
main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
