port module Main exposing (main)

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



-- ports


port guardarEstadisticas : Encode.Value -> Cmd msg



-- modelo


type alias Model =
    { partida : Partida
    , aviso : Maybe String
    , estadisticas : Estadisticas
    }


type Msg
    = PalabraSorteada Palabra
    | LetraPresionada Char
    | BorrarPresionado
    | EnviarPresionado
    | PartidaReiniciada
    | TeclaIgnorada


init : Decode.Value -> ( Model, Cmd Msg )
init guardadas =
    ( { partida = Partida.nueva Palabra.porDefecto
      , aviso = Nothing
      , estadisticas = Almacenamiento.decodificar guardadas
      }
    , sortearPalabra
    )


sortearPalabra : Cmd Msg
sortearPalabra =
    Random.generate PalabraSorteada Diccionario.generador



-- actualización


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PalabraSorteada palabra ->
            ( { model | partida = Partida.nueva palabra, aviso = Nothing }
            , Cmd.none
            )

        LetraPresionada caracter ->
            ( { model
                | partida = Partida.escribirLetra caracter model.partida
                , aviso = Nothing
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
                    ( { model
                        | aviso = Just (Partida.descripcionRechazo rechazo)
                      }
                    , Cmd.none
                    )

        PartidaReiniciada ->
            ( model, sortearPalabra )

        TeclaIgnorada ->
            ( model, Cmd.none )


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
            ( previas, Cmd.none )

        Ganada ->
            acumular
                (Estadisticas.Victoria
                    (List.length (Partida.intentos partida))
                )

        Perdida ->
            acumular Estadisticas.Derrota



-- suscripciones


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        (Decode.map desdeTecla (Decode.field "key" Decode.string))


desdeTecla : String -> Msg
desdeTecla tecla =
    case tecla of
        "Enter" ->
            EnviarPresionado

        "Backspace" ->
            BorrarPresionado

        otra ->
            case String.uncons otra of
                Just ( caracter, "" ) ->
                    LetraPresionada caracter

                _ ->
                    TeclaIgnorada



-- vista


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



-- programa


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
