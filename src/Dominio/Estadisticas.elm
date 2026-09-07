module Dominio.Estadisticas exposing
    ( Estadisticas
    , Resultado(..)
    , desdePartes
    , distribucion
    , ganadas
    , jugadas
    , mejorRacha
    , porcentajeVictorias
    , rachaActual
    , registrar
    , vacias
    )

import Dict exposing (Dict)


type Estadisticas
    = Estadisticas Interno


type alias Interno =
    { jugadas : Int
    , ganadas : Int
    , rachaActual : Int
    , mejorRacha : Int
    , distribucion : Dict Int Int
    }


type Resultado
    = Victoria Int
    | Derrota


vacias : Estadisticas
vacias =
    Estadisticas
        { jugadas = 0
        , ganadas = 0
        , rachaActual = 0
        , mejorRacha = 0
        , distribucion = Dict.empty
        }


desdePartes :
    { jugadas : Int
    , ganadas : Int
    , rachaActual : Int
    , mejorRacha : Int
    , distribucion : List ( Int, Int )
    }
    -> Estadisticas
desdePartes partes =
    Estadisticas
        { jugadas = max 0 partes.jugadas
        , ganadas = max 0 partes.ganadas
        , rachaActual = max 0 partes.rachaActual
        , mejorRacha = max 0 partes.mejorRacha
        , distribucion = Dict.fromList partes.distribucion
        }


registrar : Resultado -> Estadisticas -> Estadisticas
registrar resultado (Estadisticas interno) =
    case resultado of
        Victoria intento ->
            let
                racha =
                    interno.rachaActual + 1
            in
            Estadisticas
                { interno
                    | jugadas = interno.jugadas + 1
                    , ganadas = interno.ganadas + 1
                    , rachaActual = racha
                    , mejorRacha = max racha interno.mejorRacha
                    , distribucion =
                        Dict.update intento
                            (Maybe.withDefault 0 >> (+) 1 >> Just)
                            interno.distribucion
                }

        Derrota ->
            Estadisticas
                { interno
                    | jugadas = interno.jugadas + 1
                    , rachaActual = 0
                }



-- consultas


jugadas : Estadisticas -> Int
jugadas (Estadisticas interno) =
    interno.jugadas


ganadas : Estadisticas -> Int
ganadas (Estadisticas interno) =
    interno.ganadas


rachaActual : Estadisticas -> Int
rachaActual (Estadisticas interno) =
    interno.rachaActual


mejorRacha : Estadisticas -> Int
mejorRacha (Estadisticas interno) =
    interno.mejorRacha


distribucion : Estadisticas -> Dict Int Int
distribucion (Estadisticas interno) =
    interno.distribucion


porcentajeVictorias : Estadisticas -> Int
porcentajeVictorias (Estadisticas interno) =
    if interno.jugadas == 0 then
        0

    else
        round (100 * toFloat interno.ganadas / toFloat interno.jugadas)
