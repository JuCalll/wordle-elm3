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

{-| Acumulado histórico del jugador.

Tipo opaco, igual que `Palabra`: los contadores solo pueden avanzar a
través de `registrar`, nunca asignarse a mano. Eso hace IMPOSIBLE tener,
por ejemplo, más partidas ganadas que jugadas.

-}

import Dict exposing (Dict)


{-| Tipo opaco que envuelve un registro privado.
-}
type Estadisticas
    = Estadisticas Interno


{-| La forma interna. Al no estar en el `exposing`, nadie fuera del módulo
puede siquiera nombrar este tipo.
-}
type alias Interno =
    { jugadas : Int
    , ganadas : Int
    , rachaActual : Int
    , mejorRacha : Int
    , distribucion : Dict Int Int -- intento -> cuántas veces se ganó ahí
    }


{-| Cómo terminó una partida.

`Victoria` lleva un dato: en qué intento se ganó (1 a 6). `Derrota` no
lleva nada porque no hay más que decir.

-}
type Resultado
    = Victoria Int
    | Derrota


{-| El punto de partida: un jugador que nunca ha jugado.
-}
vacias : Estadisticas
vacias =
    Estadisticas
        { jugadas = 0
        , ganadas = 0
        , rachaActual = 0
        , mejorRacha = 0
        , distribucion = Dict.empty
        }


{-| Reconstruye unas estadísticas guardadas previamente.

Los negativos se anulan con `max 0`: un dato corrupto en el navegador
(alguien editando el localStorage a mano) no debe producir un estado
imposible.

-}
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


{-| La única forma de que los contadores avancen.

Recibe cómo terminó la partida y las estadísticas anteriores; devuelve
unas estadísticas NUEVAS. Las anteriores quedan intactas.

-}
registrar : Resultado -> Estadisticas -> Estadisticas
registrar resultado (Estadisticas interno) =
    case resultado of
        Victoria intento ->
            let
                racha =
                    interno.rachaActual + 1
            in
            Estadisticas
                -- Sintaxis de actualización de registro: "igual que
                -- `interno`, pero con estos campos cambiados".
                { interno
                    | jugadas = interno.jugadas + 1
                    , ganadas = interno.ganadas + 1
                    , rachaActual = racha

                    -- La mejor racha solo sube, nunca baja.
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

                    -- La derrota corta la racha actual, pero no la mejor.
                    , rachaActual = 0
                }



-- CONSULTAS
-- Cada una desempaqueta el tipo opaco y devuelve un campo. Son la
-- interfaz de solo lectura: desde fuera se puede mirar, no tocar.


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


{-| El porcentaje de victorias, redondeado.

Fíjate en el caso de cero partidas: sin ese `if`, sería una división por
cero. Devolvemos 0 en vez de reventar.

-}
porcentajeVictorias : Estadisticas -> Int
porcentajeVictorias (Estadisticas interno) =
    if interno.jugadas == 0 then
        0

    else
        round (100 * toFloat interno.ganadas / toFloat interno.jugadas)
