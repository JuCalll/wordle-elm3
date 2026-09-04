module Vista.Estilos exposing
    ( acento
    , borde
    , colorDeEstado
    , colorTexto
    , fondo
    , superficie
    , tenue
    )

{-| Paleta y medidas del juego.

Centralizar los colores aquí evita que los valores se repartan por todas
las vistas. Si mañana quieren cambiar el tema, tocan un solo archivo.

-}

import Dominio.Evaluacion exposing (Estado(..))


fondo : String
fondo =
    "#121213"


superficie : String
superficie =
    "#1e1e20"


borde : String
borde =
    "#3a3a3c"


acento : String
acento =
    "#565758"


tenue : String
tenue =
    "#818384"


colorTexto : String
colorTexto =
    "#ffffff"


{-| El color de cada estado de casilla.

El compilador exige un color para cada variante de `Estado`. Si mañana se
agrega una cuarta, este `case` deja de compilar hasta que se decida su
color: no se puede olvidar.

-}
colorDeEstado : Estado -> String
colorDeEstado estado =
    case estado of
        Correcta ->
            "#538d4e"

        PosicionIncorrecta ->
            "#b59f3b"

        Ausente ->
            "#3a3a3c"
