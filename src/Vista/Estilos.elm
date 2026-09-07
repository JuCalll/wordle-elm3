module Vista.Estilos exposing
    ( acento
    , borde
    , colorDeEstado
    , colorTexto
    , fondo
    , superficie
    , tenue
    )

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


colorDeEstado : Estado -> String
colorDeEstado estado =
    case estado of
        Correcta ->
            "#538d4e"

        PosicionIncorrecta ->
            "#b59f3b"

        Ausente ->
            "#3a3a3c"
