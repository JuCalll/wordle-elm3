module Datos.Diccionario exposing
    ( crudas
    , soluciones
    , esSolucion
    , esAceptada
    , generador
    )

{-| La lista de palabras del juego.

Las palabras se guardan como texto plano en `crudas` y se validan al
convertirlas en `soluciones`. La prueba `DiccionarioTest` garantiza que
ninguna entrada se pierda en esa conversión: si alguien agrega una palabra
con tilde, la prueba falla y avisa.

-}

import Dominio.Palabra as Palabra exposing (Palabra)
import Random
import Set


{-| Texto plano, en minúsculas, sin tildes. La ñ sí está permitida.

La lista se escribe con la coma AL PRINCIPIO de cada línea. Es el estilo
estándar de Elm y tiene una ventaja práctica: agregar o quitar una línea
nunca deja una coma huérfana.
-}
crudas : List String
crudas =
    [ "abeja", "abril", "acero", "aguja", "aldea", "altar", "amigo"
    , "ancho", "andar", "arena", "arroz", "avion", "ayuda", "barco"
    , "barro", "bello", "bicho", "blusa", "bolsa", "borde", "botas"
    , "bravo", "breve", "brisa", "broma", "bruja", "bueno", "bulto"
    , "burla", "cabra", "cacao", "cajas", "calor", "campo", "canal"
    , "canto", "capaz", "carne", "carro", "casas", "casco", "cebra"
    , "celda", "cerca", "cerdo", "cesta", "chico", "choza", "cielo"
    , "cifra", "cinco", "circo", "cisne", "claro", "clase", "clave"
    , "clima", "cobre", "colas", "color", "comer", "coral", "corte"
    , "costa", "crear", "crema", "cruce", "crudo", "cruel", "culpa"
    , "curso", "curva", "damas", "danza", "dardo", "datos", "dedos"
    , "delta", "denso", "digno", "dique", "disco", "doble", "dogma"
    , "dolor", "donde", "dosis", "drama", "duelo", "dueño", "dulce"
    , "duque", "echar", "ejote", "enano", "enero", "enojo", "entre"
    , "error", "etapa", "extra", "falda", "falso", "fango", "farol"
    , "fatal", "fauna", "favor", "fecha", "feliz", "fibra", "ficha"
    , "fiera", "filas", "final", "finca", "firma", "flaco", "flete"
    , "flojo", "flora", "fluir", "fobia", "folio", "fondo", "forma"
    , "frase", "freno", "fresa", "fruta", "fuego", "fugaz", "fumar"
    , "funda", "furia", "fusil", "galgo", "ganar", "ganso", "garra"
    , "gasto", "gatos", "gemas", "genio", "gente", "gesto", "girar"
    , "globo", "golfo", "golpe", "gorra", "grado", "grano", "grasa"
    , "grave", "grifo", "grito", "grupo", "guapo", "guiar", "gusto"
    , "haber", "habla", "hacer", "hacha", "hasta", "hebra", "hecho"
    , "helar", "herir", "hielo", "hijos", "hilos", "himno", "hogar"
    , "hojas", "honda", "honor", "horas", "horno", "hotel", "hueco"
    , "hueso", "huevo", "huida", "humor", "hurto", "igual", "impar"
    , "indio", "islas", "istmo", "jaula", "jefes", "joven", "joyas"
    , "juego", "jugar", "junio", "junta", "junto", "jurar", "justo"
    , "labio", "labor", "lanza", "largo", "latir", "lavar", "leche"
    , "legal", "lejos", "lento", "letra", "leyes", "libra", "libre"
    , "libro", "licor", "ligar", "lirio", "lista", "litro", "llama"
    , "llano", "llave", "llena", "lobos", "local", "logro", "lucha"
    , "lucir", "luego", "lugar", "lunes", "macho", "madre", "magia"
    , "malla", "mango", "manos", "manta", "marca", "marco", "marea"
    , "marzo", "mayor", "mecha", "medio", "mejor", "menor", "menos"
    , "mente", "mesas", "metal", "meter", "metro", "miedo", "miles"
    , "milla", "minas", "mirar", "mismo", "mitad", "mixto", "modas"
    , "modos", "mojar", "molde", "moler", "monte", "moral", "morir"
    , "motor", "mover", "mucho", "mudar", "muela", "mujer", "multa"
    , "mundo", "museo", "musgo", "nabos", "nacer", "nadar", "nadie"
    , "naipe", "nariz", "natal", "naval", "negar", "negro", "nevar"
    , "nicho", "nidos", "nieve", "niños", "nivel", "noble", "noche"
    , "norte", "notas", "novia", "nubes", "nudos", "nuevo", "nunca"
    , "obeso", "obras", "ocaso", "oeste", "oigan", "ojear", "oliva"
    , "ollas", "ondas", "opaco", "opera", "orden", "oreja", "otoño"
    , "pacto", "padre", "pagar", "pagos", "palma", "palos", "panal"
    , "panes", "papel", "parar", "pared", "parte", "pasar", "pasos"
    , "pasta", "pasto", "patio", "patos", "pausa", "pecar", "pecho"
    , "pedal", "pedir", "pegar", "peine", "pelar", "pelos", "penal"
    , "perro", "pesar", "pesca", "pesos", "pieza", "pilar", "pinos"
    , "pinza", "pisar", "pisos", "pista", "plato", "playa", "plaza"
    , "pleno", "plomo", "pluma", "pobre", "poder", "poema", "polen"
    , "polvo", "pollo", "poner", "posar", "potro", "pozos", "prado"
    , "presa", "primo", "prisa", "pulga", "pulpo", "pulso", "punta"
    , "punto", "puros", "queda", "quema", "queso", "quien", "quiso"
    , "quita", "rabia", "radio", "ramas", "rampa", "ranas", "rango"
    , "rapaz", "rasgo", "ratas", "rayas", "rayos", "recto", "redes"
    , "regar", "regla", "reina", "reino", "rejas", "reloj", "remar"
    , "remos", "renta", "resta", "retar", "rezar", "ricos", "riego"
    , "rifle", "rigor", "rimar", "riñas", "risas", "ritmo", "rival"
    , "rizos", "robar", "roble", "robos", "rocas", "rojos", "rollo"
    , "ronda", "ropas", "rosal", "rosas", "rubia", "rubor", "rueda"
    , "rugir", "ruido", "rumbo", "rural", "rutas", "sabor", "sacar"
    , "sacos", "sagaz", "salas", "salir", "salsa", "salto", "salud"
    , "salvo", "sanar", "santo", "sapos", "sauce", "secar", "secos"
    , "sedas", "segar", "selva", "sello", "senda", "senos", "señal"
    , "serie", "sesos", "setas", "sexto", "sidra", "siglo", "signo"
    , "silla", "simio", "sitio", "sobre", "socio", "solar", "soles"
    , "sonar", "sopas", "soplo", "sorbo", "sordo", "subir", "sucio"
    , "sudor", "suelo", "sueño", "sumar", "surco", "sutil", "tabla"
    , "tacos", "talco", "talla", "tallo", "tapas", "tapiz", "tarde"
    , "tarea", "tarro", "tazas", "techo", "tejas", "tejer", "telas"
    , "temer", "temor", "tenaz", "tener", "tenis", "tenso", "terco"
    , "tesis", "texto", "tibio", "tigre", "tinta", "tinto", "tirar"
    , "tocar", "todos", "tomar", "tonos", "tonto", "topos", "torre"
    , "torso", "torta", "tosco", "total", "traer", "trago", "traje"
    , "trama", "trapo", "trazo", "trece", "treta", "tribu", "trigo"
    , "tripa", "trono", "tropa", "trozo", "tubos", "tumba", "turba"
    , "turno", "untar", "urbes", "usual", "vacas", "valle", "valor"
    , "vapor", "vasos", "vejez", "velas", "velos", "vello", "venas"
    , "venir", "verbo", "verde", "verja", "verso", "vetar", "viaje"
    , "vibra", "vicio", "vidas", "video", "viejo", "vigor", "villa"
    , "vinos", "virar", "virus", "visor", "vista", "vital", "vivir"
    , "vivos", "volar", "votar", "votos", "vuelo", "yates", "yegua"
    , "yerba", "yerno", "yesos", "yogur", "yunta", "zafra", "zanja"
    , "zarpa", "zonas", "zorro", "zumba", "zurdo"
    ]


{-| Solo las entradas que pasaron la validación.

`List.filterMap` hace dos cosas a la vez: transforma cada elemento y
descarta los que dan `Nothing`.
`Palabra.desdeTexto >> Result.toMaybe` es la composición de dos funciones:
primero valida (dando Result), luego convierte ese Result en Maybe.
-}
soluciones : List Palabra
soluciones =
    List.filterMap (Palabra.desdeTexto >> Result.toMaybe) crudas


{-| ¿Puede esta palabra salir sorteada?
-}
esSolucion : Palabra -> Bool
esSolucion palabra =
    Set.member (Palabra.aTexto palabra) conjunto


{-| ¿Se acepta como intento del jugador?

Hoy aceptamos cualquier palabra bien formada: cinco letras del alfabeto
español sin tildes. Es deliberadamente permisivo, porque exigir la lista
corta haría el juego injugable.

Cuando exista una lista amplia de palabras válidas, se cambia SOLO esta
función. Ni `Partida` ni `Main` se enteran.
-}
esAceptada : Palabra -> Bool
esAceptada _ =
    True


{-| Cómo elegir una palabra al azar.

ATENCIÓN: esto NO elige nada. Es un VALOR que describe un sorteo, como una
receta describe un plato sin cocinarlo. Quien lo ejecuta es el runtime de
Elm, cuando `Main` se lo entrega envuelto en un `Cmd`.

`Random.uniform` recibe un elemento Y una lista, no una lista sola. ¿Por
qué? Porque sortear entre cero opciones no tiene respuesta posible: el tipo
no te deja ni plantear la pregunta.
-}
generador : Random.Generator Palabra
generador =
    case soluciones of
        primera :: resto ->
            Random.uniform primera resto

        [] ->
            Random.constant Palabra.porDefecto



-- INTERNO


{-| Las palabras como conjunto, para que `esSolucion` responda rápido.
-}
conjunto : Set.Set String
conjunto =
    Set.fromList crudas