module Library where
import PdePreludat

data Jugador = UnJugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Number,
    precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
    velocidad :: Number,
    precision :: Number,
    altura :: Number,
    superaObstaculo :: Bool
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
    | f a > f b = a
    | otherwise = b


data Palo = Putter | Madera | Hierro Number
    deriving (Show, Eq, Ord)

golpe :: Jugador -> Palo -> Tiro
golpe jugador Putter = UnTiro{
    velocidad = 10,
    precision = precisionJugador $ habilidad jugador,
    altura = 0,
    superaObstaculo = False
    }
golpe jugador Madera = UnTiro{
    velocidad = 100,
    precision = div (precisionJugador $ habilidad jugador) 2,
    altura = 5,
    superaObstaculo = False
}
golpe jugador (Hierro nro) = UnTiro{
    velocidad = (* nro) . fuerzaJugador $ habilidad jugador,
    precision = div (precisionJugador $ habilidad jugador) nro,
    altura = max 0 $ (-) nro 3,
    superaObstaculo = False
}

palos :: [Palo]
palos = [Putter, Madera, Hierro 1, Hierro 2, Hierro 3, Hierro 4, Hierro 5, Hierro 6, Hierro 7, Hierro 8, Hierro 9, Hierro 10]

data Obstaculo = TunelConRampita | Laguna Number | Hoyo
    deriving (Show, Eq, Ord)

detenerTiro :: Tiro -> Tiro
detenerTiro tiro = tiro{velocidad=0,precision=0,altura=0}

superarObstaculo :: Tiro -> Tiro
superarObstaculo tiro = tiro{superaObstaculo=True}

enfrentarObstaculo :: Tiro -> Obstaculo -> Tiro
enfrentarObstaculo tiro TunelConRampita
    |   ( < precision tiro ) 90 = superarObstaculo $ tiro{
            velocidad = (*3) . velocidad $ tiro,
            precision = 100,
            altura = 0}
    |   otherwise = detenerTiro tiro
enfrentarObstaculo tiro (Laguna largo)
    |   (&& ( < velocidad tiro ) 80) $ elem (altura tiro) [1..5] = superarObstaculo $ tiro{
            altura = div (altura tiro) largo
            }
    |   otherwise = detenerTiro tiro
enfrentarObstaculo tiro Hoyo
    |   (&& ( < precision tiro ) 95) $ elem (velocidad tiro) [5..20] = (superarObstaculo . detenerTiro) tiro
    |   otherwise = detenerTiro tiro

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculo . flip enfrentarObstaculo obstaculo . golpe jugador) palos

superarObstaculosConsecutivos :: Tiro -> [Obstaculo] -> [Obstaculo]
superarObstaculosConsecutivos tiro [] = []
superarObstaculosConsecutivos tiro (obstaculo:demasObstaculos)
    |   superaObstaculo $ enfrentarObstaculo tiro obstaculo = obstaculo : superarObstaculosConsecutivos (enfrentarObstaculo tiro obstaculo) demasObstaculos
    |   otherwise = []

superarObstaculosConsecutivos' :: Tiro -> [Obstaculo] -> [Obstaculo]
superarObstaculosConsecutivos' tiro = takeWhile (superaObstaculo . enfrentarObstaculo tiro)

paloMasUtil :: Jugador -> [Obstaculo] -> [Palo] -> [Palo]
paloMasUtil _ [] _ = []
paloMasUtil jugador obstaculos (palo:demasPalos)
    | (== length obstaculos) . length $ superarObstaculosConsecutivos' (golpe jugador palo) obstaculos = [palo]
    | otherwise = paloMasUtil jugador obstaculos demasPalos

-- pierdeApuesta :: [(Jugador, Puntos)] -> [String]
-- pierdeApuesta jugadores 