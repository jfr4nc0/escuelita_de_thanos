{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat

data Material = Hierro | Uru | Goma
    deriving (Show, Eq, Ord)

data Gema = Mente Number | Alma Habilidad | Espacio String | Poder | Tiempo | Loca Gema
    deriving (Show, Eq, Ord)

-- type Gemas = Number
type Habilidad = String

data Guantele = Guantele {
    material :: Material,
    gemas :: [Gema]
} deriving (Show, Eq, Ord)

data Personaje = Personaje {
    edad :: Number,
    energia :: Number,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: String
} deriving (Show, Eq, Ord)

data Universo = Universo {
    habitantes :: [Personaje]
} deriving (Show, Eq, Ord)

-- Elimina a la mitad de un grupo de personajes teniendo en cuenta la division entera,
-- si se tiene 6 gemas en el guantalete y el material es Uru
extermiverso :: Guantele -> Universo -> Universo
extermiverso guantalete universo
    | esGuanteCompleto guantalete = universo {habitantes = take (calcularMitad universo) (habitantes universo)}
    | otherwise = universo

esGuanteCompleto :: Guantele -> Bool
esGuanteCompleto guante = (&&) ((== 6) . length . gemas $ guante) ((== Uru) . material $ guante)

calcularMitad :: Universo -> Number
calcularMitad universo = div (length (habitantes universo)) 2

aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any ((<45).edad) (habitantes universo)

energiaTotal :: Universo -> Number
energiaTotal universo = sumOf energia (filter ((>1) . length . habilidades) (habitantes universo))

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema (Mente val) personaje = personaje{
    energia = energia personaje - val
}
aplicarGema (Alma habilidad) personaje = personaje{
    habilidades = filter (/=habilidad) (habilidades personaje),
    energia = energia personaje - 10
}
aplicarGema (Espacio planeta) personaje = personaje{
    planeta = planeta,
    energia = energia personaje - 20
}
aplicarGema Poder personaje = personaje{
    habilidades = if (<=2) . length . habilidades $ personaje
        then drop 2 (habilidades personaje)
        else habilidades personaje,
    energia = 0
}
aplicarGema Tiempo personaje = personaje{
    edad = max (div (edad personaje) 2) 18,
    energia = energia personaje - 20
}
aplicarGema (Loca gema) personaje = (aplicarGema gema . aplicarGema gema) personaje

utilizar :: Guantele -> Personaje -> Personaje
utilizar guantele personaje = foldr aplicarGema personaje (reverse $ gemas guantele)

gemaMasPoderosa :: Guantele -> Personaje -> Gema
gemaMasPoderosa guante personaje = gemaMasPoderosa' personaje (gemas guante)

gemaMasPoderosa' :: Personaje -> [Gema] -> Gema
gemaMasPoderosa' _ [gema] = gema
gemaMasPoderosa' personaje (gema1:gema2:demasGemas)
    | energia (aplicarGema gema1 personaje) > energia (aplicarGema gema2 personaje) = gemaMasPoderosa' personaje (gema1:demasGemas)
    | otherwise = gemaMasPoderosa' personaje (gema2:demasGemas)

-- gemaMasPoderosa punisher guanteleteDeLocos no se puede ejecutar porque no tiene restriccion de evalucion, la ejecucion sigue comparando por un elemento mayor al anterior.
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher se puede ejecutar ya que acorta la evaluacion de la lista infinita a una constante, por lo que la evaluacion diferida toma esa restriccion y evalua la lista
