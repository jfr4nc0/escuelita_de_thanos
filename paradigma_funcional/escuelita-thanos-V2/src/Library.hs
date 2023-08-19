module Library where
import PdePreludat

type Gema = Personaje -> Personaje
type Universo = [Personaje]

data Personaje = Personaje{
    nombre::String,
    edad::Number,
    energia::Number,
    planeta::String,
    habilidades::[String]
}deriving(Show)

data Guantalete = Guantalete{
    material::String,
    gemas::[Gema]
}deriving(Show)

usarChasquido :: Guantalete -> Universo -> Universo 
usarChasquido guante universo | guanteCompleto guante = eliminarMedioUniverso universo
    | otherwise = universo

guanteCompleto :: Guantalete -> Bool
guanteCompleto guante = ((=="uru").material) guante && ((==6).length.gemas) guante

eliminarMedioUniverso :: Universo -> Universo 
eliminarMedioUniverso universo = take (div (length universo) 2) universo

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((<=45).edad)

energiaTotal :: Universo -> Number
energiaTotal = sum.map energia.filter ((>1).length.habilidades) 

mente :: Number -> Gema
mente valor = quitarEnergia valor

quitarEnergia :: Number -> Gema
quitarEnergia valor personaje = personaje{
    energia = max (energia personaje - valor) 0
}

alma :: String -> Gema
alma habilidad personaje = quitarEnergia 10 personaje{
    habilidades = filter (/=habilidad) $ habilidades personaje
}

espacio :: String -> Gema
espacio planetaDestino personaje = quitarEnergia 20 personaje{
    planeta = planetaDestino
}

poder :: Gema
poder personaje = quitarEnergia (energia personaje) personaje{
    habilidades = quitarHabilidades personaje
}

quitarHabilidades :: Personaje -> [String]
quitarHabilidades personaje | ((<=2).length.habilidades) personaje = []
    | otherwise = habilidades personaje

tiempo :: Gema
tiempo personaje = quitarEnergia 50 personaje{
    edad = max (div (edad personaje) 2) 18
}

loca :: Gema -> Gema
loca gema = gema.gema

utilizar :: [Gema] -> Gema
utilizar listaGemas enemigo = foldr ($) enemigo $ listaGemas 

gemaMasPoderosa :: Personaje -> Guantalete -> Gema
gemaMasPoderosa personaje guantalete = gemaMasPoderosaDe personaje $ gemas guantalete

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:demasGemas)
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:demasGemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2:demasGemas)

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantalete
guanteleteDeLocos = Guantalete "vesconite" $ infinitasGemas tiempo

usoLasTresPrimerasGemas :: Guantalete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete