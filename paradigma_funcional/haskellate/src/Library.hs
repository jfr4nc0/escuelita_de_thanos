{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat


data Ingrediente = Ingrediente{
    gusto :: TipoGustos,
    calorias :: Number,
    unidades :: Number
} deriving (Show, Eq, Ord)

data Chocolate = Chocolate{
    ingredientes :: [Ingrediente],
    nombre :: String,
    gramos :: Number,
    porcentajeCacao :: Number,
    porcentajeAzucar :: Number
} deriving (Show, Eq, Ord)

calcularPrecioPremium :: Chocolate -> Number
calcularPrecioPremium chocolate
    | (==0) . porcentajeAzucar $ chocolate = 8
    | otherwise = 5

calcularPrecioTotal :: Chocolate -> Number
calcularPrecioTotal chocolate
    | (>60) . porcentajeCacao $ chocolate = ( * calcularPrecioPremium chocolate) $ gramos chocolate
    | (>4) . length . ingredientes $ chocolate = (*8) . length . ingredientes $ chocolate
    | otherwise = (*1.5) $ gramos chocolate

esBombonAsesino :: Chocolate -> Bool
esBombonAsesino chocolate = any ((>200) . calorias) $ ingredientes chocolate

totalCalorias :: Chocolate -> Number
totalCalorias chocolate = sumOf calorias $ ingredientes chocolate

aptoParaNinios :: [Chocolate] -> [Chocolate]
aptoParaNinios chocolates = take 3 (filter (not . esBombonAsesino) chocolates)

data TipoGustos = Naranja | Frutilla | DulceDeLeche | CeliaCrucera | Licor
    deriving (Show, Eq, Ord)

data TipoChocolate = Frutalizado TipoGustos Number| DulceDeLeche' | CeliaCrucera' Number| Embriagadora Number
    deriving (Show, Eq, Ord)

aplicarProceso :: Chocolate -> TipoChocolate -> Chocolate
aplicarProceso chocolate (Frutalizado gusto unidades) = chocolate{
    ingredientes = Ingrediente{gusto=gusto,calorias=(*2) unidades,unidades=unidades} : ingredientes chocolate
}
aplicarProceso chocolate DulceDeLeche' = chocolate{
    ingredientes = Ingrediente{gusto=DulceDeLeche,calorias=220,unidades=1} : ingredientes chocolate,
    nombre = (++ " tentacion") . nombre $ chocolate
}
aplicarProceso chocolate (CeliaCrucera' porcentaje) = chocolate{
    ingredientes = Ingrediente{gusto=CeliaCrucera,calorias=0,unidades=1} : ingredientes chocolate,
    porcentajeAzucar = (+ porcentaje) . porcentajeAzucar $ chocolate
}
aplicarProceso chocolate (Embriagadora grados) = chocolate{
    ingredientes = Ingrediente{gusto=Licor,calorias=min grados 30, unidades=1} : ingredientes chocolate,
    porcentajeAzucar = (+ 20) . porcentajeAzucar $ chocolate
}

aplicarReceta :: Chocolate -> [TipoChocolate] -> Chocolate
aplicarReceta = foldr $ flip aplicarProceso

data Persona = Persona {
    nombrePersona :: String,
    caloriasIngeridas :: Number,
    limiteSaturacion :: Number,
    gustosRechazados :: [TipoGustos]
}

comerChocolate :: Persona -> Chocolate -> Persona
comerChocolate persona chocolate = persona{
    caloriasIngeridas = ( + totalCalorias chocolate) (caloriasIngeridas persona)
}

estaSaturado :: Persona -> Chocolate -> Bool
estaSaturado persona choco = ( > limiteSaturacion persona) (caloriasIngeridas $ comerChocolate persona choco)

rechazaGusto :: Persona -> [Ingrediente] -> Bool
rechazaGusto persona [ingrediente] = notElem (gusto ingrediente) $ gustosRechazados persona
rechazaGusto persona (ingrediente:demasIngredientes)
    | notElem (gusto ingrediente) $ gustosRechazados persona = rechazaGusto persona demasIngredientes

hastaAcaLlegue :: Persona -> [Chocolate] -> [Chocolate]
hastaAcaLlegue persona [] = []
hastaAcaLlegue persona (choco:demasChocos)
    | rechazaGusto persona $ ingredientes choco = hastaAcaLlegue persona demasChocos
    | estaSaturado persona choco = []
    | otherwise = choco : hastaAcaLlegue (comerChocolate persona choco) demasChocos

-- Caso "aptosParaNinios": Se puede evaluar la lista infinita ya que tiene una restriccion de 3 por lo que la evaluacion diferida tomara los primeros tres elementos que cumplan la condicion asignada
-- Caso "TotalCalorias": No se puede evaluar y llegar a un resultado con la lista infinita ya que carece de restricciones, por lo que la evaluacion diferida va a seguir aplicando elementos de la lista y sumandolo al total hasta romper el stack