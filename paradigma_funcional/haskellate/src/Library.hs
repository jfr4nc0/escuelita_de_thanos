{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat

data Ingrediente = Ingrediente{
    tipo :: String,
    calorias :: Number,
    unidades :: Number
} deriving (Show, Eq, Ord)

data Chocolate = Chocolate{
    ingredientes :: [Ingrediente],
    nombre :: String ,
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

data TipoChocolate = Frutalizado String Number| DulceDeLeche | CeliaCrucera Number| Embriagadora Number
    deriving (Show, Eq, Ord)

aplicarProceso :: Chocolate -> TipoChocolate -> Chocolate
aplicarProceso chocolate (Frutalizado tipo unidades) = chocolate{
    ingredientes = Ingrediente{tipo=(++tipo)"Frutalizado ",calorias=(*2)unidades,unidades=unidades} : ingredientes chocolate
}
aplicarProceso chocolate DulceDeLeche = chocolate{
    ingredientes = Ingrediente{tipo="Dulce de Leche",calorias=220,unidades=1} : ingredientes chocolate,
    nombre = (++ " tentacion") . nombre $ chocolate
}
aplicarProceso chocolate (CeliaCrucera porcentaje) = chocolate{
    ingredientes = Ingrediente{tipo="Celia Crucera",calorias=0,unidades=1} : ingredientes chocolate,
    porcentajeAzucar = (+ porcentaje) . porcentajeAzucar $ chocolate 
}
aplicarProceso chocolate (Embriagadora grados) = chocolate{
    ingredientes = Ingrediente{tipo="Licor",calorias=min grados 30, unidades=1} : ingredientes chocolate,
    porcentajeAzucar = (+ 20) . porcentajeAzucar $ chocolate
}

aplicarReceta :: Chocolate -> [TipoChocolate] -> Chocolate
aplicarReceta = foldr $ flip aplicarProceso


