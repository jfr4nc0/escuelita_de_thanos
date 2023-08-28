module Library where
import PdePreludat

data Material = Material {
    nombre :: String,
    calidad :: Number
} deriving (Show, Eq)

data Edificio = Edificio {
    tipoEdificio :: String,
    materiales :: [Material]
} deriving (Show, Eq)

data Aldea = Aldea {
    poblacion :: Number,
    materialesDisponibles :: [Material],
    edificios :: [Edificio]
} deriving (Show, Eq)

esValioso :: Material -> Bool
esValioso = (>20).calidad 

unidadesDisponibles :: String -> Aldea -> Number
unidadesDisponibles nombreMaterial aldea = length.filter ((==nombreMaterial).nombre) $ materialesDisponibles aldea

-- valorTotal :: Aldea -> Number
-- valorTotal aldea = calidadTotalUsada aldea + calidadTotalDisponible aldea

calidadTotalUsada :: Aldea -> Number
calidadTotalUsada aldea = sumOf (calidad) $ materialesDisponibles aldea

-- calidadTotalDisponible :: Aldea -> [[Material]]
-- -- calidadTotalDisponible aldea = sum.map.sumOf (calidad) materiales $ edificios aldea 
-- calidadTotalDisponible aldea = map (calidad.materiales) $ edificios aldea

recolectar :: Number -> Material -> Aldea
recolectar cant material aldea = aldea{
    materialesDisponibles = recolectarTotal cant [material]
    }

recolectarTotal :: Number -> [Material] -> [Material]
recolectarTotal 0 materiales = materiales
recolectarTotal cant (material:demasMateriales) = recolectarTotal cant-1 (material:demasMateriales)