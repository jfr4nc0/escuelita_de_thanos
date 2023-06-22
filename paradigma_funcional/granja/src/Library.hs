module Library where
import PdePreludat

data TipoAnimal = Vaca | Lobo
    deriving (Show, Eq, Ord)

data Animal = Animal {
    nombre :: [Char],
    tipoAnimal :: TipoAnimal,
    peso :: Number,
    edad :: Number,
    enfermo :: Bool,
    recuperacion :: VisitaMedica
} deriving (Show, Eq, Ord)

data VisitaMedica = VisitaMedica{
    diasRecuperacion :: Number,
    costoVisita :: Number
} deriving (Show, Eq, Ord)

-- visitaMedica :: Animal -> VisitaMedica -> DiasRecuperacion
-- visitaMedica animal = 

laPasoMal :: Animal -> [VisitaMedica] -> Bool
laPasoMal animal = any ((>30) . diasRecuperacion)

-- laPasoMal' :: Animal -> Bool
-- laPasoMal' animal = (&& enfermo animal) ((>30) . diasRecuperacion $ recuperacion animal)

nombreFalopa :: Animal -> Bool
nombreFalopa = (=='i') . last . nombre

data Actividad = Engorde Number | Revisacion VisitaMedica | FestejoCumple | ChequeoPeso Number
    deriving (Show, Eq, Ord)

aplicarActividad :: Actividad -> Animal -> Animal
aplicarActividad (Engorde kilos) animal = animal{
    peso = (+) (min 5 $ div kilos 2) $ peso animal
}
aplicarActividad (Revisacion visita) animal = animal{
    peso = peso $ aplicarActividad (Engorde 2) animal,
    recuperacion = VisitaMedica{diasRecuperacion = diasRecuperacion visita, costoVisita = costoVisita visita}
}
aplicarActividad FestejoCumple animal = animal{
    edad = (+1) . edad $ animal,
    peso = (-) (peso animal) 1
}
aplicarActividad (ChequeoPeso limitePeso) animal = animal{
    enfermo = (< limitePeso) . peso $ animal
}

aplicarProceso :: Animal -> [Actividad] -> Animal
aplicarProceso animal actividades = foldr aplicarActividad animal $ reverse actividades

-- proceso = aplicarProceso animal [Engorde 5, Revisacion VisitaMedica{diasRecuperacion=29,costoVisita=400}, FestejoCumple, ChequeoPeso 50]

mantienePeso :: Animal -> Actividad -> Bool
mantienePeso animal actividad = (>= peso animal) . peso $ aplicarActividad actividad animal

noSubePeso :: Animal -> Actividad -> Bool
noSubePeso animal actividad = ( < ((+3) . peso $ animal)) . peso $ aplicarActividad actividad animal

mejoraONoMejora :: Animal -> [Actividad] -> Bool
mejoraONoMejora animal [] = True
mejoraONoMejora animal (actividad:demasActividades)
    |   (&&) (mantienePeso animal actividad) (noSubePeso animal actividad) = mejoraONoMejora animal demasActividades
    |   otherwise = False

primerosTresConNombreFalopa :: [Animal] -> [Animal]
primerosTresConNombreFalopa animales = take 3 $ filter nombreFalopa animales

-- Si se puede evaluar una funcion infinita de elementos con la funcion anterior ya que establece una restriccion (take n) para que la evaluacion diferida calcule los elementos que necesita,
-- si no estuviera esa condicion, no se podria llegar a un valor ya que se filtraria una lista infinita con el criterio establecido (nombreFalopa)

-- Ejemplo de evaluacion diferida en lista infinita: primerosTresConNombreFalopa $ repeat vaca