module Library where
import PdePreludat

data Auto = Auto{
    color::String,
    velocidad::Number,
    distancia::Number,
    posicion::Number
}deriving(Show, Eq)

type Carrera = [Auto]

autosDistintos :: Auto -> Auto -> Bool
autosDistintos auto1 auto2 = color auto1 /= color auto2

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = autosDistintos auto1 auto2 && ((<10).distanciaEntreAutos auto1) auto2

distanciaEntreAutos :: Auto -> Auto -> Number
distanciaEntreAutos auto1 auto2 = abs (distancia auto1 - distancia auto2)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = all (not.estaCerca auto) carrera && autoConMayorDistancia auto carrera

carreraLimpia :: Auto -> Carrera -> Carrera
carreraLimpia auto carrera = filter ((/= color auto).color) carrera

autoConMayorDistancia :: Auto -> Carrera -> Bool
autoConMayorDistancia auto carrera = all ((< distancia auto).distancia) (carreraLimpia auto carrera)

puesto :: Auto -> Carrera -> Number
puesto auto carrera = 1 + length (filter (vaGanando auto) (carreraLimpia auto carrera))

vaGanando :: Auto -> Auto -> Bool
vaGanando auto1 auto2 = distancia auto1 < distancia auto2

corra :: Number -> Auto -> Auto
corra tiempo auto = auto{
    distancia = (+distancia auto).(*tiempo) $ velocidad auto
}

alterarVelocidad :: (Number -> Number) -> Auto -> Auto
alterarVelocidad alteracion auto = auto{
    velocidad = alteracion $ velocidad auto
}

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad valor auto = alterarVelocidad (+ (-valor)) auto 