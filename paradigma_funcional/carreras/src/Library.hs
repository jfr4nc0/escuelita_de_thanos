module Library where
import PdePreludat

data Color = Azul | Rojo | Blanco | Negro
    deriving (Show, Eq, Ord)

data Auto = Auto{
    color::Color,
    velocidad::Number,
    distancia::Number
}deriving(Show, Eq)

type Carrera = [Auto]

autosDistintos :: Auto -> Auto -> Bool
autosDistintos auto1 auto2 = color auto1 /= color auto2

mismoAuto :: Auto -> Auto -> Bool
mismoAuto auto1 auto2 = color auto1 == color auto2

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
    velocidad = max (alteracion $ velocidad auto) 0
}

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad valor auto = alterarVelocidad (+ (-valor)) auto 

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
    = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Maybe Number -> Color -> Carrera -> Carrera
terremoto :: PowerUp
terremoto (Nothing) color carrera = afectarALosQueCumplen (estaCerca (getAutoByColor color carrera)) (bajarVelocidad 50) carrera

miguelitos :: PowerUp
miguelitos (Just valor) color carrera = afectarALosQueCumplen (vaGanando (getAutoByColor color carrera)) (bajarVelocidad valor) carrera

jetpack :: PowerUp
jetpack (Just tiempo) color carrera = afectarALosQueCumplen (mismoAuto (getAutoByColor color carrera)) ((corra tiempo).(alterarVelocidad (*2))) carrera

getAutoByColor :: Color -> Carrera -> Auto
getAutoByColor colorId carrera = head(filter ((==colorId).color) carrera)

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Number, Color)]
simularCarrera carrera eventos = getEstadoFinal (foldr ($) carrera (reverse eventos))

correnTodos :: Number -> Carrera -> Carrera
correnTodos tiempo carrera = map (corra tiempo) carrera

usaPowerUp :: (PowerUp) -> (PowerUp)
usaPowerUp powerup = powerup

getEstadoFinal :: Carrera -> [(Number, Color)]
getEstadoFinal carrera = quicksort (map (flip estadoFinal carrera) carrera)

estadoFinal :: Auto -> Carrera -> (Number, Color)
estadoFinal auto carrera = (puesto auto carrera,color auto)

quicksort :: [(Number, a)] -> [(Number, a)]
quicksort [] = []
quicksort ((x, y):xs) =
    let lesser = quicksort [(a, b) | (a, b) <- xs, a <= x]
        greater = quicksort [(a, b) | (a, b) <- xs, a > x]
    in lesser ++ [(x, y)] ++ greater