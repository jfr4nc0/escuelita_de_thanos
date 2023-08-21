module Domain where
import PdePreludat
import Library

autoA :: Auto
autoA = Auto Rojo 120 0

autoB :: Auto
autoB = Auto Blanco 120 0

autoC :: Auto
autoC = Auto Azul 120 0

autoD :: Auto
autoD = Auto Negro 120 0

carrera1 = [autoA,autoB,autoC,autoD]

eventos = [correnTodos 30,jetpack (Just 3) Azul,terremoto Nothing Blanco, correnTodos 40,miguelitos (Just 20) Blanco,jetpack (Just 6) Negro, correnTodos 10] -- devuelve una lista de [Carrera]
