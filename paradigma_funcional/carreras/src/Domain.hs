module Domain where
import PdePreludat
import Library

autoA :: Auto
autoA = Auto "verde" 200 100 0

autoB :: Auto
autoB = Auto "rojo" 200 105 0

autoC :: Auto
autoC = Auto "gris" 200 80 0

carrera1 = [autoA,autoB,autoC]