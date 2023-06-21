module Domain where
import PdePreludat
import Library

chocolatex = Chocolate{
    ingredientes = [],
    nombre = "Chocolate" ,
    gramos = 30,
    porcentajeCacao = 70,
    porcentajeAzucar = 10
}

punto4 = aplicarReceta chocolatex [Frutalizado "Naranja" 10,DulceDeLeche,Embriagadora 32]