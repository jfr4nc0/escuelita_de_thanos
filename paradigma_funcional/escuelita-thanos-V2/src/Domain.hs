module Domain where
import PdePreludat
import Library

ironMan :: Personaje
ironMan = Personaje{
    edad = 50,
    energia = 100,
    habilidades = ["romperCosas"],
    nombre = "Tony Stark",
    planeta = "Tierra"
}

drStrange :: Personaje
drStrange = Personaje{
    edad = 50,
    energia = 100,
    habilidades = ["romperCosas"],
    nombre = "Doctor Strange",
    planeta = "Tierra"
}

groot :: Personaje
groot = Personaje{
    edad = 50,
    energia = 100,
    habilidades = ["romperCosas","nose"],
    nombre = "Groot",
    planeta = "Marte"
}

wolverine :: Personaje
wolverine = Personaje{
    edad = 100,
    energia = 200,
    habilidades = ["romperCosas","nose","usar Mjolnir","programacion en Haskell"],
    nombre = "Wolverine",
    planeta = "Tierra"
}

universo1 :: Universo
universo1 = [ironMan,drStrange,groot,wolverine]