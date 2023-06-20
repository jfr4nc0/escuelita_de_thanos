module Domain where
import PdePreludat
import Library

guanteleCompleto :: Guantele
guanteleCompleto = Guantele {
    material = Uru,
    gemas = []
}

guanteleIncompleto :: Guantele
guanteleIncompleto = Guantele {
    material = Hierro,
    gemas = []
}

guanteleGoma :: Guantele
guanteleGoma = Guantele {
    material = Goma,
    gemas = [Tiempo, Alma "usar Mjolnir", Loca (Alma "programacion en Haskell")]
}

ironMan :: Personaje
ironMan = Personaje{
    edad = 40,
    energia = 100,
    habilidades = ["romperCosas"],
    nombre = "Tony Stark",
    planeta = "Tierra"
}

drStrange :: Personaje
drStrange = Personaje{
    edad = 40,
    energia = 100,
    habilidades = ["romperCosas"],
    nombre = "Doctor Strange",
    planeta = "Tierra"
}

groot :: Personaje
groot = Personaje{
    edad = 40,
    energia = 100,
    habilidades = ["romperCosas","nose"],
    nombre = "Groot",
    planeta = "Marte"
}

wolverine :: Personaje
wolverine = Personaje{
    edad = 30,
    energia = 100,
    habilidades = ["romperCosas","nose","usar Mjolnir","programacion en Haskell"],
    nombre = "Wolverine",
    planeta = "Tierra"
}

universo1 :: Universo
universo1 = Universo{
    habitantes = [ironMan,drStrange,groot,wolverine]
}