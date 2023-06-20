{-# OPTIONS_GHC -Wno-missing-fields #-}
module Domain where
import PdePreludat
import Library 

------------------------------------------------------------
---------------------- PERSONAS ----------------------------

personaPepe :: Persona
personaPepe = Persona{
    nombre = "Pepe",
    satisfaccion = 20,
    edad = 30,
    cantFilmacionesVistas = 3,
    saldoDisponible = 1500,
    saga = [],
    versionMala = 0
}

joan :: Persona
joan = Persona {
    nombre = "Joan",
    satisfaccion = 20,
    edad = 22,
    cantFilmacionesVistas = 3,
    saldoDisponible = 1500,
    saga = [],
    versionMala = 0
}

uriel :: Persona
uriel = Persona{
    nombre = "Uriel",
    satisfaccion = 7,
    edad = 24,
    cantFilmacionesVistas = 5,
    saldoDisponible = 10,
    saga = [],
    versionMala = 0
}

personaMoni :: Persona
personaMoni = Persona{
    nombre = "Moni",
    satisfaccion = 50,
    edad = 31,
    cantFilmacionesVistas = 1,
    saldoDisponible = 5600,
    saga = [],
    versionMala = 0
}

personaCoky :: Persona
personaCoky = Persona{
    nombre = "Coky",
    satisfaccion = 120,
    edad = 20,
    cantFilmacionesVistas = 40,
    saldoDisponible = 50,
    saga = [],
    versionMala = 0
}

--------------------- < END PERSONAS > ---------------------



------------------------------------------------------------
------------------------ PELICULAS -------------------------

armaMortal :: Filmacion
armaMortal = Filmacion{
    titulo = "Arma Mortal",
    puntaje = 7,
    duracion = 109,
    anio = 1987,
    actores = ["Mel Gibson", "Danny Glover", "Gary Busey"],
    tipoGenero = aplicarAccion
}

nueveReinas :: Filmacion
nueveReinas = Filmacion {
    titulo = "9 Reinas",
    puntaje = 8,
    duracion = 114,
    anio = 2000,
    actores = ["Gastón Pauls", "Ricardo Darín", "Leticia Bredice", "Pochi Ducasse"],
    tipoGenero = aplicarDrama 5
}

laOdiseaDeLosGiles :: Filmacion
laOdiseaDeLosGiles = Filmacion {
    titulo = "La odisea de los giles",
    puntaje = 8,
    duracion = 116,
    anio = 2019,
    actores = ["Ricardo Darin", "Luis Brandoni", "Veronica Llinas", "Daniel Araoz", "Rita Cortese"],
    tipoGenero = aplicarComedia
}

laFlor :: Filmacion
laFlor = Filmacion {
    titulo = "La Flor",
    puntaje = 7,
    duracion = 840,
    anio = 2018,
    actores = ["Pilar Gamboa"],
    tipoGenero = aplicarTragicomico
}

speed :: Filmacion
speed = Filmacion {
    titulo = "Speed",
    puntaje = 7,
    duracion = 116,
    anio = 1994,
    actores = ["Keanu Reeves", "Sandra Bullock", "Dennis Hopper", "Jeff Daniels", "Alan Ruck"],
    tipoGenero = aplicarAccion
}

indianaJones4 :: Filmacion
indianaJones4 = Filmacion {
    titulo = "Indiana Jones IV",
    puntaje = 6,
    duracion = 125,
    anio = 2007,
    actores = ["Harrison Ford"],
    tipoGenero = aplicarAventura 4
}

indianaJones :: Filmacion
indianaJones = Filmacion {
    titulo = "Indiana Jones",
    puntaje = 8,
    duracion = 115,
    anio = 1981,
    actores = ["Harrison Ford"],
    tipoGenero = aplicarAventura 0
}

elSecretoDeSusOjos :: Filmacion
elSecretoDeSusOjos = Filmacion {
    titulo = "El secreto de sus ojos",
    puntaje = 9,
    duracion = 129,
    anio = 2009,
    actores = ["Ricardo Darin","Soledad Villamil"],
    tipoGenero = aplicarDrama 3
}

------------------- < END PELICULAS > ----------------------

------------------------------------------------------------
------------------------ SAGAS -----------------------------

sagaIndianaJones :: [String]
sagaIndianaJones = [titulo indianaJones,titulo indianaJones4]

-------------------- < END SAGAS > -------------------------
