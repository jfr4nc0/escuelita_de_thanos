module Domain where
import PdePreludat
import Library

nacho :: Persona
nacho = Persona{
    nombre = "Nacho",
    edad = 23,
    nivelEnergia = 0,
    nivelAlegria = 10,
    nivelAmsiedad = 20,
    nivelEstres = 0,
    tareasPendientes = [codearUnProyectoNuevo]
}

joan :: Persona
joan = Persona{
    nombre = "Joan",
    edad = 45,
    nivelEnergia = 0,
    nivelAlegria = 150,
    nivelAmsiedad = 90,
    nivelEstres = 0,
    tareasPendientes = [andarEnBici,escucharMusica,hacerTramitesEnAfip 2]
}

uriel :: Persona
uriel = Persona{
    nombre = "Uriel",
    edad = 50,
    nivelEnergia = 0,
    nivelAlegria = 220,
    nivelAmsiedad = 80,
    nivelEstres = 0,
    tareasPendientes = []
}

juan :: Persona
juan = Persona{
    nombre = "Juan",
    edad = 41,
    nivelEnergia = 0,
    nivelAlegria = 120,
    nivelAmsiedad = 25,
    nivelEstres = 0,
    tareasPendientes = [andarEnBici,codearUnProyectoNuevo]
}

grupo = [nacho,uriel,juan,joan]