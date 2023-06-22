module Domain where
import PdePreludat
import Library

vaca = Animal{
    nombre = "Vakiti",
    tipoAnimal = Vaca,
    peso = 12,
    edad = 10,
    enfermo = False,
    recuperacion = VisitaMedica{diasRecuperacion=0,costoVisita=0}
}

proceso :: Animal
proceso = aplicarProceso vaca [Engorde 5, Revisacion VisitaMedica{diasRecuperacion=29,costoVisita=400}, FestejoCumple, ChequeoPeso 50]

