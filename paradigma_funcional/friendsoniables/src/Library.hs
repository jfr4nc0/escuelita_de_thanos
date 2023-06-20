{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Library where
import PdePreludat

------------------------------------------ PUNTO 1 -----------------------------------------------

type Yulius = Number
type Alegronios = Number
type Nerviofrinas = Number
type Tarea = Persona -> Persona
type Estresometro = Number
data Persona = Persona {
    nombre :: String,
    edad :: Number,
    nivelEnergia :: Yulius, -- es funcion
    nivelAlegria :: Alegronios,
    nivelAmsiedad :: Nerviofrinas,
    nivelEstres :: Estresometro,
    tareasPendientes :: [Tarea] -- es funcion
}deriving(Show,Eq,Ord)

calcularNivelEstres :: Persona -> Persona
calcularNivelEstres persona
    | length (tareasPendientes persona) > 5 = persona { nivelEstres = ((* 1.5).nivelAmsiedad) persona}
    | otherwise = persona { nivelEstres = nivelAmsiedad persona}

masAlegreQueAnsioso :: Persona -> Bool
masAlegreQueAnsioso persona = ((< nivelAlegria persona). nivelAmsiedad) persona

masAnsiosoQueAlegreYJoven :: Persona -> Bool
masAnsiosoQueAlegreYJoven persona = (> edad persona) 40 && not (masAlegreQueAnsioso persona)

calcularNivelEnergia :: Persona -> Persona
calcularNivelEnergia persona
    | masAlegreQueAnsioso persona = persona { nivelEnergia = min (nivelAlegria persona) 340}
    | masAnsiosoQueAlegreYJoven persona = persona { nivelEnergia = ((300-).nivelEstres) persona}
    | otherwise = persona { nivelEnergia = (+10) (nivelAlegria persona)}

instaciarPersona :: Persona -> Persona
instaciarPersona = calcularNivelEnergia . calcularNivelEstres

getNivelEnergia :: Persona -> Yulius
getNivelEnergia persona = nivelEnergia (instaciarPersona persona)

------------------------------------------ PUNTO 2 -----------------------------------------------
esJovato :: Persona -> Bool
esJovato = (>=40) . edad

esVital :: Persona -> Bool
esVital = (>100) . getNivelEnergia

cuantoDueleVerLasBuenas :: [Persona] -> [Persona]
cuantoDueleVerLasBuenas grupo = map instaciarPersona (filter (\persona -> esVital persona && esJovato persona) grupo)

-- nivelTotalDeAmsiedad grupo = foldr (+) 0 (map (nivelAmsiedad) (filter esJovato grupo))
nivelTotalDeAmsiedad :: [Persona] -> Number
nivelTotalDeAmsiedad grupo = foldr ((+) . nivelAmsiedad) 0 (filter esJovato grupo)

type Criterio = Persona -> Bool
losMasCriticados :: [Persona] -> Criterio -> [String]
losMasCriticados grupo criterio = map nombre (take 2 (filter criterio grupo))

-- 1) losMasCriticados grupo ((>50) . nivelAmsiedad)
-- 2) losMasCriticados grupo (even . getNivelEnergia)

------------------------------------------ PUNTO 3 ----------------------------------------------
-- 1ra Implementacion : Typeclass y funciones
descomprimirPersona :: Tarea
descomprimirPersona persona = instaciarPersona persona{nivelEstres = ((10-) . nivelEstres) persona}

codearUnProyectoNuevo :: Tarea
codearUnProyectoNuevo persona = descomprimirPersona persona{
    nivelAlegria = ((+100) . nivelAlegria) persona,
    nivelAmsiedad = ((+50) . nivelAmsiedad) persona
}

hacerTramitesEnAfip :: Number -> Tarea
hacerTramitesEnAfip cant persona = descomprimirPersona persona{
    nivelAmsiedad = max (cant * nivelAmsiedad persona) 300
}

andarEnBici :: Tarea
andarEnBici persona = descomprimirPersona persona{
    nivelAmsiedad = 0,
    nivelAlegria = ((+50) . nivelAlegria) persona
}

escucharMusica :: Tarea
escucharMusica persona = descomprimirPersona persona{
    nivelAmsiedad = max (nivelAmsiedad persona - 10) 0
}
class InterfazTareas a b where
    aplicarTarea :: a -> b -> a


instance InterfazTareas Persona Tarea where
    aplicarTarea :: Persona -> Tarea -> Persona
    aplicarTarea persona tarea = tarea persona

-- 2da Implementacion : Multiples Constructores
data TareaType = CodearUnProyectoNuevo | HacerTramitesEnAfip Number | AndarEnBici | EscucharMusica
aplicarTarea' :: TareaType -> Persona -> Persona
aplicarTarea' CodearUnProyectoNuevo persona = descomprimirPersona persona{
    nivelAlegria = ((+100) . nivelAlegria) persona,
    nivelAmsiedad = ((+50) . nivelAmsiedad) persona
}
aplicarTarea' (HacerTramitesEnAfip cant) persona = descomprimirPersona persona{
    nivelAmsiedad = max (cant * nivelAmsiedad persona) 300
}
aplicarTarea' AndarEnBici persona = descomprimirPersona persona{
    nivelAmsiedad = 0,
    nivelAlegria = ((+50) . nivelAlegria) persona
}
aplicarTarea' EscucharMusica persona = descomprimirPersona persona{
    nivelAmsiedad = max (nivelAmsiedad persona - 10) 0
}

-------------------------------------------- PUNTO 4 -----------------------------------------------
data TareaConstructor = TareasPendientes
    deriving (Eq, Ord, Show, Enum)

instance InterfazTareas Persona [Tarea] where
    aplicarTarea :: Persona -> [Tarea] -> Persona
    aplicarTarea persona tareas = foldr ($) persona (reverse tareas)

instance InterfazTareas Persona TareaConstructor where
    aplicarTarea :: Persona -> TareaConstructor -> Persona
    aplicarTarea persona TareasPendientes = (foldr ($) persona (reverse (tareasPendientes persona))){
        tareasPendientes = []
    }

-------------------------------------------- PUNTO 5 -----------------------------------------------
-- Modelar hiceLoQuePude para una persona y una serie de tareas . 
-- La persona intenta tomar de a una las tareas y realizarlas, siempre y cuando la tarea lo deje con más de 100 yulius de energía. 
-- Si cumple con la condición la ejecuta y pasa a la siguiente, pero si no supera este valor, 
-- deja de hacer tareas y la persona queda en dicho estado. Resolver este punto con recursividad.

hiceLoQuePude :: Persona -> [Tarea] -> Persona
hiceLoQuePude persona [] = persona
hiceLoQuePude persona (tarea:demasTareas)
    | ((>100) . nivelEnergia) (aplicarTarea persona tarea) = hiceLoQuePude (aplicarTarea persona tarea) demasTareas
    | otherwise = persona

-------------------------------------------- PUNTO 6 -----------------------------------------------
-- Dada una lista de personas infinitas, ¿podemos determinar el nivelTotalDeAmsiedad o cuantoDueleVerLasBuenas?
muchosDeUno :: t -> [t]
muchosDeUno n = n:muchosDeUno n

muchosDeMuchos :: [t] -> [t]
muchosDeMuchos n = n++muchosDeMuchos n

-- Primero que en el caso de cuantoDueleVerlasBuenas devuelve un resultado por cada persona en el grupo, por lo que la evalucion es infinita.
-- En el caso del nivelTotalDeAmsiedad, evalua el nivel de ansiedad filtrando a los jovatos, y como puede haber siempre una persona mas que cumpla
-- el criterio, la evalucion es infinita y rompe el stack. En ambos casos, aunque se aplique evaluacion diferida, no resuelven a un valor.