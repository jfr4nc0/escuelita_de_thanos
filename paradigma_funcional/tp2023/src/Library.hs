{-# OPTIONS_GHC -Wno-missing-fields #-}
module Library where
import PdePreludat
import Data.Maybe
--import GHC.Windows (failIfFalse_)
data Filmacion = Filmacion {
    titulo :: String,
    puntaje :: Number,
    duracion :: Number,
    anio :: Number,
    actores :: [String],
    tipoGenero :: Genero
}deriving(Show)

data Persona = Persona {
    nombre :: String,
    satisfaccion :: Number,
    edad :: Number,
    cantFilmacionesVistas :: Number,
    saldoDisponible :: Number,
    saga :: [String],
    versionMala :: Number
}deriving(Show);

type PrecioBase = Number

esVieja :: Filmacion -> Bool
esVieja = (<1990).anio

precioBase ::  Filmacion -> PrecioBase
precioBase filmacion
    | pintaGrosa filmacion = 200
    | esVieja filmacion = ((2*) . length . titulo) filmacion -- aplicando composicion
    | otherwise = ((100+) . (3*) . puntaje) filmacion


-- Se agrega momentaneamente una funcion esVieja2 que, si bien cumple la misma funcionalidad que
-- esVieja, se debe cambiar esta ultima dado que recibia un number en vez de la data de filmacion
-- luego hay que modificarlo

type PrecioExtra = Number

esLarga :: Filmacion -> Bool
esLarga = (>115).duracion

precioExtra :: Filmacion -> Number
precioExtra unaFilmacion | esLarga unaFilmacion = min (minutosExcedentes unaFilmacion * 10) 100
                         | (not.esVieja) unaFilmacion = 50
                         | otherwise = 0

{--Queremos saber el precioTotal que se compone 
de la suma del precio base más el precio extra,
pero con la condición que si la sumatoria es mayor a $200, 
se le aplica un 10% de descuento. --}

type PrecioTotal = Number

precioSubtotal :: Filmacion -> Number
precioSubtotal filmacion = sum [precioBase filmacion,precioExtra filmacion]

precioTotal :: Filmacion -> PrecioTotal
precioTotal filmacion
    | ((>200).precioSubtotal) filmacion = ((*0.9).precioSubtotal) filmacion
    | otherwise = precioSubtotal filmacion

-- f(x) = head(x); g(x) = aplicacion parcial; f(g(x))
-- aplicacion parcial de comparar dos strings, hardcodeando "Ricardo Darin"

esDarinesca :: Filmacion -> Bool
esDarinesca = (== "Ricardo Darin").head.actores

--Saber los minutos excedentes de una filmación que se calcula como
--el valor absoluto entre la diferencia de 115 minutos con una duración de una filmación

--Para ganar expresividad se me ocurrio llamar la funcion excedente
--que indica con aplicacion parcial cuantos minutos hay que restar a la duracion de la pelicula
--Entonces en la funcion 'minutosExcedentes' podria componerla como: abs.excedente.duracion
--Pero como no era posible usar funciones auxiliares se opto por descartar tal funcion
--excedente :: Number -> Number
--excedente = (-115 +)

minutosExcedentes :: Filmacion -> Number
minutosExcedentes = abs.(-115 +).duracion

-- se usa una composición simple y aplicación parcial para determinar si la cantidad de actores
-- en la lista homónima es de 5 o más actores
tipoPinta :: Number -> Filmacion -> Bool
tipoPinta number = (>= number).length.actores

pintaBuena :: Filmacion -> Bool
pintaBuena = tipoPinta 5

pintaGrosa = pintaBuena
-- Pinta Grosa y pinta Buena son equivalentes. Pinta buena es >= 5 participantes mientras que pinta Grosa es > 4 
-- por lo tanto el dominio es a partir de [5,inf)

-----------------------------------------------------------------------------------------------------------------------------


definirCriterioSagaMala :: Persona -> [String] -> Number -> Persona
definirCriterioSagaMala persona saga version = persona {
    saga = saga,
    versionMala = version
}

esSagaBuena :: Persona -> Filmacion -> Number -> Bool
esSagaBuena persona filmacion version = (elem.titulo) filmacion (saga persona) && (versionMala persona /= version)

---------------------------------------------------------------------
------------------------- CONSTRUCTORES -----------------------------

aplicarTerror :: Number -> Persona -> Filmacion -> Persona -- OK
aplicarTerror sangre = genero.terror sangre

aplicarComedia :: Persona -> Filmacion -> Persona -- OK 
aplicarComedia = genero . comedia

aplicarDrama :: Number -> Persona -> Filmacion -> Persona -- OK
aplicarDrama escenas = genero . drama escenas

aplicarAccion ::  Persona -> Filmacion -> Persona -- OK
aplicarAccion persona filmacion = genero (accion filmacion persona) filmacion

aplicarTragicomico :: Persona -> Filmacion -> Persona -- OK 
aplicarTragicomico = genero . tragiComico

aplicarAventura :: Number -> Persona -> Filmacion -> Persona -- OK
aplicarAventura version persona filmacion = genero (aventura version persona filmacion) filmacion

---------------------- < END CONTRUCTORES > -------------------------


---------------------------------------------------------------------
---------------------------- GENEROS --------------------------------

-- OK
type AplicarGenero = Persona -> Filmacion -> Persona
genero :: AplicarGenero
genero persona filmacion = persona {
    cantFilmacionesVistas = cantFilmacionesVistas persona + 1,
    saldoDisponible = saldoDisponible persona - min (precioTotal filmacion) (saldoDisponible persona)
}

-- OK
terror :: Number -> Persona -> Persona
terror cantSangre persona = persona {
    satisfaccion = satisfaccion persona - cantSangre
}

-- OK
comedia :: Persona -> Persona
comedia persona = persona {
    satisfaccion = satisfaccion persona * 2,
    nombre = nombre persona ++ " muy alegre"
}

-- OK
drama :: Number -> Persona -> Persona
drama  cantEscenasFelices persona
    | cantEscenasFelices < 3 = persona {
        edad = edad persona + 1,
        satisfaccion = satisfaccion persona + cantEscenasFelices
        }
    | otherwise = persona {
        edad = edad persona + 1,
        satisfaccion = satisfaccion persona + 3
        }

-- OK
accion :: Filmacion -> Persona -> Persona
accion filmacion persona
    | pintaBuena filmacion = persona {satisfaccion = satisfaccion persona + 100}
    | otherwise = persona

-- OK
escenasFelicesTragiComico :: Number
escenasFelicesTragiComico = 4
tragiComico :: Persona -> Persona
tragiComico =  drama escenasFelicesTragiComico . comedia

-- OK
aventura :: Number -> Persona -> Filmacion -> Persona
aventura version persona filmacion
    | esSagaBuena persona filmacion version = comedia persona
    | otherwise = persona

--------------------------- END GENEROS ----------------------------------


--------------------------- SEGUNDA PARTE ----------------------------------
--------------------------- PRIMERA ORDEN ----------------------------------
type Genero = Persona -> Filmacion -> Persona

verFilmacion :: Persona -> Filmacion -> Persona
verFilmacion persona filmacion = tipoGenero filmacion persona filmacion

verFilmaciones :: Persona -> [Filmacion] -> Persona
verFilmaciones persona filmaciones = foldr (flip verFilmacion) persona (reverse filmaciones)

------------------------------ PUNTO 4 -------------------------------------

------------------------- INTEGRANTE I - NEVER PONY ------------------------
aplicarFilmaciones :: Filmacion -> [Persona] -> [Persona]
aplicarFilmaciones filmacion = map (`verFilmacion` filmacion)

builderListaSatisfaccion :: Filmacion -> [Persona] -> [Number]
builderListaSatisfaccion film televidentes = map satisfaccion (aplicarFilmaciones film televidentes)

-- True = Filter a la persona con mayor nivel de satisfaccion despues de ver la pelicula y es mayor a 100 unidades
esNeverPony :: Filmacion -> [Persona] -> Bool
esNeverPony filmacion televidentes = any (> 100) (builderListaSatisfaccion filmacion televidentes)

------------------------ INTEGRANTE II - COMBO VENDIBLE ---------------------
builderListaPrecios :: [Filmacion] -> [Number]
builderListaPrecios = map precioTotal

builderListaSaldos :: [Persona] -> [Number]
builderListaSaldos = map saldoDisponible

-- esComboVendible :: [Filmacion] -> [Persona] -> Bool
-- esComboVendible combo televidentes =  (>=) (minimum (builderListaSaldos televidentes)) (minimum (builderListaPrecios combo))

minimumSuperior :: [Number] -> Number
minimumSuperior = foldr1 (\a1 a2 -> if (a1>a2) then a2 else a1)

-- foldr las listas para obtener los valores mas chicos de cada uno y comparo
--esComboVendible :: [Filmacion] -> [Persona] -> Bool
-- esComboVendible combo televidentes = (>=) (minimumSuperior (builderListaSaldos televidentes)) (minimumSuperior (builderListaPrecios combo))

--Combo vendible version 2
esComboVendible :: [Filmacion] -> [Persona] -> Bool
esComboVendible filmaciones personas =all ((< foldr1 min (builderListaSaldos personas)) . precioTotal) filmaciones

--------------------------- INTEGRANTE III - DEME DOS ------------------------
--lasDosPrimerasDarinescas  :: [Filmacion] -> [string] --tiene que devolver una lista de titulos de filmaciones danirescas
--lasDosPrimerasDarinescas = map titulo.filter esDarinesca
--lasDosPrimerasDarinescas filmaciones = take 2 (map titulo (filter esDarinesca filmaciones))

--NOTA: QUEDA PENDIENTE HACERLO POR COMPOSICION
lasDosPrimerasDarinescas  :: [Filmacion] -> [String]
lasDosPrimerasDarinescas filmaciones = take 2 (map titulo (filter esDarinesca filmaciones))

------------------------------------ PUNTO 5 --------------------------------

--------------- INTEGRANTE I - HASTA DONDE DE LA BILLETERA ------------------
plataSuficiente :: Persona -> Filmacion -> Bool
plataSuficiente persona filmacion = (>=) (saldoDisponible persona) (precioTotal filmacion)

verHastaDondeDeLaBilletera :: Persona -> [Filmacion] -> Persona
verHastaDondeDeLaBilletera persona [] = persona
verHastaDondeDeLaBilletera persona (filmacion:filmaciones)
    | plataSuficiente persona filmacion = verHastaDondeDeLaBilletera (verFilmacion persona filmacion) filmaciones
    | otherwise = verHastaDondeDeLaBilletera persona filmaciones

-------------------------- INTEGRANTE II - I CANT GET NO----------------------

condicioniCantGet :: Filmacion -> Persona -> Bool
condicioniCantGet filmacion persona = (>=200) (satisfaccion (verFilmacion persona filmacion))

iCantGetRecursivo :: Filmacion -> [Persona] -> [Persona]
iCantGetRecursivo filmacion [] = []
iCantGetRecursivo filmacion (persona:personas)
    | condicioniCantGet filmacion persona = verFilmacion persona filmacion : iCantGetRecursivo filmacion personas
    | otherwise = persona : iCantGetRecursivo filmacion personas

---------------------------- REEEEEEEE MANIJA ---------------------------------------
--NOTA HAY QUE REVISARLO PARA VER SI SE PUEDE DEFINIR MAS PROLIJO Y CON COMPOSICION
manija :: Persona -> [Filmacion] -> Bool
manija unaPersona [filmacion1,filmacion2] = satisfaccion (verFilmacion unaPersona filmacion1) < satisfaccion (verFilmacion unaPersona filmacion2)

reManija :: Persona -> [Filmacion] -> Bool
reManija unaPersona [unaPelicula, otraPelicula] = manija unaPersona [unaPelicula, otraPelicula]
reManija unaPersona (filmacion:filmaciones)
    | manija unaPersona [filmacion,head filmaciones] = reManija unaPersona filmaciones
    | otherwise = False

--reManija unaPersona [] = False NO HAY MATCHING


----------------------------- PUNTO 6 ------------------------------------

--------------------- ME QUEDO CON LOS PRIMEROS --------------------------
meQuedoConLosPrimeros :: (a -> Bool) -> [a] -> [a]
meQuedoConLosPrimeros f [] = []
meQuedoConLosPrimeros f (x:xs)
    | f x = x : meQuedoConLosPrimeros f xs
    | otherwise = []


---------------- INTEGRANTE I - LA QUE LE GUSTA A LOS STONES -------------
laPulenta :: Persona -> Number -> Filmacion -> Bool
laPulenta persona nivel filmacion = (> nivel) (satisfaccion (verFilmacion persona filmacion))

laPulentaSuperior :: Persona -> Number -> [Filmacion] -> Bool
laPulentaSuperior persona nivel = all (((> nivel) . satisfaccion) . verFilmacion persona)

-- Ejemplo: meQuedoConLosPrimeros (laPulenta personaCoky 200) [laOdiseaDeLosGiles,nueveReinas]

----------------------- INTEGRANTE II . CAPRICHITO-------------------------
tengoCaprichitoCon :: Filmacion -> Number -> Bool
tengoCaprichitoCon filmacion = (== puntaje filmacion)

tengoCaprichitoConSuperior :: Filmacion -> [Number] -> Bool
tengoCaprichitoConSuperior filmacion = elem (puntaje filmacion)

-- con General: meQuedoConLosPrimeros (tengoCaprichitoCon laOdiseaDeLosGiles) [8,9,10]
-- nota importante! revisar funcion meQuedoConLosPrimeros porque si no le pongo como primer elemento
-- un numero que de me true, me da lista vacia, creo que tiene que ver con el otherwise.

-------------------- INTEGRANTE III - SHOW ME THE MONEY--------------------
tienenLaTeca :: Persona -> Filmacion -> Bool
tienenLaTeca unaPersona unaFilmacion = saldoDisponible unaPersona >= precioTotal unaFilmacion

tienenLaTecaSuperior :: Persona -> Filmacion ->Bool
tienenLaTecaSuperior unaPersona unaFilmacion = (>=precioTotal unaFilmacion).saldoDisponible $ unaPersona

--Ejemplo con funcion general:
-- meQuedoConLosPrimeros (flip tienenLaTeca laOdiseaDeLosGiles) [personaPepe ,personaMoni,personaCoky]
--Devuelve a personaPepe y a personaMony ya que personaCoky no cumple con la condicion

------------------------------ PUNTO 7 ---------------------------------------
-- Si definimos una lista infinita de filmaciones, es decir de tipo data, y se invoca lasDosPrimerasDarinescas o comboVendible,
-- mediante Evalucion Diferida se llegara a un resultado tal que no necesita finalizar de recorrer toda la lista infinita para obtener los resultados que necesita.

muchosDeUno :: t -> [t]
muchosDeUno n = n:muchosDeUno n

muchosDeMuchos :: [t] -> [t]
muchosDeMuchos n = n++muchosDeMuchos n

-- esComboVendible con lista infinita no lo resulve por como esta implementado

-- Aplicando la funcion lasDosPrimerasDarinescas con una lista infinita de filmaciones ocurre:

-- Caso 1: Si la lista infinita esta definida por una cantidad infinita de filmaciones que no son darinescas como por ejemplo
-- "muchosDeUno armaMortal" entonces la funcion no termina nunca. Haskell intenta buscar en todos los elementos hasta encontrar
-- alguno que si cumpla

-- Caso 2: Si la lista infinita de filmaciones esta definida tal que existen peliculas darinescas como por ejemplo
-- "muchosDeMuchos [armaMortal, laFlor, elSecretoDeSusOjos,speed,nueveReinas,indianaJones,indianaJones4,laOdiseaDeLosGiles]"
-- Entonces gracias a como esta definida la funcion lasPrimerasDosDarinescas, explicitamente por la parte del "take 2"
-- lo que nos devuelve la funcion son las dos primeras darinescas que encuentra, en este caso
-- ["El secreto de sus ojos","La odisea de los giles"]

