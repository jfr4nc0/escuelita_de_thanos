module Spec where
import PdePreludat
import Test.Hspec
import Library
import Domain
import Control.Exception (evaluate)


correrTests :: IO ()
correrTests = hspec $ do

{--
  describe "Tests de punto 1 - Integrante I - Pelicula Daniresca" $ do
    it "Test pelicula con darin que no es el principal" $ do
      esDarinesca nueveReinas `shouldBe` False
    it "Test pelicula sin darin" $ do
      esDarinesca laFlor `shouldBe` False
    it "Test pelicula con darin como principal" $ do
      esDarinesca laOdiseaDeLosGiles `shouldBe` True

  describe "Test de punto 1 - Integrante II - Pinta Buena" $ do
    it "Test de una pelicula que tiene mas de 5 actores" $ do
      pintaBuena laOdiseaDeLosGiles `shouldBe` True
    it "Test de una pelicula que tiene menos de 5 actores" $ do
      pintaBuena armaMortal `shouldBe` False      

  describe "Tests de punto 1 - Integrante III - Minutos Excedentes" $ do
    it "Pelicula con mas de 115 minutos de duracion" $ do
      minutosExcedentes laOdiseaDeLosGiles `shouldBe` 1
    it "Pelicula con menos de 115 minutos de duracion" $ do
      minutosExcedentes armaMortal `shouldBe` 6  

  describe "Tests de punto 2 - Integrante I - Precio Base" $ do
    it "Precio Base para una pelicula que pinta grosa" $ do
      precioBase laOdiseaDeLosGiles `shouldBe` 200
    it "Precio base para una pelicula con menos de 5 actores y vieja" $ do
      precioBase armaMortal `shouldBe` 22
    it "Precio Base para una pelicula con menos de 5 actores y nueva" $ do
      precioBase nueveReinas `shouldBe` 124

  
  describe "Tests de punto 2 - Integrante II - Precio Extra" $ do
    it "Precio Extra para una pelicula larga" $ do
      precioExtra laOdiseaDeLosGiles `shouldBe` 10
    it "Precio Extra para una pelicula que supera el maximo de minutos excedentes" $ do
      precioExtra laFlor `shouldBe` 100
    it "Precio Extra para una pelicula que no es larga pero no es vieja" $ do
      precioExtra nueveReinas `shouldBe` 50
    it "Precio extra para una pelicula que no es larga  y es vieja" $ do
      precioExtra armaMortal `shouldBe` 0

  describe "Tests de punto 2 - Integrante III - Precio Total" $ do
    it "Precio total para una pelicula que supera el precio total minimo para aplicar descuento" $ do
      precioTotal laOdiseaDeLosGiles `shouldBe` 189
    it "Precio total para una pelicula que no alcanza el precio total minimo para aplicar descuento" $ do
      precioTotal armaMortal `shouldBe` 22

  describe "Tests de punto 3 - Genero" $ do
    it "Persona que vio 3 filmaciones  y que ve algun genero" $ do
      cantFilmacionesVistas (genero joan laFlor) `shouldBe` 4
    it "Persona que ve la odisea de los giles y tiene $1500" $ do
      saldoDisponible (genero joan laOdiseaDeLosGiles) `shouldBe` 1311
    it "Persona que ve la odisea de los giles y tiene $10" $ do
      saldoDisponible (genero uriel laOdiseaDeLosGiles) `shouldBe` 0

  describe "Tests de punto 3 - Integrante I - Terror y Comedia" $ do
    it "Persona con 20 de satisfaccion que ve una pelicula cualquiera de terror con 5 litros de sangre" $ do
      satisfaccion (aplicarTerror 5 joan laOdiseaDeLosGiles) `shouldBe` 15
    it "Persona que ve una pelicula de comedia con 20 de satisfaccion" $ do
      satisfaccion (aplicarComedia joan laOdiseaDeLosGiles) `shouldBe` 40
    it "Persona Pepe que ve una pelicula de comedia" $ do
      nombre (aplicarComedia personaPepe laOdiseaDeLosGiles) `shouldBe` "Pepe muy alegre"
      describe "Tests de punto 3 - Integrante II - Drama y Accion" $ do  
    it "Persona que ve una pelicula de drama de 2 escenas felices con 30 años" $ do
      edad (aplicarDrama 2 personaPepe laOdiseaDeLosGiles ) `shouldBe` 31
    it "Persona que ve una pelicula de drama de 2 escenas felices con 20 de satisfaccion" $ do
      satisfaccion (aplicarDrama 2 personaPepe laOdiseaDeLosGiles ) `shouldBe` 22
    it "Persona que ve una pelicula de drama de 5 escenas felices con 20 de satisfaccion" $ do
      satisfaccion (aplicarDrama 5 personaPepe laOdiseaDeLosGiles ) `shouldBe` 23
    it "Persona que ve la pelicula de accion Speed con 20 de satisfaccion" $ do
      satisfaccion (aplicarAccion speed personaPepe) `shouldBe` 120
    it "Persona que ve la pelicula de accion Arma Mortal con 20 de satisfaccion" $ do
      satisfaccion (aplicarAccion armaMortal personaPepe) `shouldBe` 20
    
  describe "Tests de punto 3- Integrante III - Tragicomico y Aventura" $ do  
    it "Persona que ve la pelicula de tragicomedia La odisea de los giles con 20 de satisfaccion y 30 años" $ do
      edad (aplicarTragicomico personaPepe laOdiseaDeLosGiles) `shouldBe` 31
    it "Persona que ve la pelicula de tragicomedia La odisea de los giles con 20 de satisfaccion y 30 años" $ do
      satisfaccion (aplicarTragicomico personaPepe laOdiseaDeLosGiles) `shouldBe` 43
    it "Persona que ve la pelicula de tragicomedia La odisea de los giles con 20 de satisfaccion y 30 años" $ do
      nombre (aplicarTragicomico personaPepe laOdiseaDeLosGiles) `shouldBe` "Pepe muy alegre"
    it "Persona con un nivel del satisfaccion 20 que piensa que la saga IV de Indiana Jones es mala, aplica aventura con 'Indiana Jones IV'" $ do
      satisfaccion (aplicarAventura (definirCriterioSagaMala joan sagaIndianaJones 4) indianaJones 4) `shouldBe` 20
    it "Persona con un nivel del satisfaccion 20 que piensa que la saga IV de Indiana Jones es mala, aplica aventura con 'Indiana Jones I'" $ do
      satisfaccion (aplicarAventura (definirCriterioSagaMala joan sagaIndianaJones 4) indianaJones 1) `shouldBe` 40
    it "Persona con un nivel del satisfaccion 20 que piensa que la saga IV de Indiana Jones es mala, aplica aventura con 'nueveReinas'" $ do
      satisfaccion (aplicarAventura (definirCriterioSagaMala joan sagaIndianaJones 4) nueveReinas 4) `shouldBe` 20
--}

-----------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------ SEGUNDA PARTE --------------------------------------------------------------------------------------------------------

  describe "Test de punto 4 - Primera orden del tp - Luz, camara ..." $ do
    it "Persona que ve la filmacion 'La odisea de los giles'" $ do
      saldoDisponible (verFilmacion personaPepe laOdiseaDeLosGiles) `shouldBe` 1311
    it "Persona que ve la filmacion 'La odisea de los giles'" $ do
      cantFilmacionesVistas (verFilmacion personaPepe laOdiseaDeLosGiles) `shouldBe` 4
    it "Persona que ve la filmacion 'La odisea de los giles'" $ do
      satisfaccion (verFilmacion personaPepe laOdiseaDeLosGiles) `shouldBe` 40
    it "Persona que ve la filmacion 'La odisea de los giles ' y 'Speed'" $ do
      saldoDisponible (verFilmaciones personaPepe [laOdiseaDeLosGiles,speed]) `shouldBe` 1122
    it "Persona que ve la filmacion 'La odisea de los giles ' y 'Speed'" $ do
      cantFilmacionesVistas (verFilmaciones personaPepe [laOdiseaDeLosGiles,speed]) `shouldBe` 5
    it "Persona que ve la filmacion 'La odisea de los giles ' y 'Speed'" $ do
      satisfaccion (verFilmaciones personaPepe [laOdiseaDeLosGiles,speed]) `shouldBe` 140

  describe "Test de punto 4 - Integrante I - Never Pony" $ do
    it "Never pony con la filmacion speed para el grupo de televidentes formado por Pepe y Moni" $ do
      esNeverPony speed [personaPepe,personaMoni] `shouldBe` True
    it "never pony con la filmación arma mortal para el grupo de televidentes formado por Pepe y Moni" $ do
      esNeverPony armaMortal [personaPepe,personaMoni] `shouldBe` False

  describe "Test de punto 4 - Integrante II - Combo vendible" $ do
    it "Combo vendible de las filmaciones speed y nueve reinas para Pepe y Moni" $ do
      esComboVendible [speed,nueveReinas] [personaPepe,personaMoni] `shouldBe` True
    it "Combo vendible de las filmaciones speed y nueve reinas " $ do
      esComboVendible [speed,nueveReinas] [personaPepe,personaMoni,personaCoky] `shouldBe` False

  describe "Tests de punto 4 - Integrante III - ¡Deme Dos!" $ do
    it "Las filmaciones danirescas para el conjunto [nueveReinas, laOdiseaDeLosGiles, elSecretodeSusOjos]" $ do
        lasDosPrimerasDarinescas  [nueveReinas,laOdiseaDeLosGiles, elSecretoDeSusOjos] `shouldBe` ["La odisea de los giles", "El secreto de sus ojos"]
    it "Las filmaciones danirescas para el conjunto [nueveReinas, elSecretodeSusOjos]" $ do
        lasDosPrimerasDarinescas  [nueveReinas, elSecretoDeSusOjos] `shouldBe` ["El secreto de sus ojos"]
    it "Las filmaciones danirescas para el conjunto [nueveReinas, laFlor]" $ do
        lasDosPrimerasDarinescas  [nueveReinas,laFlor] `shouldBe` []

  describe "Test de punto 5 - Integrante I - Hasta donde de la billetera" $ do
    it "Coky ve hasta donde le da la billetera con las filmaciones tituladas 'Indiana Jones', 'La Flor', 'Arma Mortal' y 'Speed'" $ do
      saldoDisponible (verHastaDondeDeLaBilletera personaCoky [indianaJones,laFlor,armaMortal,speed]) `shouldBe` 2
    it "Coky ve hasta donde le da la billetera con las filmaciones '9 Reinas' y 'La Flor'." $ do
      saldoDisponible (verHastaDondeDeLaBilletera personaCoky [nueveReinas,laFlor]) `shouldBe` 50

  describe "Test de punto 6 - Me quedo con los primeros" $ do
    it "meQuedoConLosPrimeros menores a 3 de una lista que va del 1 al 100" $ do
       meQuedoConLosPrimeros (<3) [1..100] `shouldBe` [1,2]
    it "meQuedoConLosPrimeros menores a 3 de una lista compuesta por [5,1,2,3]" $ do
      meQuedoConLosPrimeros (<3) [5,1,2,3] `shouldBe` []

  describe "Test de punto 6 - La que le gusta a los Stones" $ do
      it "laPulenta para coky con un nivel de satisfacción 200 y la filmación 'La odisea de los giles'" $ do
        laPulenta personaCoky 200 laOdiseaDeLosGiles `shouldBe` True
      it "laPulenta para coky con un nivel de satisfacción 300 y la filmación 'La odisea de los giles'" $ do
        laPulenta personaCoky 300 laOdiseaDeLosGiles `shouldBe` False
      it "laPulenta para coky con un nivel de satisfacción 200 y las filmaciones 'La odisea de los giles' y 'La Flor'" $ do
        laPulentaSuperior personaCoky 200 [laOdiseaDeLosGiles,laFlor] `shouldBe` True
      it "laPulenta para coky con un nivel de satisfacción 300 y la filmación 'La odisea de los giles' y 'La Flor'" $ do
        laPulentaSuperior personaCoky 300 [laOdiseaDeLosGiles,laFlor] `shouldBe` False

  describe "Test de punto 6 - Caprichito" $ do
      it "tengoCaprichitoCon calificaciones 6, 8 y 9 para 'La odisea de los giles'" $ do
        tengoCaprichitoConSuperior laOdiseaDeLosGiles [6,8,9] `shouldBe` True
      it "tengoCaprichitoCon calificaciones 6, 7 y 9 para 'La odisea de los giles'" $ do
        tengoCaprichitoConSuperior laOdiseaDeLosGiles [6,7,9] `shouldBe` False

  describe "Test de punto 6 - Integrante III - Show me the money" $ do
      it "mony tiene la teca para ver la odisea de los giles" $ do
        tienenLaTecaSuperior personaMoni laOdiseaDeLosGiles `shouldBe` True
      it "coky no tiene la teca para ver la odisea de los giles" $ do
        tienenLaTecaSuperior personaCoky laOdiseaDeLosGiles `shouldBe` False
      

  describe "Test de punto 7" $ do
      it "Lista infinita de filmaciones evaluada de forma diferida con lasDosPrimerasDarinescas" $ do
        (lasDosPrimerasDarinescas . muchosDeUno) laOdiseaDeLosGiles `shouldBe` ["La odisea de los giles","La odisea de los giles"]
      -- it "Lista infinita de filmaciones evaluada de forma diferida con esComboVendible" $ do
        -- (flip esComboVendible [personaPepe,personaMoni] . muchosDeMuchos) [speed,nueveReinas]  `shouldBe` 

  
  describe "Tests de punto 5- Integrante III - Reeeeeeee Manija" $ do
    it "Coky queda re manija despues de ver las filmaciones [armaMortal,nueveReinas,speed]" $ do
        reManija personaCoky [armaMortal,nueveReinas,speed] `shouldBe` True
    it "Coky no queda re manija despues de ver las filmaciones [nueveReinas,laFlor,speed]" $ do
        reManija personaCoky [nueveReinas,laFlor,speed] `shouldBe` False
    it "Coky falla por pattern matching al intentar quedar manija sin ver filmaciones" $ do
        evaluate (reManija personaCoky []) `shouldThrow` anyException        
