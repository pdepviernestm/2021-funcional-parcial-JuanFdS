module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

barcoConTripulacion :: [Tripulante] -> Barco
barcoConTripulacion tripulacion = unBarco { tripulacion = tripulacion }

barcoDeTipo :: TipoDeBarco -> Barco
barcoDeTipo tipoDeBarco = unBarco { tipoDeBarco = tipoDeBarco }

correrTests :: IO ()
correrTests = hspec $ do
   describe "tripulantes" $ do
     describe "esta en las ultimas" $ do
       it "es verdad si tiene menos de 50 de energia" $ do
         estaEnLasUltimas (Tripulante 40) `shouldBe` True
       it "es falso si tiene 0 de energia" $ do
         estaEnLasUltimas (Tripulante 50) `shouldBe` False
       it "es falso si tiene mas de 50 de energia" $ do
         estaEnLasUltimas (Tripulante 51) `shouldBe` False
     describe "enfrentar un esqueleto" $ do
       it "le saca 10 de energia a menos que este en las ultimas" $ do
         enfrentarEsqueleto (Tripulante 100) `shouldBe` Tripulante 90
       it "le saca 20 de energia si esta en las ultimas" $ do
         enfrentarEsqueleto (Tripulante 40) `shouldBe` Tripulante 20
       it "no lo puede dejar en menos de 0 de energia" $ do
         enfrentarEsqueleto (Tripulante 5) `shouldBe` Tripulante 0
     describe "transportar una carga" $ do
       it "le saca tanta energia como la carga transportada" $ do
         transportarCarga 20 (Tripulante 30) `shouldBe` Tripulante 10
         transportarCarga 5 (Tripulante 10) `shouldBe` Tripulante 5
       it "no le puede dejar menos de 0 de energia" $ do
         transportarCarga 10 (Tripulante 5) `shouldBe` Tripulante 0
     describe "beber grog" $ do
       it "le aumenta la energia en 20" $ do
         beberGrog (Tripulante 0) `shouldBe` Tripulante 20
         beberGrog (Tripulante 15) `shouldBe` Tripulante 35
     describe "murio" $ do
       it "es verdad si tiene 0 de energia" $ do
         murio (Tripulante 0) `shouldBe` True
       it "es falso si tiene mas de 0 de energia" $ do
         murio (Tripulante 5) `shouldBe` False
   describe "barcos" $ do
     describe "es barco fantasma" $ do
       it "es verdad si no tiene tripulacion" $ do
         esBarcoFantasma (barcoConTripulacion []) `shouldBe` True
       it "es falso si tiene algun tripulante con vida" $ do
         esBarcoFantasma (barcoConTripulacion [Tripulante 10, Tripulante 0]) `shouldBe` False
       it "es verdad si no tiene ningun tripulante con vida" $ do
         esBarcoFantasma (barcoConTripulacion [Tripulante 0, Tripulante 0]) `shouldBe` True
     describe "metrosCuadrados" $ do
       it "los galeones miden 150m2" $ do
         metrosCuadrados (barcoDeTipo Galeon) `shouldBe` 150
       it "los bergatines miden 100m2" $ do
         metrosCuadrados (barcoDeTipo Bergatin) `shouldBe` 100
       it "los balardos miden 50m2" $ do
         metrosCuadrados (barcoDeTipo Balardo) `shouldBe` 50
     describe "llenar a maxima capacidad" $ do
       it "el barco queda con 7 de oro x metro cuadrado" $ do
         oro (llenarAMaximaCapacidad (barcoDeTipo Galeon)) `shouldBe` 1050
         oro (llenarAMaximaCapacidad (barcoDeTipo Bergatin)) `shouldBe` 700
         oro (llenarAMaximaCapacidad (barcoDeTipo Balardo)) `shouldBe` 350
       it "el barco queda con 3 balas por persona" $ do
         balas (llenarAMaximaCapacidad (barcoConTripulacion [Tripulante 0])) `shouldBe` 3
         balas (llenarAMaximaCapacidad (barcoConTripulacion [Tripulante 0, Tripulante 10])) `shouldBe` 6
     describe "enfrentamiento" $ do
       it "tras enfrentarse con un enemigo y perder, se queda sin recursos" $ do
         enfrentarse (unBarco { tipoDeBarco = Galeon, madera = 1 })
                     (unBarco { tipoDeBarco = Galeon, madera = 2 })
                        `shouldBe` (unBarco { tipoDeBarco = Galeon, oro = 0, madera = 0, balas = 0 })
       it "tras enfrentarse con un enemigo y ganar, se queda con los recursos de su enemigo" $ do
         enfrentarse (unBarco { tipoDeBarco = Galeon, madera = 2, oro = 1, balas = 2})
                     (unBarco { tipoDeBarco = Galeon, madera = 1, oro = 15, balas = 5})
                        `shouldBe` (unBarco { tipoDeBarco = Galeon, madera = 3, oro = 16, balas = 7 })
     describe "gana" $ do
       describe "siendo mas grande que el enemigo" $ do
         it "es verdad cuando tiene igual cantidad de balas" $ do
           gana ((barcoDeTipo Bergatin) { balas = 1 })
                ((barcoDeTipo Balardo) { balas = 1 }) `shouldBe` True
         it "es verdad cuando tiene mas balas" $ do
           gana ((barcoDeTipo Bergatin) { balas = 2 })
                ((barcoDeTipo Balardo) { balas = 1 }) `shouldBe` True
         it "es falso cuando tiene menos balas" $ do
           gana ((barcoDeTipo Bergatin) { balas = 1 })
                ((barcoDeTipo Balardo) { balas = 2 }) `shouldBe` False
     describe "siendo igual de grande que el enemigo" $ do
       it "es verdad cuando tiene mas madera" $ do
         gana (unBarco { madera = 2 }) (unBarco { madera = 1 }) `shouldBe` True
       it "es verdad cuando tienen igual cantidad de madera" $ do
         gana (unBarco { madera = 1 }) (unBarco { madera = 1 }) `shouldBe` True
       it "es falso cuando tiene menos madera" $ do
         gana (unBarco { madera = 1 }) (unBarco { madera = 2 }) `shouldBe` False
     describe "siendo mas chico que el enemigo" $ do
       it "es verdad cuando tiene mas tripulantes con vida" $ do
         gana ((barcoDeTipo Balardo) { tripulacion = [Tripulante 10] })
             ((barcoDeTipo Bergatin) { tripulacion = [Tripulante 0] })
             `shouldBe` True
       it "es verdad cuando tiene igual cantidad de tripulantes con vida" $ do
          gana ((barcoDeTipo Balardo) { tripulacion = [Tripulante 10] })
             ((barcoDeTipo Bergatin) { tripulacion = [Tripulante 10] })
             `shouldBe` True
       it "es verdad cuando tiene menos tripulantes con vida" $ do
         gana ((barcoDeTipo Balardo) { tripulacion = [Tripulante 10] })
             ((barcoDeTipo Bergatin) { tripulacion = [Tripulante 10, Tripulante 15] })
             `shouldBe` False
     describe "sucesos" $ do
       describe "embarcar un tesoro" $ do
         it "el barco aumenta su oro en el peso del tesoro y sus tripulantes con vida pierden tanta energia como el peso dividido la cantidad de ellos" $ do
           embarcarTesoro 150 (unBarco { tripulacion = [Tripulante 100, Tripulante 80, Tripulante 0], oro = 5 })
           `shouldBe` (unBarco { tripulacion = [Tripulante 25, Tripulante 5, Tripulante 0], oro = 155 })
       describe "encontrar un cargamento de grog" $ do
         it "toda la tripulacion del barco bebe 5 grog, aumentando su energia" $ do
           encontrarCargamentoDeGrog (barcoConTripulacion [Tripulante 0, Tripulante 100 ])
           `shouldBe` barcoConTripulacion [Tripulante 100, Tripulante 200]
       describe "enfrentar un ejercito de esqueletos" $ do
         it "si el barco tiene un solo tripulante con vida, le envia a enfrentar a todos los esqueletos uno tras otro" $ do
           enfrentarEsqueletos 5 (barcoConTripulacion [Tripulante 100])
           `shouldBe` barcoConTripulacion [Tripulante 50]
         it "si el barco no tiene tripulantes con vida, queda igual" $ do
           enfrentarEsqueletos 5 (barcoConTripulacion [])
           `shouldBe` barcoConTripulacion []
         it "si el barco tiene varios tripulantes con vida, envia al primero a pelear contra los esqueletos" $ do
           enfrentarEsqueletos 3 (barcoConTripulacion [Tripulante 60, Tripulante 20])
           `shouldBe` barcoConTripulacion [Tripulante 20, Tripulante 20]
       describe "pasarPorUnaTiendaDeGrog" $ do
         it "si el barco no puede pagar el grog, queda igual" $ do
           pasarPorUnaTiendaDeGrog (unBarco { oro = 20 }) `shouldBe` unBarco { oro = 20 }
         it "si puede pagar el grog pero no tiene tripulantes que hayan muerto, queda igual" $ do
           pasarPorUnaTiendaDeGrog (unBarco { oro = 50, tripulacion = [Tripulante 10] })
           `shouldBe` unBarco { oro = 50, tripulacion = [Tripulante 10] }
         it "si puede pagar el grog y tiene algun tripulante que haya muerto, paga el grog y se lo da al primer tripulante muerto" $ do
           pasarPorUnaTiendaDeGrog (unBarco { oro = 50, tripulacion = [Tripulante 0, Tripulante 10] })
           `shouldBe` unBarco { oro = 20, tripulacion = [Tripulante 20, Tripulante 10] }
     it "un suceso nunca le sucede a un barco fantasma" $ do
      suceder (barcoConTripulacion [Tripulante 0]) encontrarCargamentoDeGrog `shouldBe` barcoConTripulacion [Tripulante 0]
      suceder ((barcoConTripulacion [Tripulante 0]) { oro = 100 }) pasarPorUnaTiendaDeGrog `shouldBe` (barcoConTripulacion [Tripulante 0]) { oro = 100 }
     describe "travesia" $ do
       it "el barco que la logra superar sin convertirse en un barco fantasma cobra la recompensa" $ do
         realizarTravesia (Travesia [enfrentarEsqueletos 5] (\_ -> 20))
                         (unBarco { tripulacion = [Tripulante 100], oro = 0 })
                         `shouldBe` 
                         (unBarco { tripulacion = [Tripulante 50], oro = 20 })
     describe "fuerte de los condenados" $ do
       it "consiste en enfrentar 100 esqueletos pasar por una tienda de grog y embarca 30kg de oro no es fantasma, se cobra 50kg de oro como recompensa" $ do
         realizarTravesia fuerteDeLosCondenados (unBarco { tripulacion = [Tripulante 2000, Tripulante 0], oro = 30 }) `shouldBe`
              (unBarco { tripulacion = [Tripulante 985, Tripulante 5], oro = 80 })
         realizarTravesia fuerteDeLosCondenados (unBarco { tripulacion = [Tripulante 50, Tripulante 15], oro = 0 }) `shouldBe`
              (unBarco { tripulacion = [Tripulante 0, Tripulante 0], oro = 30 })
     describe "travesia del flameheart" $ do
       it "consiste en enfrentar un galeon con 4 tripulantes de 30 energia c/u, 50 tablones de madera y 50 balas de canion, luego un bergatin con 3 tripulantes con 10 de energia c/u y con 30 maderas y 30 balas de canion, luego encontrar un cargamento de grog y embarcar un tesoro de 150kg de oro, y su recompensa es de 200kg de oro por cada tripulante con vida" $ do
         realizarTravesia travesiaDelFlameheart (unBarco { tripulacion = [Tripulante 1], tipoDeBarco = Galeon, madera = 60, balas = 50, oro = 0 })
              `shouldBe` unBarco { tripulacion = [Tripulante 0], tipoDeBarco = Galeon, madera = 140, balas = 130, oro = 150 }
         realizarTravesia travesiaDelFlameheart (unBarco { tripulacion = [Tripulante 200], tipoDeBarco = Galeon, madera = 60, balas = 50, oro = 0 })
              `shouldBe` unBarco { tripulacion = [Tripulante 150], tipoDeBarco = Galeon, madera = 140, balas = 130, oro = 350 }
     describe "la girita" $ do
       it "consiste en pasar 4 veces por una tienda de grog y pelear contra un ejercito de 10 esqueletos, la recompensa es duplicar el oro del barco" $ do
         realizarTravesia laGirita (unBarco { tripulacion = [Tripulante 200, Tripulante 0, Tripulante 0, Tripulante 0, Tripulante 0], oro = 130 })
            `shouldBe` unBarco { tripulacion = [Tripulante 100, Tripulante 20, Tripulante 20, Tripulante 20, Tripulante 20], oro = 20 }
         realizarTravesia laGirita (unBarco { tripulacion = [Tripulante 0], oro = 130 })
            `shouldBe` unBarco { tripulacion = [Tripulante 0], oro = 130 }

unBarco :: Barco
unBarco = Barco { tripulacion = [Tripulante 10, Tripulante 20], tipoDeBarco = Galeon, oro = 50, balas = 5, madera = 2 }

escribime :: Expectation
escribime = implementame

