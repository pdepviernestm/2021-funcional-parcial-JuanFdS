module Library where
import PdePreludat

data Tripulante = Tripulante { energia :: Number } deriving (Show, Eq)

type ActividadDiaria = Tripulante -> Tripulante

estaEnLasUltimas :: Tripulante -> Bool
estaEnLasUltimas = (<50) . energia

aumentarEnergia :: Number -> ActividadDiaria
aumentarEnergia cantidad (Tripulante n) = Tripulante (n + cantidad)

disminuirEnergia :: Number -> ActividadDiaria
disminuirEnergia cantidad (Tripulante n) = Tripulante (max 0 $ n - cantidad)

enfrentarEsqueleto :: ActividadDiaria
enfrentarEsqueleto tripulante
    | estaEnLasUltimas tripulante = disminuirEnergia 20 tripulante
    | otherwise = disminuirEnergia 10 tripulante


transportarCarga :: Number -> ActividadDiaria
transportarCarga peso tripulante = disminuirEnergia peso tripulante

beberGrog :: ActividadDiaria
beberGrog = beberGrogs 1

beberGrogs :: Number -> ActividadDiaria
beberGrogs cantidad tripulante = aumentarEnergia (20 * cantidad) tripulante

murio :: Tripulante -> Bool
murio = (==0) . energia

data Barco = Barco { tripulacion :: [Tripulante], tipoDeBarco :: TipoDeBarco, oro :: Oro, balas :: Number, madera :: Number } deriving (Eq, Show)

data TipoDeBarco = Galeon | Bergatin | Balardo deriving (Show, Eq)

esBarcoFantasma :: Barco -> Bool
esBarcoFantasma = all murio . tripulacion


metrosCuadrados :: Barco -> Number
metrosCuadrados = tamanioSegunTipoDeBarco . tipoDeBarco

tamanioSegunTipoDeBarco :: TipoDeBarco -> Number
tamanioSegunTipoDeBarco Galeon = 150
tamanioSegunTipoDeBarco Bergatin = 100
tamanioSegunTipoDeBarco Balardo = 50

conOro :: Oro -> Barco -> Barco
conOro oro (Barco tripulacion tipoDeBarco _ balas madera) =
    Barco tripulacion tipoDeBarco oro balas madera

aumentarOro :: Oro -> Barco -> Barco
aumentarOro masOro (Barco tripulacion tipoDeBarco oro balas madera) =
    Barco tripulacion tipoDeBarco (oro + masOro) balas madera


conBalas :: Number -> Barco -> Barco
conBalas balas (Barco tripulacion tipoDeBarco oro _ madera) =
    Barco tripulacion tipoDeBarco oro balas madera

aumentarBalas :: Number -> Barco -> Barco
aumentarBalas masBalas (Barco tripulacion tipoDeBarco oro balas madera) =
    Barco tripulacion tipoDeBarco oro (balas + masBalas) madera

conMadera :: Number -> Barco -> Barco
conMadera madera (Barco tripulacion tipoDeBarco oro balas _) =
    Barco tripulacion tipoDeBarco oro balas madera

aumentarMadera :: Number -> Barco -> Barco
aumentarMadera masMadera (Barco tripulacion tipoDeBarco oro balas madera) =
    Barco tripulacion tipoDeBarco oro balas (madera + masMadera)

cantidadTripulantes :: Barco -> Number
cantidadTripulantes = length . tripulacion

llenarAMaximaCapacidad :: Barco -> Barco
llenarAMaximaCapacidad barco =
    (conOro (7 * metrosCuadrados barco) . conBalas (3 * cantidadTripulantes barco)) barco

sumarOro :: Number -> Barco -> Barco
sumarOro cantidad barco = barco { oro = oro barco + cantidad }

llevarseRecursosDe :: Barco -> Barco -> Barco
llevarseRecursosDe barco barcoSaqueado =
    (aumentarMadera (madera barcoSaqueado) .
    aumentarOro (oro barcoSaqueado) .
    aumentarBalas (balas barcoSaqueado)) barco

enfrentarse :: Barco -> Barco -> Barco
enfrentarse barco enemigo
    | gana barco enemigo = llevarseRecursosDe barco enemigo
    | otherwise = (conOro 0 . conBalas 0 . conMadera 0) barco

gana :: Barco -> Barco -> Bool
gana unBarco otroBarco
    | metrosCuadrados unBarco > metrosCuadrados otroBarco = balas unBarco >= balas otroBarco
    | metrosCuadrados unBarco == metrosCuadrados otroBarco = madera unBarco >= madera otroBarco
    | otherwise = cantidadDeTripulantesConVida unBarco >= cantidadDeTripulantesConVida otroBarco

-- Alternativa
-- tieneMas :: Ord b => (a -> b) -> a -> a -> Bool
-- tieneMas f x y = f x > f y

-- tieneMasOIgual :: Ord b => (a -> b) -> a -> a -> Bool
-- tieneMasOIgual f x y = f x >= f y

-- gana :: Barco -> Barco -> Bool
-- gana unBarco otroBarco
--     | tieneMas metrosCuadrados unBarco otroBarco = tieneMasOIgual balas unBarco otroBarco
--     | tieneMas metrosCuadrados otroBarco unBarco = tieneMasOIgual cantidadDeTripulantesConVida unBarco otroBarco
--     | otherwise  = tieneMasOIgual madera unBarco otroBarco


conVida :: Tripulante -> Bool
conVida = not . murio

cantidadDeTripulantesConVida :: Barco -> Number
cantidadDeTripulantesConVida = length . tripulantesConVida

tripulantesConVida :: Barco -> [Tripulante]
tripulantesConVida = filter conVida . tripulacion

conTripulacion :: [Tripulante] -> Barco -> Barco
conTripulacion nuevaTripulacion (Barco _ tipoDeBarco oro balas madera) =
    Barco nuevaTripulacion tipoDeBarco oro balas madera

mapTripulantes :: (Tripulante -> Tripulante) -> Barco -> Barco
mapTripulantes afectarTripulante barco =
    conTripulacion (map afectarTripulante (tripulacion barco)) barco

cargarEntreQuienesPueden :: Number -> Barco -> Barco
cargarEntreQuienesPueden peso barco = mapTripulantes (transportarCarga (peso / cantidadDeTripulantesConVida barco)) barco

type Suceso = Barco -> Barco

embarcarTesoro :: Number -> Suceso
embarcarTesoro pesoEnOro = cargarEntreQuienesPueden pesoEnOro . aumentarOro pesoEnOro

encontrarCargamentoDeGrog :: Suceso
encontrarCargamentoDeGrog = mapTripulantes (beberGrogs 5)

lucharContraEsqueletos :: Number -> Tripulante -> Tripulante
lucharContraEsqueletos cantidad tripulante = foldr ($) tripulante (replicate cantidad enfrentarEsqueleto)

conPrimerTripulanteConVida :: (Tripulante -> Tripulante) -> Barco -> Barco
conPrimerTripulanteConVida afectarTripulante barco =
    conTripulacion
        (takeWhile murio (tripulacion barco) ++
         map afectarTripulante ((take 1 . dropWhile murio . tripulacion) barco) ++
         (drop 1 . dropWhile murio . tripulacion) barco)
         barco

conPrimerTripulanteQue :: (Tripulante -> Bool) -> (Tripulante -> Tripulante) -> Barco -> Barco
conPrimerTripulanteQue condicion afectarTripulante barco =
    conTripulacion
        (takeWhile (not.condicion) (tripulacion barco) ++
         map afectarTripulante ((take 1 . dropWhile (not.condicion) . tripulacion) barco) ++
         (drop 1 . dropWhile (not.condicion) . tripulacion) barco)
         barco

enfrentarEsqueletos :: Number -> Suceso
enfrentarEsqueletos cantidad = conPrimerTripulanteQue (not . murio) (lucharContraEsqueletos cantidad)

precioGrog :: Number
precioGrog = 30

pagar :: Number -> Barco -> Barco
pagar cantidad (Barco tripulacion tipoDeBarco oro balas madera) =
    Barco tripulacion tipoDeBarco (oro - cantidad) balas madera

algunTripulanteMurio :: Barco -> Bool
algunTripulanteMurio = any murio . tripulacion

pasarPorUnaTiendaDeGrog :: Suceso
pasarPorUnaTiendaDeGrog barco
    | oro barco >= precioGrog && algunTripulanteMurio barco =
        (conPrimerTripulanteQue murio beberGrog . pagar precioGrog) barco
    | otherwise = barco

suceder :: Suceso -> Barco -> Barco
suceder suceso barco
    | esBarcoFantasma barco = barco
    | otherwise = suceso barco

type Oro = Number

type Recompensa = Barco -> Oro

data Travesia = Travesia { sucesos :: [Suceso], recompensa :: Recompensa }

pasarTodosLosSucesos :: Travesia -> Barco -> Barco
pasarTodosLosSucesos travesia barco = foldl (flip suceder) barco (sucesos travesia)

cobrarRecompensa :: Travesia -> Barco -> Barco
cobrarRecompensa travesia barco
    | esBarcoFantasma barco = barco
    | otherwise = aumentarOro (recompensa travesia barco) barco

fuerteDeLosCondenados :: Travesia
fuerteDeLosCondenados = Travesia {
    sucesos = [enfrentarEsqueletos 100, pasarPorUnaTiendaDeGrog, embarcarTesoro 30],
    recompensa = (\_ -> 50)
}

realizarTravesia :: Travesia -> Barco -> Barco
realizarTravesia travesia barco = (cobrarRecompensa travesia . pasarTodosLosSucesos travesia) barco

travesiaDelFlameheart :: Travesia
travesiaDelFlameheart = Travesia {
    sucesos = [
        flip enfrentarse (Barco {
            tripulacion = replicate 4 (Tripulante 30),
            madera = 50,
            balas = 50,
            tipoDeBarco = Galeon,
            oro = 0
        }),
        flip enfrentarse (Barco {
            tripulacion = replicate 3 (Tripulante 10),
            madera = 30,
            balas = 30,
            tipoDeBarco = Bergatin,
            oro = 0
        }),
        encontrarCargamentoDeGrog,
    embarcarTesoro 150
 ],
 recompensa = cantidadDeTripulantesConVida
}

laGirita :: Travesia
laGirita = Travesia {
    sucesos = replicate 4 pasarPorUnaTiendaDeGrog ++ [enfrentarEsqueletos 10],
    recompensa = oro
}