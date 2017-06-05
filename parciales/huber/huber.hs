--Punto 1
import Text.Show.Functions

type CondicionViaje = Viaje -> Bool
data Chofer = Chofer {nombre :: String, kilometraje :: Int, viajes :: [Viaje], condicionViaje :: CondicionViaje} deriving (Show)
data Viaje = Viaje {fecha :: (Int, Int, Int), cliente :: Cliente, costo :: Float} deriving (Show)
data Cliente = Cliente {nombreCliente :: String, lugar :: String} deriving (Show)

-- Punto 2
cualquierViaje :: CondicionViaje
cualquierViaje _ = True

viajesDe200 :: CondicionViaje
viajesDe200 = (> 200) . costo

clienteNombreLargo :: Int -> CondicionViaje
clienteNombreLargo n = (> n) . length . nombreCliente . cliente

clienteNoViveEn :: String -> CondicionViaje
clienteNoViveEn donde = (/= donde) . lugar . cliente

-- Punto 3
lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

dani :: Chofer
dani = Chofer "Daniel" 23500 [Viaje (20, 4, 2017) lucas 150] (clienteNoViveEn "Olivos")

ale :: Chofer
ale = Chofer "Alejandra" 180000 [] cualquierViaje

-- Punto 4
puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = condicionViaje chofer $ viaje

-- Punto 5
liquidacionChofer :: Chofer -> Float
liquidacionChofer chofer = foldr ((+) . costo) 0 (viajes chofer)

-- Punto 6
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = hacerViaje viaje . choferConMenosViajes . filter (puedeTomarViaje viaje)

choferConMenosViajes :: [Chofer] -> Chofer
--choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2 
   | cuantosViajes chofer1 > cuantosViajes chofer2 = chofer2 
   | otherwise                                     = chofer1

cuantosViajes = length . viajes

hacerViaje viaje chofer = chofer {viajes = viaje : viajes chofer}

--Punto 7
nito = Chofer "Nito Infy" 70000 viajeInfinito$clienteNombreLargo 3

repetirViaje viaje = viaje : repetirViaje viaje

viajeInfinito = repetirViaje $ Viaje (11, 3, 2017) lucas 50
-- Al intentar liquidar el sueldo del chofer nito se crearia una ejecucion infinita por lo que nunca terminaria de realizar
-- los viajes y en consecuencia no podria hacerla la liquidacion del sueldo

-- puedeTomarViaje (Viaje (2,5,2017) lucas 50) nito
-- True
-- porque no involucra a la lista de viajes

--Punto 8
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3

--gongNeng::Ord c => c -> (c->Bool) -> (a->c) -> c -> [a] ->c