-- TP Individual Pinky y Cerebro

-- Punto 1
-- Datas

data Animal = Animal { iq::Int,
					   especie::String,
					   capacidades:: Capacidades
					   } deriving (Show,Eq)

data Experimento = Experimento { transformaciones :: [Transformacion],
								 criterioExito :: Criterio
								}

-- Tipos 

type Animales = [Animal]

type Capacidades = [String]

type Coeficientes = [Int]

type Criterio = Animal -> Bool

type Especies = [String]

type Informe = [Animal] -> [Capacidades] -> Experimento

type Transformacion = Animal -> Animal

-- Casos de prueba

cerebro = Animal 999 "raton" ["pensar"]
dumbo = Animal 100 "elefante" ["volar"]
pinky = Animal 20 "raton" ["hablar"]

-- Punto 2
-- Transformaciones

inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior numero animal = Animal (((+numero).iq) animal) (especie animal) (capacidades animal)

pinkificar :: Transformacion
pinkificar animal = Animal (iq animal) (especie animal) []

superPoderes :: Transformacion
superPoderes animal | (especie animal) == "elefante" =  agregarHabilidad "no tenerle miedo a los ratones" animal
					| esRatonInteligente animal = agregarHabilidad "hablar" animal
					| otherwise = animal

agregarHabilidad :: String -> Transformacion
agregarHabilidad habilidad animal = Animal (iq animal) (especie animal) ( ((++ [habilidad]).capacidades) animal )


esRatonInteligente :: Criterio
esRatonInteligente animal = (((== "raton").especie) animal) && (((>100).iq)animal)

-- Punto 3
-- Criterios

antropomorfico:: Criterio
antropomorfico animal = (&&) (puedeHacer "hablar" animal) (((>60).iq)animal)  

noTanCuerdo :: Criterio
noTanCuerdo = (>=2).length.(filter pinkiesco).capacidades

puedeHacer :: String -> Animal -> Bool
puedeHacer habilidad = (elem habilidad).capacidades


-- Punto 4
-- Experimentos

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento animal = (criterioExito experimento) (hacerExperimento experimento animal)

hacerExperimento :: Experimento -> Animal -> Animal
hacerExperimento experimento animal= (foldl1 (.) (transformaciones experimento)) animal

-- Ejemplo de consulta:
ratonLoco = Animal 17 "raton" ["destruenglonir el mundo", "hacer planes desalmandos"]

experimentoDeRatonLoco = experimentoExitoso (Experimento [pinkificar,(inteligenciaSuperior 10),superPoderes] antropomorfico) ratonLoco


-- Punto 5
-- Reportes
experimento0 = Experimento [(inteligenciaSuperior 100)] antropomorfico

animalesExperimentados :: Experimento -> Animales -> Animales
animalesExperimentados experimento animales = map (hacerExperimento experimento) animales

generarReporte propiedad fSuperior animales habilidades experimento = map propiedad (filter (tieneCapacidadDada habilidades) (animalesExperimentados experimento animales))
																	where tieneCapacidadDada habilidades animal = fSuperior(\habilidad -> elem habilidad (capacidades animal)) habilidades

reporteCoeficiente :: Animales -> Capacidades -> Experimento -> Coeficientes
reporteCoeficiente = generarReporte iq any

reporteEspecies :: Animales -> Capacidades -> Experimento -> Especies
reporteEspecies = generarReporte especie all

reporteCapacidades :: Animales -> Capacidades -> Experimento -> [Capacidades]
reporteCapacidades animales habilidades experimento = map capacidades (filter (tieneCapacidadDada habilidades) (animalesExperimentados experimento animales))
																	where tieneCapacidadDada habilidades animal = all(\habilidad -> (not.elem habilidad) (capacidades animal)) habilidades

-- Punto 6
-- Capacidades infinitas

ratonBravo = Animal 17 "raton" [(cycle "decir 'Oh, nenas' ")]

-- Se pueden hacer el siguiente experimento
experimento1 = Experimento [pinkificar,(inteligenciaSuperior 10)] antropomorfico
hacerExperimento1 = hacerExperimento experimento1 ratonBravo

-- No se puede hacer el siguiente experimento
experimento2 = Experimento [superPoderes] antropomorfico
hacerEexperimento2 = hacerExperimento experimento2 ratonBravo

-- La razón por esto es que Haskell trabaja con lazy evaluation, por lo que en el experimento uno no modifica a la lista infinita
-- En cambio en el experimento2 no tiene más remedio que trabajar con esa lista.


-- Bonus zone

-- Bonus A
-- Pinkiesco

isVowel :: Char -> Bool
isVowel elemento = elem elemento ['a','e','i','o','u','A','E','I','O','U'] 

haceAlgo :: String -> Bool
haceAlgo = ((== "hacer ").take 6)

palabraPinkiesca :: String -> Bool
palabraPinkiesca habilidad = ( ((<=4).length) habilidad ) && (any isVowel habilidad)

pinkiesco :: String -> Bool
pinkiesco habilidad = (haceAlgo habilidad) && ((palabraPinkiesca.(drop 6)) habilidad)

diccionarioPinkiesco :: [String]
diccionarioPinkiesco = filter palabraPinkiesca (generateWordsUpTo 4 [])

-- Bonus B
-- Generando palabras pinkiescas

generateWordsUpTo :: Int -> [String] -> [String]
generateWordsUpTo 0 lista = lista 
generateWordsUpTo numero lista = lista ++ ( generateWordsUpTo (numero-1) (generateWords numero) ) 

-- Como no se pide desarrollar generateWords, implemento esta función que está basicamente para que compilen los tipos.
generateWords :: Int -> [String]
generateWords numero = ["ah","hola","hacer miau"]