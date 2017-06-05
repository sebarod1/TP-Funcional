module TP where 
import Text.Show.Functions
import Data.Char

data Persona = Persona {edad :: Float,
	peso :: Float,
	tonificacion :: Float} deriving (Show)

pancho = Persona 40 120 1
andres = Persona 22 80 6

----punto 1----
saludable :: Persona -> Bool
saludable unaPersona = not (obesa unaPersona) && tonificacion unaPersona > 5

obesa :: Persona -> Bool
obesa unaPersona = peso unaPersona > 100

----punto 2----
quemarCalorias :: Persona -> Float -> Persona
quemarCalorias unaPersona calorias | obesa unaPersona = Persona (edad unaPersona) ((peso unaPersona)  - (calorias / 150)) (tonificacion unaPersona)
									| edad unaPersona > 30 && calorias > 200 = Persona (edad unaPersona) ((peso unaPersona)  - 1) (tonificacion unaPersona)
									| otherwise = Persona (edad unaPersona) ((peso unaPersona)  - (calorias / ((edad unaPersona) * (peso unaPersona)))) (tonificacion unaPersona)

----punto 3----
type Ejercicio = Float -> Persona -> Persona

caminata :: Ejercicio
caminata tiempo unaPersona =  quemarCalorias unaPersona (5*tiempo) -- cinta 5 tiempo unaPersona
-- caminata = cinta 5 -- "simplifico" unaPersona y minuto

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta tiempo unaPersona = quemarCalorias unaPersona ((6+(tiempo/5)/2)*tiempo) -- cinta (promedio) tiempo unaPersona

pesas :: Float -> Ejercicio -- le paso tambien el peso
pesas carga tiempo unaPersona | tiempo > 10 = tonificar (-(carga/10)) unaPersona--Persona (edad unaPersona) (peso unaPersona) ((tonificacion unaPersona) - (carga/10))
							 | otherwise = unaPersona

colina :: Float -> Ejercicio
colina inclinacion tiempo unaPersona = quemarCalorias unaPersona (2*tiempo*inclinacion)

montana :: Float -> Ejercicio
montana inclinacion tiempo unaPersona = (tonificar 1.colina(inclinacion/3)(tiempo/2).colina inclinacion (tiempo/2))unaPersona

tonificar :: Float -> Persona -> Persona
tonificar tonif unaPersona = Persona (edad unaPersona) (peso unaPersona) ((tonificacion unaPersona) + tonif)

cinta :: Float -> Ejercicio --le paso la velocdad
cinta velocidad tiempo unaPersona = quemarCalorias unaPersona (tiempo * velocidad)

----punto 4----
