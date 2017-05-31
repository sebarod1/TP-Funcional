module TP where
import Text.Show.Functions
import Data.Char
import Data.List

--Modelado de Cliente
type Nombre      = String
type Resistencia = Int
type Amigos      = [Nombre]
type Bebida 	 = Cliente -> Cliente --segunda entrega punto 1
type Bebidas 	 = [Bebida]
data Cliente 	 = Cliente Nombre Resistencia Amigos Bebidas deriving (Show)
--data Cliente       = Cliente { Nombre :: String, Resistencia :: Int, Amigos :: [nombre], Bebidas [Bebida] } deriving (Show)

nombre (Cliente nom _ _ _) = nom
resistencia (Cliente _ res _ _) = res
amigos (Cliente _ _ ami _) = ami
bebidas (Cliente _ _ _ beb) = beb

agregarAmigo nuevoAmigo unCliente | nombre nuevoAmigo == nombre unCliente = unCliente
                                  | esAmigo nuevoAmigo unCliente = unCliente
                                  | otherwise = (Cliente (nombre unCliente) (resistencia unCliente) ((nombre nuevoAmigo):(amigos unCliente)) (bebidas UnCliente))

--Clientes
rodri         = Cliente "Rodri" 55 [] [tintico]
marcos        = Cliente "Marcos" 40 ["Rodri"] [klusener "guinda"]
cristian      = Cliente "Cristian" 2 [] [grogXD, jarraloca]
ana           = Cliente "Ana" 120 ["Marcos", "Rodri"] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []

--Funcion comoEsta
comoEsta unCliente | (resistencia unCliente >= 50) = (nombre unCliente) ++ " fresco"
                   | (length (amigos unCliente) >= 2) = (nombre unCliente) ++ " piola"
                   | otherwise = (nombre unCliente) ++  " duro"
				   
--Reconocer a un cliente como amigo
esAmigo unCliente otroCliente = elem (nombre unCliente) (amigos otroCliente)

--Bebidas
cambiarResistencia :: Int -> Cliente -> Cliente
--cambiarResistencia valor unCliente = (Cliente (nombre unCliente) valor (amigos unCliente) (bebidas unCliente))
cambiarResistencia valor unCliente = (Cliente (nombre unCliente) (resistencia unCliente + valor) (amigos unCliente) (bebidas unCliente))

cambiarNombre :: String -> Cliente -> Cliente
cambiarNombre nuevonombre unCliente = (Cliente nuevonombre (resistencia unCliente) (amigos unCliente) (bebidas unCliente))

calcularRs :: Int -> String
calcularRs cantidad = replicate cantidad 'r'

grogXD :: Bebida
grogXD unCliente = cambiarResistencia 0 unCliente

jarraloca :: Bebida
jarraloca unCliente = cambiarResistencia (-10) unCliente

klusener :: String->Bebida
klusener gusto unCliente = cambiarResistencia (-(length gusto)) unCliente

tintico:: Bebida
tintico unCliente = cambiarResistencia (5 * (length (amigos unCliente))) unCliente

soda::Int->Bebida
soda fuerza unCliente  = cambiarNombre ("e" ++ (calcularRs fuerza) ++ "p" ++ (nombre unCliente)) unCliente

----Funcion Rescatarse (Punto 6)
rescatarse horas unCliente   | horas > 3 = cambiarResistencia (resistencia unCliente + 200) unCliente
                             | otherwise = cambiarResistencia (resistencia unCliente + 100) unCliente

---- Itinerario Ana (Punto 7)
----klusener "huevo" (rescatarse 2 (klusener "chocolate" (jarraloca ana)))
----((klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraloca))ana

--------------------------segunda entrega----------------------
---Punto 1
cantidadDeBebidasTomadas :: Cliente -> Int
cantidadDeBebidasTomadas unCliente = length (bebidas unCliente)

---B
agregarBebida :: Bebida -> Cliente -> Cliente
agregarBebida beb unCliente = (Cliente (nombre unCliente) (resistencia unCliente) (amigos unCliente) ((bebidas unCliente)++[beb]))

tomarBebida :: Bebida -> Cliente -> Cliente
tomarBebida beb unCliente = beb (agregarBebida beb unCliente)

---C
tomarTragos :: [Bebida] -> Cliente -> Cliente
tomarTragos listaBebidas unCliente = foldl(\unCliente trago -> tomarBebida trago unCliente) unCliente listaBebidas

---D
dameOtro :: Cliente -> Cliente
dameOtro unCliente = tomarBebida (last(bebidas unCliente)) unCliente

--Punto 2
---A
puedeTomar :: Cliente -> Bebida -> Bool
puedeTomar unCliente bebida = ((resistencia (tomarBebida bebida unCliente)) > 0)

cualesPuedeTomar :: [Bebida] -> Cliente -> [Bebida]
cualesPuedeTomar bebidas unCliente = filter (puedeTomar unCliente) bebidas

---B
cuantasPuedeTomar :: [Bebida] -> Cliente -> Int
cuantasPuedeTomar bebida unCliente = length (cualesPuedeTomar bebida unCliente)

--Punto 3
type NombreItinerario = String
type Duracion         = Float
type Plan             = Cliente -> Cliente
type Planl            = [Plan]
data Itinerario       = Itinerario NombreItinerario Duracion Planl deriving (Show)

nombreitinerario (Itinerario nomInt _ _) = nomInt
duracion (Itinerario _ dur _ ) = dur
planeacion (Itinerario _ _ plan) = plan

mezclaExplosiva  = Itinerario "Mezcla explosiva" 2.5  [grogXD, grogXD, klusener "huevo", klusener "frutilla"]
itinerarioBasico = Itinerario "Itinerario basico" 5.0 [jarraloca, klusener "chocolate", rescatarse 2, klusener "huevo"]
salidaDeAmigos   = Itinerario "Salida de amigos" 1.0  [soda 1, tintico, (agregarAmigo robertoCarlos), jarraloca]

hacerPlan :: Cliente -> Plan -> Cliente
hacerPlan unCliente plan = plan unCliente

hacerItinerario :: Cliente->Itinerario->Cliente
hacerItinerario unCliente itinerario = foldl (hacerPlan) unCliente (planeacion itinerario)

----B----
--hacerItinerario rodri salidaDeAmigos
--hacerItinerario marcos mezclaExplosiva

--punto 4--
--intensidad la calculo como cantidad de plan en planl dividido duracion
--genericLength The genericLength function is an overloaded version of length.
--In particular, instead of returning an Int, it returns any type which is an instance of Num.
calcularIntensidad :: Itinerario -> Float
calcularIntensidad itinerario = (genericLength (planeacion itinerario)) / (duracion itinerario)

-- mayorSegun :: (Ord b, Num b) => (a -> b) -> a -> a -> Bool
mayorSegun intensidad itinerarioA itinerarioB = intensidad itinerarioA > intensidad itinerarioB

mayorItinerario :: Itinerario -> Itinerario -> Itinerario
mayorItinerario itinerarioA itinerarioB | mayorSegun calcularIntensidad itinerarioA itinerarioB = itinerarioA
 								    	| otherwise = itinerarioB

--use FOLDL1 que facilita las cosas. it takes the first 2 items of the list and applies the function to them, 
--then feeds the function with this result and the third argument and so on.
itinerarioIntenso :: [Itinerario] -> Itinerario
itinerarioIntenso itinerariol = foldl1 mayorItinerario itinerariol

--punto 5, con map sale con fritas, en caso de correr y pone chuckNorris en haskell con Ctrl+C lo interumpis.
chuckNorris = Cliente "Chuck" 1000 ["Ana"] (map soda [1..])

{-
b)como poder se puede, haskell no va a tirar ningun error. la cuestion es que al ser un lista infinita,
nunca se va a poder conocer cual es el ultimo trago, siempre hay un elemento mas en la lista y haskell intenta reducir
al ultimo elemento. Evaluacion Diferida.
c)si
resistencia ana < resistencia chuckNorris 
True
esto se logra gracias a que haskell es "lazy" osea no evalua si no se usa, por eso jamas evalua la lista de bebidas,
solo se fija en la resistencia.
d)si, mismo motivo que la anterior. ya que es lazy solo evalua las abstracciones que se usan.
-}
--punto 6--
{-agregarAmigos :: Cliente -> [] -> cliente
agregarAmigos cliente (cabeza:cola)-}
