module TP where
import Text.Show.Functions
import Data.Char
import Data.List

-- Modelado de Cliente (Punto 1)
type Nombre = String
type Resistencia = Int
type Amigos = [Nombre]
--segunda entrega punto 1
type Bebida = Cliente -> Cliente
type Bebidas = [Bebida]

data Cliente = Cliente Nombre Resistencia Amigos Bebidas deriving (Show)
--type bebida = Cliente->Cliente
nombre (Cliente nom _ _ _) = nom
resistencia (Cliente _ res _ _) = res
amigos (Cliente _ _ ami _) = ami
bebidas (Cliente _ _ _ beb) = beb

agregarAmigo nuevoAmigo unCliente | ((nombre nuevoAmigo) == (nombre unCliente)) = unCliente
                                  | (esAmigo nuevoAmigo unCliente ) = unCliente
                                  | otherwise = (Cliente (nombre unCliente) (resistencia unCliente) ((nombre nuevoAmigo):(amigos unCliente)) (bebidas unCliente))


rodri = Cliente "Rodri" 55 [] [tintico]
marcos = Cliente "Marcos" 40 ["Rodri"] [klusener "guinda"]
cristian = Cliente "Cristian" 2 [] [grogXD, jarraloca]
ana = Cliente "Ana" 120 ["Marcos", "Rodri"] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []
---- Funcion comoEsta (primera entrega  Punto 3)
comoEsta unCliente | ((resistencia unCliente) >= 50) = (nombre unCliente) ++ " esta fresco"
                   | (((resistencia unCliente) <= 50) && ((length (amigos unCliente) >= 2))) = (nombre unCliente) ++ " esta piola"
                   | otherwise = (nombre unCliente) ++  " esta duro"
-- Reconocer a un cliente como amigo (primera entrega  Punto 4)
esAmigo unCliente otroCliente = elem (nombre unCliente) (amigos otroCliente)
-- Bebidas (primera entrega Punto 5)
cambiarResistencia :: Int -> Cliente -> Cliente
cambiarResistencia valor unCliente = (Cliente (nombre unCliente) valor (amigos unCliente) (bebidas unCliente))

cambiarNombre :: String -> Cliente -> Cliente
cambiarNombre nuevonombre unCliente = (Cliente nuevonombre (resistencia unCliente) (amigos unCliente) (bebidas unCliente))

calcularRs :: Int -> String
calcularRs cantidad = replicate cantidad 'r'

grogXD :: Bebida
grogXD unCliente = cambiarResistencia 0 unCliente

jarraloca :: Bebida
jarraloca unCliente = cambiarResistencia (resistencia unCliente - 10) unCliente

klusener :: String->Bebida
klusener gusto unCliente = cambiarResistencia ((resistencia unCliente) - (length gusto)) unCliente

tintico:: Bebida
tintico unCliente = cambiarResistencia (resistencia unCliente + (5 * (length (amigos unCliente)))) unCliente

soda::Int->Bebida
soda fuerza unCliente  = cambiarNombre ("e" ++ (calcularRs fuerza) ++ "p" ++ (nombre unCliente)) unCliente

----Funcion Rescatarse (Punto 6)
rescatarse horas unCliente   | horas > 3 = cambiarResistencia (resistencia unCliente + 200) unCliente
                             | otherwise = cambiarResistencia (resistencia unCliente + 100) unCliente

---- Itinerario Ana (Punto 7)
----klusener "huevo" (rescatarse 2 (klusener "chocolate" (jarraloca ana)))
----((klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraloca))ana

--------------------------segunda entrega----------------------
---- punto 1. V----
cantidadDeBebidasTomadas :: Cliente -> Int
cantidadDeBebidasTomadas unCliente = length (bebidas unCliente)

----punto B----
agregarBebida :: Bebida -> Cliente -> Cliente
agregarBebida beb unCliente = (Cliente (nombre unCliente) (resistencia unCliente) (amigos unCliente) ((bebidas unCliente)++[beb]))
tomarBebida :: Bebida -> Cliente -> Cliente
tomarBebida beb unCliente = beb (agregarBebida beb unCliente)

----punto C----
tomarTragos :: [Bebida] -> Cliente -> Cliente
tomarTragos listaBebidas unCliente = foldl(\unCliente trago -> tomarBebida trago unCliente) unCliente listaBebidas

----punto D----
dameOtro :: Cliente -> Cliente
dameOtro unCliente = tomarBebida (last(bebidas unCliente)) unCliente

--punto 2--
----a----
puedeTomar :: Cliente -> Bebida -> Bool
puedeTomar unCliente bebida = ((resistencia (tomarBebida bebida unCliente)) > 0)
cualesPuedeTomar :: [Bebida] -> Cliente -> [Bebida]
cualesPuedeTomar bebidas unCliente = filter (puedeTomar unCliente) bebidas

----b----
cuantasPuedeTomar :: [Bebida] -> Cliente -> Int
cuantasPuedeTomar bebida unCliente = length (cualesPuedeTomar bebida unCliente)

--Punto 3--
type NombreItinerario = String
type Duracion = Float
type Plan = Cliente -> Cliente
type Planl = [Plan]
data Itinerario = Itinerario NombreItinerario Duracion Planl deriving (Show)

nombreitinerario (Itinerario nomInt _ _) = nomInt
duracion (Itinerario _ dur _ ) = dur
planeacion (Itinerario _ _ plan) = plan

mezclaExplosiva = Itinerario "Mezcla explosiva" 2.5 [grogXD, grogXD, klusener "huevo", klusener "frutilla"]
itinerarioBasico = Itinerario "Itinerario basico" 5.0 [jarraloca, klusener "chocolate", rescatarse 2, klusener "huevo"]
salidaDeAmigos = Itinerario "Salida de amigos" 1.0 [soda 1, tintico, (agregarAmigo robertoCarlos), jarraloca]

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
{-mayorSegun intensidad itinerarioA itinerarioB  intensidad itinerarioA > intensidad itinerarioB-}


{-mayorItinerario :: Itinerario -> Itinerario
mayorItinerario itinerarioA itinerarioB | mayorSegun calcularIntensidad itinerarioA itinerarioB = itinerarioA
 								    	| otherwise = itinerarioB-}

-- itinerarioIntenso :: [Itinerario] -> Itinerario
-- itinerarioIntenso itinerariol = foldl mayorItinerario itinerariol



{-mayorSegun :: (Ord b, Num b) => (a -> b) -> a -> a -> Bool
mayorSegun funcion primerElemento segundoElemento =
	funcion primerElemento > funcion segundoElemento-}
{-
devolverMayor :: Itinerario -> Itinerario -> Itinerario
devolverMayor primerItinerario segundoItinerario
	| mayorSegun calcularIntensidad primerItinerario segundoItinerario = primerItinerario
	| otherwise = segundoItinerario
-}
{-itinerarioMasIntenso :: [Itinerario] -> Itinerario
itinerarioMasIntenso itinerarios = foldl1 mayorItinerario itinerarios-}
