module TP where 
import Text.Show.Functions
import Data.Char

--Modelado de Cliente (Punto 1)
type Nombre      = String
type Resistencia = Int
type Amigos      = [Nombre]
data Cliente     = Cliente Nombre Resistencia Amigos deriving (Show, Eq)
--data Cliente       = Cliente { Nombre :: String, resistencia :: Int, amigos :: [nombre] } deriving (Show)

nombre (Cliente nom _ _ ) = nom
resistencia (Cliente _ res _ ) = res
amigos (Cliente _ _ ami ) = ami

agregarAmigo nuevoAmigo unCliente | nombre nuevoAmigo == nombre unCliente = unCliente
                                  | esAmigo nuevoAmigo unCliente = unCliente
                                  | otherwise = (Cliente (nombre unCliente) (resistencia unCliente) ((nombre nuevoAmigo):(amigos unCliente)))
-- Clientes (Punto 2)
rodri    = Cliente "Rodri" 55 [] 
marcos   = Cliente "Marcos" 40 ["Rodri"] 
cristian = Cliente "Cristian" 2 [] 
ana      = Cliente "Ana" 120 ["Marcos", "Rodri"] 

-- Funcion comoEsta (Punto 3)
comoEsta unCliente | (resistencia unCliente >= 50) = (nombre unCliente) ++ " fresco"
                   | (length (amigos unCliente) >= 2) = (nombre unCliente) ++ " piola"
                   | otherwise = (nombre unCliente) ++  " duro"

-- Reconocer a un cliente como amigo (Punto 4)
esAmigo unCliente otroCliente = elem (nombre unCliente) (amigos otroCliente)

-- Bebidas (Punto 5)
cambiarResistencia :: Int -> Cliente -> Cliente
cambiarResistencia valor unCliente = (Cliente (nombre unCliente) (resistencia unCliente + valor) (amigos unCliente))

cambiarNombre :: String -> Cliente -> Cliente 
cambiarNombre nuevonombre unCliente = (Cliente nuevonombre (resistencia unCliente) (amigos unCliente))

calcularRs :: Int -> String
calcularRs cantidad = replicate cantidad 'r' 

grogXD unCliente         = cambiarResistencia 0 unCliente
jarraloca unCliente      = cambiarResistencia (-10) unCliente 
klusener gusto unCliente = cambiarResistencia (-(length gusto)) unCliente
tintico unCliente        = cambiarResistencia (5 * length (amigos unCliente)) unCliente
soda fuerza unCliente    = cambiarNombre ("e" ++ (calcularRs fuerza) ++ "p" ++ (nombre unCliente)) unCliente

--Funcion Rescatarse (Punto 6)
rescatarse horas unCliente   | horas > 3 = cambiarResistencia (resistencia unCliente + 200) unCliente
                             | otherwise = cambiarResistencia (resistencia unCliente + 100) unCliente

-- Itinerario Ana (Punto 7) 
--klusener "huevo" (rescatarse 2 (klusener "chocolate" (jarraloca ana)))
--((klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraloca))ana
