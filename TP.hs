module TP where 
import Text.Show.Functions
import Data.Char
-- Modelado de Cliente (Punto 1)
type Nombre = String
type Resistencia = Int
type Amigos = [Nombre]
data Cliente = Cliente Nombre Resistencia Amigos deriving (Show, Eq)
nombre (Cliente nom _ _ ) = nom
resistencia (Cliente _ res _ ) = res
amigos (Cliente _ _ ami ) = ami
agregarAmigo nuevoAmigo unCliente | ((nombre nuevoAmigo) == (nombre unCliente)) = unCliente
								  | (esAmigo nuevoAmigo unCliente ) = unCliente
                                  | otherwise = (Cliente (nombre unCliente) (resistencia unCliente) ((nombre nuevoAmigo):(amigos unCliente)))
-- Clientes (Punto 2)
rodri = Cliente "Rodri" 55 [] 
marcos = Cliente "Marcos" 40 ["Rodri"] 
cristian = Cliente "Cristian" 2 [] 
ana = Cliente "Ana" 120 ["Marcos", "Rodri"] 
-- Funcion comoEsta (Punto 3)
comoEsta unCliente | ((resistencia unCliente) >= 50) = (nombre unCliente) ++ " esta fresco"
                   | (((resistencia unCliente) <= 50) && ((length (amigos unCliente) >= 2))) = (nombre unCliente) ++ " esta piola"
                   | otherwise = (nombre unCliente) ++  " esta duro"
-- Reconocer a un cliente como amigo (Punto 4)
esAmigo unCliente otroCliente = elem (nombre unCliente) (amigos otroCliente)
-- Bebidas (Punto 5)
cambiarResistencia :: Int -> Cliente -> Cliente
cambiarResistencia valor unCliente = (Cliente (nombre unCliente) valor (amigos unCliente))
cambiarNombre :: String -> Cliente -> Cliente 
cambiarNombre nuevonombre unCliente = (Cliente nuevonombre (resistencia unCliente) (amigos unCliente))
calcularRs :: Int -> String
calcularRs cantidad = replicate cantidad 'r' 
grogXD unCliente = cambiarResistencia 0 unCliente
jarraloca unCliente = cambiarResistencia (resistencia unCliente - 10) unCliente 
klusener gusto unCliente = cambiarResistencia ((resistencia unCliente) - (length gusto)) unCliente
tintico unCliente = cambiarResistencia (resistencia unCliente + (5 * (length (amigos unCliente)))) unCliente
soda fuerza unCliente  = cambiarNombre ("e" ++ (calcularRs fuerza) ++ "p" ++ (nombre unCliente)) unCliente



--Funcion Rescatarse (Punto 6)
rescatarse horas unCliente   | horas > 3 = cambiarResistencia (resistencia unCliente + 200) unCliente
                             | otherwise = cambiarResistencia (resistencia unCliente + 100) unCliente

-- Itinerario Ana (Punto 7) 
--klusener "huevo" (rescatarse 2 (klusener "chocolate" (jarraloca ana)))
--((klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraloca))ana