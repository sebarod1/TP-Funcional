import Test.HUnit
import TP

data Desconocido = ResultadoDeTipoDesconocido deriving (Show, Eq)

--
-- correr todo usando:
-- > correrTodo
--

-- DATOS PARA ARMAR LAS PRUEBAS (modificar donde corresponda)
-- Cada una de estas funciones debe retornar una tupla de aridad 3
-- cuyos valores representan (descripcion, consulta que resuelve lo pedido, resultado esperado)
-- por cuestiones de tipado, cambien los ResultadoDeTipoDesconocido para una misma función de datos de test (por ejemplo datosTest3) todos juntos
-- pueden reemplazar los undefined por la consulta que resuelve el problema por separado sin problemas porque undefined :: a


datosTest3 1 = ("Cristian debe estar duro",comoEsta cristian, "Cristian esta duro")
datosTest3 2 = ("Rodri debe estar fresco",comoEsta rodri, "Rodri esta fresco")
datosTest3 3 = ("Marcos debe estar duro",comoEsta marcos, "Marcos esta duro")
datosTest3 4 = ("Si Marcos se hace amigo de Ana y Rodri, está piola",(comoEsta.(agregarAmigo rodri).(agregarAmigo ana))marcos,"Marcos esta piola")

datosTest4 1 = ("Rodri intenta hacerse amigo de sí mismo", agregarAmigo rodri rodri,Cliente "Rodri" 55 [])
datosTest4 2 = ("Marcos intenta hacerse amigo de Rodri, de quien ya es amigo", agregarAmigo rodri marcos,Cliente "Marcos" 40 ["Rodri"])--"Cliente \"Marcos\" 40 [\"Rodri\"]")
datosTest4 3 = ("Rodri intenta hacerse amigo de Marcos, que no era su amigo", agregarAmigo marcos rodri,Cliente "Rodri" 55 ["Marcos"])--"Cliente \"Rodri\" 55 [\"Marcos\"]")

datosTest5 1 = ("Si Ana toma GrogXD queda con resistencia 0", grogXD ana, Cliente "Ana" 0 ["Marcos","Rodri"])
datosTest5 2 = ("Si Ana toma la Jarra Loca su resistencia y la de sus amigos baja", jarraloca ana, Cliente "Ana" 110 ["Marcos","Rodri"])
datosTest5 3 = ("Si Ana toma un Klusener de huevo queda con resistencia 115", klusener "huevo" ana, Cliente "Ana" 115 ["Marcos","Rodri"])
datosTest5 4 = ("Si Ana toma un Klusener de chocolate queda con resistencia 111", klusener "chocolate" ana, Cliente "Ana" 111 ["Marcos","Rodri"])
datosTest5 5 = ("Si Cristian toma un Tintico queda con resistencia 2", tintico cristian, Cliente "Cristian" 2 [])
datosTest5 6 = ("Si Ana toma un Tintico queda con resistencia 130", tintico ana, Cliente "Ana" 130 ["Marcos","Rodri"])
datosTest5 7 = ("Si Rodri toma una Soda de fuerza 2 queda con nombre errpRodri", soda 2 rodri, Cliente "errpRodri" 55 [])
datosTest5 8 = ("Si Ana toma una Soda de fuerza 10 queda con nombre errrrrrrrrrpAna", soda 10 ana, Cliente "errrrrrrrrrpAna" 120 ["Marcos","Rodri"])
datosTest5 9 = ("Si Ana toma una Soda de fuerza 0 queda con nombre epAna",soda 0 ana , Cliente "epAna" 120 ["Marcos","Rodri"])

datosTest6 1 = ("Si Rodri se rescata 5 horas queda con 255 de resistencia", rescatarse 5 rodri, Cliente "Rodri" 255 [])
datosTest6 2 = ("Si Rodri se rescata 1 hora queda con 155 de resistencia", rescatarse 1 rodri, Cliente "Rodri" 155 [])

datosTest7 1 = ("Itinerario de Ana", ((klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraloca))ana, Cliente "Ana" 196 ["Marcos","Rodri"])


--- ENTREGA 2 ---
datosEntrega2Test1b 1 = ("Marcos toma una soda de nivel 3 y queda con 2 bebidas y con 40 de resistencia", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test1c 1 = ("Rodri toma una soda de nivel 1 y una soda de nivel 2 y queda con nombre errperpRodri", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test1c 2 = ("Marcos toma un klusener de huevo, un tintico y una jarraLoca y queda con 30 de resistencia y con 4 bebidas en el historial", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test1d 1 = ("Marcos pide “dame otro” y queda con 2 bebidas en el historial y 34 de resistencia", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test1d 2 = ("Rodri toma una soda de nivel 1, y “dame otro” da como resultado que tiene 3 bebidas y su nombre queda “erperpRodri”", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test2b 1 = ("Rodri puede tomar dos bebidas, entre un grog XD, un tintico y un klusener de frutilla", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test2b 2 = ("Entre un grog XD, un tintico, un klusener de fruuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuutilla Rodri se puede tomar una sola bebida", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test3b 1 = ("Rodri hace una salida de amigos... y le pasa de todo", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test4a 1 = ("la intensidad de la mezcla explosiva es 1.6", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test4a 2 = ("la intensidad de la salidaDeAmigos es 4.0", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test4a 3 = ("la intensidad del itinerario basico es 0.8", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test4b 1 = ("Entre la salida de amigos, la mezcla explosiva y el itinerario básico, el itinerario más intenso es la salida de amigos", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test4b' 1 = ("Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico y realiza salida de amigos", undefined, ResultadoDeTipoDesconocido)

datosEntrega2Test6 1 = ("Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 0, sigue quedando con una sola amiga (Ana)", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test6 2 = ("Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 3, queda con 3 amigos (Ana, Marcos y Rodri)", undefined, ResultadoDeTipoDesconocido)
datosEntrega2Test6 3 = ("Cristian se hace amigo de Ana. Roberto Carlos se hace amigo de Cristian, toma una jarra popular de espirituosidad 4, queda con 4 amigos (Cristian, Ana, Marcos y Rodri)", undefined, ResultadoDeTipoDesconocido)

-- TESTS AUTOMATICOS (no modificar)

armarTest fDatosTest = (\(titulo, resultado, esperado) -> TestLabel titulo (resultado ~?= esperado)).fDatosTest

armarTestSuite titulo fDatosTest cant = (TestLabel titulo . TestList . map (armarTest fDatosTest)) [1.. cant]
tests = TestList [
    armarTestSuite "Entrega 1 Punto 3" datosTest3 4,
    armarTestSuite "Entrega 1 Punto 4" datosTest4 3,
    armarTestSuite "Entrega 1 Punto 5" datosTest5 9,
    armarTestSuite "Entrega 1 Punto 6" datosTest6 2,
    armarTestSuite "Entrega 1 Punto 7" datosTest7 1,
    armarTestSuite "Entrega 2 Punto 1b" datosEntrega2Test1b 1,
    armarTestSuite "Entrega 2 Punto 1c" datosEntrega2Test1c 2,
    armarTestSuite "Entrega 2 Punto 1d" datosEntrega2Test1d 2,
    armarTestSuite "Entrega 2 Punto 2b" datosEntrega2Test2b 2,
    armarTestSuite "Entrega 2 Punto 3b" datosEntrega2Test3b 1,
    armarTestSuite "Entrega 2 Punto 4a" datosEntrega2Test4a 3,
    armarTestSuite "Entrega 2 Punto 4b" datosEntrega2Test4b 1,
    armarTestSuite "Entrega 2 Punto 4b" datosEntrega2Test4b' 1,
    armarTestSuite "Entrega 2 Punto 6" datosEntrega2Test6 3
  ]
correrTodo = runTestTT tests