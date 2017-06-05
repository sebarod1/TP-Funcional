data Chico = Chico {nombreChico:: String,
					edadChico:: Int,
					habilidadesChico:: [String],
					deseosChico:: [Chico->Chico]
					}
					
data Chica = Chica {nombreChica :: String,
					condicionChica :: String -> Bool
					} 

type Padrino = Chico -> Chico

type Habilidad = String				
					
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]
chester = Chico "Chester" 10 ["ser genio"] [serMayor]
			
aprenderHabilidades habilidades = (++(habilidades)).habilidadesChico


serGrosoEnNeedForSpeed :: Chico -> Chico 
serGrosoEnNeedForSpeed unChico = habilidadesChico unChico [1,2..]

serMayor unChico = Chico (nombreChico unChico) 18 (habilidadesChico unChico) (deseosChico unChico)

-- Padrinos
hacerMadurar :: Chico -> Chico
hacerMadurar unChico = Chico (nombreChico unChico) ((edadChico unChico)+1) (habilidadesChico unChico) (deseosChico unChico)

--wanda :: Padrino
--wanda unChico = Chico (nombreChico unChico) (edadChico unChico) (habilidadesChico unChico) ( ((head.deseosChico) unChico) unChico )  

-- Aplicacion parcial
cosmo :: Padrino
cosmo unChico = Chico (nombreChico unChico) ((div) (edadChico unChico) 2) (habilidadesChico unChico) (deseosChico unChico)

muffinMagico :: Padrino
muffinMagico unChico = Chico (nombreChico unChico) (edadChico unChico) (habilidadesChico unChico) (cumplirTodosLosDeseos unChico)

cumplirTodosLosDeseos unChico = (foldl1 (.) (deseosChico unChico)) unChico 

-- Baile de Fin de Anio
tieneHabilidad :: Habilidad -> Bool
tieneHabilidad unaHabilidad unChico = elem unaHabilidad (deseosChico unChico)

-- Composicion
esMayor unChico = ((>=18).edadChico) unChico 

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = (esMayor unChico) && (tieneHabilidad manejar unChico)

 --quienConquistaA :: Chica -> [Chico]
 --quienConquistaA unaChica losPretendientes = filter (condicionChica) losPretendientes
 
 --2b Consulta
 -- quienConquistaA (Chica "Anna" tieneHabilidad cocinar) [timmy,chester]
 
habilidadesProhibidas = [enamorar,matar,dominarMundo]
 
-- Orden Superior
deseosParaTodos chicos = map cumplirTodosLosDeseos chicos

relem lista elemento = elem elemento lista

tieneHabilidadProhibida habilidades = map (relem habilidadesProhibidas)
 
infractoresDeDaRules :: [Chico]->[String]
infractoresDeDaRules chicos = filter (\chico = tieneHabilidadProhibida (take 5 (habilidadesChico chico)) ) (deseosParaTodos chicos)
