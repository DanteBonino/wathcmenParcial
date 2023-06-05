module Lib () where

--Punto 1:
desarrolloDeUnaHistoria :: GrupoDeVigilantes -> Aventura -> GrupoDeVigilantes
desarrolloDeUnaHistoria unGrupoDeVigilantes = foldl (flip ($)) unGrupoDeVigilantes

type GrupoDeVigilantes = [Vigilante]

data Vigilante = Vigilante{
    alias          :: String,
    habilidades    :: [Habilidad],
    añoDeAparicion :: Int
} deriving (Show, Eq)

type Habilidad = String
type Aventura = [Evento]

type Evento = GrupoDeVigilantes -> GrupoDeVigilantes

destruccionDeNiuShork :: GrupoDeVigilantes -> GrupoDeVigilantes
destruccionDeNiuShork = (retira "Dr. Manhattan" . muerte "Rorscharch")

muerte :: String -> GrupoDeVigilantes -> GrupoDeVigilantes
muerte unAliasDeUnVigilante = filter ((/= unAliasDeUnVigilante) . alias)

retira :: String -> GrupoDeVigilantes -> GrupoDeVigilantes --No sé qué tan necesario sea esto
retira = muerte 

guerraDeVietnam :: GrupoDeVigilantes -> GrupoDeVigilantes
guerraDeVietnam = map (agregarHabilidadSoloAAgentes "Cinismo")

agregarHabilidadSoloAAgentes :: String -> Vigilante -> Vigilante
agregarHabilidadSoloAAgentes unaHabilidad unVigilante
    | esAgente unVigilante = agregarHabilidad unaHabilidad unVigilante
    | otherwise            = unVigilante

esAgente :: Vigilante -> Bool
esAgente  = (flip elem (nombresDeLosAgentes) . alias)

agentesDelGobierno :: [(String, String)]
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

nombresDeLosAgentes :: [String]
nombresDeLosAgentes = map (fst) agentesDelGobierno

agregarHabilidad :: String -> Vigilante -> Vigilante
agregarHabilidad unaHabilidad = mapearHabilidades (unaHabilidad:)

mapearHabilidades :: ([String] -> [String]) -> Vigilante -> Vigilante
mapearHabilidades f unVigilante = unVigilante {habilidades = (f . habilidades) unVigilante}

accidenteLaboratorio :: Int -> GrupoDeVigilantes -> GrupoDeVigilantes
accidenteLaboratorio unAño = agregarVigilante (Vigilante "Dr. Manhattan" ["manipulacion de la materia a nivel atómico"] unAño)

agregarVigilante :: Vigilante -> GrupoDeVigilantes -> GrupoDeVigilantes
agregarVigilante unVigilante = (unVigilante:)

actaDeKeene :: GrupoDeVigilantes -> GrupoDeVigilantes
actaDeKeene unGrupoDeVigilantes = filter (not . tieneSucesor unGrupoDeVigilantes) unGrupoDeVigilantes

tieneSucesor :: GrupoDeVigilantes -> Vigilante -> Bool
tieneSucesor unGrupoDeVigilantes unVigilante = any (tieneMismoAliasYUnAñoMasGrande unVigilante) unGrupoDeVigilantes

tieneMismoAliasYUnAñoMasGrande :: Vigilante -> Vigilante -> Bool
tieneMismoAliasYUnAñoMasGrande unVigilante otroVigilante = ((== (alias unVigilante)) . alias) otroVigilante && ((>(añoDeAparicion unVigilante)) . añoDeAparicion) otroVigilante
vigilante1 :: Vigilante
vigilante1 = Vigilante "asd" [] 1903

vigilante2 :: Vigilante
vigilante2 = Vigilante "asd" [] 1903

vigilante3 :: Vigilante
vigilante3 = Vigilante "sarasa" [] 2000

grupoDeVigilantes :: GrupoDeVigilantes
grupoDeVigilantes = [vigilante1, vigilante2, vigilante3]

--b
aventurasDeEjemplo :: Aventura
aventurasDeEjemplo = [actaDeKeene, accidenteLaboratorio 1959, guerraDeVietnam, muerte "el Comediante", destruccionDeNiuShork]

ejemplo :: GrupoDeVigilantes
ejemplo = desarrolloDeUnaHistoria grupoDeVigilantes aventurasDeEjemplo
