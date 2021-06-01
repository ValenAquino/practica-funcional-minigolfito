module Library where
import PdePreludat

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

data Obstaculos = UnObstaculo{
  condicionParaSerSuperado :: Tiro -> Bool,
  efectoDeTiro :: Tiro -> Tiro
}

type Puntos = Number
type Palo =  Jugador -> Tiro
type Obstaculo = Tiro -> Tiro

-- Funciones útiles

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = n `elem` [m .. x]

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------

-- Punto 1:

palosPermitidos:: [Palo]
palosPermitidos = [putter, paloDeMadera] ++ map hierro [1..10]

total :: String -> Jugador -> (Number -> Number) -> Number
total "Precision" jugador modificacionDelPalo = (modificacionDelPalo.precisionJugador.habilidad) jugador
total "Fuerza" jugador modificacionDelPalo = (modificacionDelPalo.fuerzaJugador.habilidad) jugador


putter :: Palo
putter jugador = UnTiro 10 (total "Precision" jugador (* 2) ) 0

paloDeMadera :: Palo
paloDeMadera jugador = UnTiro 100 (total "Precision" jugador (/2)) 5

hierro :: Number -> Palo
hierro n jugador = UnTiro (total "Fuerza" jugador (* n)) (total "Precision" jugador (/n)) ((n-3) `max` 0)


-- Punto 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo jugador

-- Punto 3

type Condicion = Tiro -> Bool

tiroAnulado :: Tiro
tiroAnulado = UnTiro 0 0 0


tunelConRampita :: Obstaculos
tunelConRampita = UnObstaculo condicionDelTunelConRampita efectoDelTunelConRampita

condicionDelTunelConRampita :: Condicion
condicionDelTunelConRampita (UnTiro velocidadDelTiro precisionDelTiro alturaDelTiro) = precisionDelTiro > 90 && alturaDelTiro == 0

efectoDelTunelConRampita :: Tiro -> Tiro
efectoDelTunelConRampita tiro = UnTiro (velocidad tiro * 2) 100 0


laguna :: Number -> Obstaculos
laguna largoDeLaLaguna = UnObstaculo condicionDeLaLaguna (efectoDeLaLaguna largoDeLaLaguna)

condicionDeLaLaguna :: Condicion
condicionDeLaLaguna (UnTiro velocidad precision altura) = velocidad > 80 &&  between altura 1 5

efectoDeLaLaguna :: Number -> Tiro -> Tiro
efectoDeLaLaguna largoDeLaLaguna (UnTiro velocidad precision altura) = UnTiro velocidad precision (altura/largoDeLaLaguna)


condicionDelHoyo :: Condicion
condicionDelHoyo (UnTiro velocidad precision altura) = between velocidad 5 20 && altura == 0 && precision > 95

efectoDelHoyo :: Tiro -> Tiro
efectoDelHoyo tiro = tiroAnulado

hoyo :: Obstaculos
hoyo = UnObstaculo condicionDelHoyo efectoDelHoyo


 -- Punto 4

palosUtiles :: Jugador -> Obstaculos -> [Palo]
palosUtiles jugadorBase obstaculo = filter (\palo -> (condicionParaSerSuperado obstaculo . palo) jugadorBase) palosPermitidos

{--
obstaculosSuperables :: [Obstaculos] -> Tiro -> Number
obstaculosSuperables [] _ = 0
obstaculosSuperables (obstaculo:obstaculos) tiroBase
 | not (condicionParaSerSuperado obstaculo tiroBase)   = 0
 | otherwise = 1 + obstaculosSuperables  obstaculos (obstaculo `efectoDeTiro`  tiroBase)
--}

obstaculosSuperables :: [Obstaculos] -> Tiro -> Number
obstaculosSuperables obstaculos tiroBase =
 (length . takeWhile (\(obstaculo, tiro) -> condicionParaSerSuperado obstaculo tiro) . zip obstaculos . tirosSucesivos tiroBase) obstaculos

tirosSucesivos :: Tiro -> [Obstaculos] -> [Tiro]
tirosSucesivos tiroBase = foldl (\tiros obstaculo -> tiros ++ [efectoDeTiro obstaculo (last tiros)] ) [tiroBase] 

paloMasUtil :: Jugador -> [Obstaculos] -> Palo
paloMasUtil jugador obstaculo = snd (maximoSegun (obstaculosSuperables obstaculo.fst ) (listaDeTirosConCadaPalo jugador))

listaDeTirosConCadaPalo :: Jugador -> [(Tiro, Palo)]
listaDeTirosConCadaPalo jugador = map (\palo -> (palo jugador, palo)) palosPermitidos

-- Punto 5

padresDeNiñosNoGanadores :: [(Jugador, Puntos)] -> [String]
padresDeNiñosNoGanadores jugadores = map (padre.fst) (filter (\jugador -> jugador /= maximoSegun snd jugadores) jugadores)