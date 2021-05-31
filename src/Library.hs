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

type Puntos = Number
type Palo =  Jugador -> Tiro
type Obstaculo = Tiro -> Tiro

-- Funciones útiles

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = x `elem` [n .. m]

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
putter jugador = UnTiro 10 (total "Precision" jugador (2 *) ) 0

paloDeMadera :: Palo
paloDeMadera jugador = UnTiro 100 (total "Precision" jugador (/2)) 5

hierro :: Number -> Palo
hierro n jugador = UnTiro (total "Fuerza" jugador (* n)) (total "Precision" jugador (/n)) ((n-3) `max` 0)

-- Punto 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo jugador

-- Punto 3

tiroAnulado :: Tiro
tiroAnulado = UnTiro 0 0 0

tunelConRampita :: Obstaculo
tunelConRampita (UnTiro velocidadDelTiro precisionDelTiro alturaDelTiro)
 | precisionDelTiro <= 90 || alturaDelTiro >= 1 = tiroAnulado
 | otherwise = UnTiro (velocidadDelTiro * 2) 100 0

laguna :: Number -> Obstaculo
laguna largoDeLaLaguna (UnTiro velocidadDelTiro precisionDelTiro alturaDelTiro)
  | velocidadDelTiro <= 80 || alturaDelTiro < 1 || alturaDelTiro > 5 = tiroAnulado
  | otherwise = UnTiro velocidadDelTiro precisionDelTiro (alturaDelTiro/largoDeLaLaguna)

hoyo :: Obstaculo
hoyo (UnTiro velocidadDelTiro precisionDelTiro alturaDelTiro)
 | alturaDelTiro > 0 || precisionDelTiro <= 95 || velocidadDelTiro < 5 || velocidadDelTiro > 20 = tiroAnulado
 | otherwise = tiroAnulado

 -- Punto 4

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugadorBase obstaculo = filter (\palo -> (obstaculo.palo) jugadorBase /= tiroAnulado) palosPermitidos

obstaculosSuperables :: Tiro -> [Obstaculo] -> Number
obstaculosSuperables tiroBase (obstaculo:obstaculos)
 | tiroBase == tiroAnulado || null (obstaculo:obstaculos) = 0
 | otherwise = 1 + obstaculosSuperables (obstaculo tiroBase) obstaculos

--paloMasUtil :: Jugador -> [Obstaculo] -> Palo
--paloMasUtil jugador obstaculos = map obstaculosSuperables 
--paloMasUtil jugador = maximoSegun ((max.obstaculosSuperables) (golpe jugador))

-- takeWhile (a -> Bool) ([a])
-- filter will iterate through whole input iterator while takewhile will break once the predicate turn False,
-- if you have an iterator with 1st element that false to predicate, takewhile will break at 1st iteration and return empty

-- Punto 5

padresDeNiñosNoGanadores :: [(Jugador, Puntos)] -> [String]
padresDeNiñosNoGanadores jugadores = map (padre.fst) (filter (\jugador -> jugador /= maximoSegun snd jugadores) jugadores)