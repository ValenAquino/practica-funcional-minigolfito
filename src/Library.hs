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

type Palo =  Jugador -> Tiro

const :: p -> [Palo]
const palosPermitidos = [putter, paloDeMadera, hierro 1, hierro 2, hierro 3, hierro 4, hierro 5, hierro 6, hierro 7, hierro 8, hierro 9, hierro 10]

total :: String -> Jugador -> (Number -> Number) -> Number 
total "Precision" jugador modificacionDelPalo = (modificacionDelPalo.precisionJugador.habilidad) jugador
total "Fuerza" jugador modificacionDelPalo = (modificacionDelPalo.fuerzaJugador.habilidad) jugador

putter :: Palo
putter jugador = UnTiro 10 (total "Precision" jugador (2 *) ) 0 

paloDeMadera :: Palo
paloDeMadera jugador = UnTiro 100 (total "Precision" jugador (/2)) 5 

hierro :: Number -> Palo
hierro n jugador 
 | n >= 3 = UnTiro (total "Fuerza" jugador (* n)) (total "Precision" jugador (/n)) (n-3)
 | otherwise = UnTiro (total "Fuerza" jugador (* n)) (total "Precision" jugador (/n)) 0

-- Punto 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo jugador

