module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = UnaHabilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--También necesitaremos modelar los palos de golf que pueden usarse y los obstáculos que deben enfrentar para ganar el juego.

-- 1) Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo correcto puede ser la diferencia entre ganar o perder el torneo.
-- a) Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura

-- i) El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.

putter :: Habilidad -> Tiro
putter habilidad = UnTiro{
    velocidad = 10,
    precision = precisionJugador habilidad * 2,  -- es el doble de la precision de la habilidad que le doy!! (osea este palo lo que hace es que mi tiro tenga una presicion igual al doble de la presicionDelJugador)
    altura = 0
}

-- ii) La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.

madera :: Habilidad -> Tiro
madera habilidad = UnTiro{
    velocidad = 100,
    precision = precisionJugador habilidad `div` 2,
    altura = 5
}

-- iii) Los hierros, que varían del 1 al 10 (número al que denominaremos n), 
-- generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0)

hierro :: Number -> Habilidad -> Tiro
hierro n habilidad = UnTiro{
    velocidad = fuerzaJugador habilidad * n,
    precision = precisionJugador habilidad `div` n,
    altura = max 0 (n-3)  -- una altura de n-3 (con mínimo 0)
}