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

-- type Palo = Habilidad -> Tiro

putter :: Palo                                  -- por ejemplo en terminal : putter (habilidad bart)
putter habilidad = UnTiro{
    velocidad = 10,
    precision = precisionJugador habilidad * 2,  -- es el doble de la precision de la habilidad que le doy!! (osea este palo lo que hace es que mi tiro tenga una presicion igual al doble de la presicionDelJugador)
    altura = 0
}

-- ii) La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.

madera :: Palo                                  -- por ejemplo en terminal : madera (habilidad bart)
madera habilidad = UnTiro{
    velocidad = 100,
    precision = precisionJugador habilidad `div` 2,
    altura = 5
}

-- iii) Los hierros, que varían del 1 al 10 (número al que denominaremos n), 
-- generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0)

hierro :: Number -> Palo                       -- por ejemplo en terminal : hierro 5 (habilidad bart)
hierro n habilidad = UnTiro{
    velocidad = fuerzaJugador habilidad * n,
    precision = precisionJugador habilidad `div` n,
    altura = max 0 (n-3)  -- una altura de n-3 (con mínimo 0)
}

-- b) Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego

type Palo = Habilidad -> Tiro

palos :: [Palo]
palos = [putter , madera] ++ map hierro [1..10]     -- lo contatenamos a una lista de los palos de hierro [hierro 1 , hierro 2 , hierro 3 , .... , hierro 10]

-- 2) Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
-- Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)   -- por ej: golpe bart putter

-- 3) Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, 
-- cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos:

-- a) Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
-- independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
-- la precisión pasa a ser 100 y la altura 0.

-- FUNCIONES AUXILIARES --

yendoAlRasDelSuelo :: Tiro -> Bool
yendoAlRasDelSuelo tiro = altura tiro == 0

superaPrecision :: Number -> Tiro -> Bool
superaPrecision n tiro = precision tiro > n

superaVelocidad :: Number -> Tiro -> Bool
superaVelocidad n tiro = precision tiro > n

-- type Obstaculo = Tiro -> Tiro (ya no me sirve)
 
data Obstaculo = UnObstaculo {
    puedeSupear :: Tiro -> Bool,
    efectoLuegoDeSuperar :: Tiro -> Tiro 
}deriving (Show,Eq)

--tunelConRampita :: Obstaculo
--tunelConRampita tiro 
--  | puedeSuperartunelConRampita tiro = tiro {velocidad = velocidad tiro * 2,
--                                             precision = 100,                                 
--                                             altura = 0                     }
--  |otherwise = noSuperaObstaculo tiro

tunelConRampitaV2 :: Obstaculo             -- por ej: tunelConRampitaV2 (UnTiro {precision = 105, altura = 0, velocidad = 82})
tunelConRampitaV2 = UnObstaculo puedeSuperartunelConRampita efectoTunelConRampita

puedeSuperartunelConRampita :: Tiro -> Bool 
puedeSuperartunelConRampita tiro = precision tiro > 90  && yendoAlRasDelSuelo tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = tiro {velocidad = velocidad tiro * 2,
                                             precision = 100,                                 
                                             altura = 0           }

-- b) Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
-- Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura 
-- original dividida por el largo de la laguna.
 
type LargoLaguna = Number 

--laguna :: LargoLaguna -> Obstaculo
--laguna largo tiro 
--  | puedeSuperarLaguna tiro = tiro {altura = (altura tiro) `div` largo}
--  | otherwise = noSuperaObstaculo tiro

lagunaV2 :: LargoLaguna -> Obstaculo             -- por ej: lagunaV2 2 (UnTiro {precision = 85, altura = 3, velocidad = 82}) 
lagunaV2 largo = UnObstaculo puedeSuperarLaguna (efectoLaguna largo) 

puedeSuperarLaguna :: Tiro -> Bool
puedeSuperarLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro) 

efectoLaguna :: LargoLaguna -> Tiro -> Tiro
efectoLaguna largo tiro = tiro {altura = (altura tiro) `div` largo}

-- c) Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
-- Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

--hoyo :: Obstaculo
--hoyo tiro 
--  | puedeSuperarHoyo tiro = noSuperaObstaculo tiro
--  | otherwise = noSuperaObstaculo tiro  

hoyoV2 :: Obstaculo                            -- por ej: hoyoV2 (UnTiro {precision = 105, altura = 0, velocidad = 7})
hoyoV2 = UnObstaculo puedeSuperarHoyo efectoHoyo

puedeSuperarHoyo :: Tiro -> Bool
puedeSuperarHoyo tiro = between 5 20 (velocidad tiro) && yendoAlRasDelSuelo tiro && superaPrecision 95 tiro 

efectoHoyo :: Tiro -> Tiro 
efectoHoyo _ = tiroDetenido 

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0     -- mas facil que el de abajo

--noSuperaObstaculo :: Tiro -> Tiro
--noSuperaObstaculo tiro = tiro {velocidad = 0, precision = 0, altura = 0}

-- Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, 
-- se detiene, quedando con todos sus componentes en 0.

--obstaculoSuperableSi :: (Tiro -> Bool) -> Obstaculo -> Tiro -> Tiro     -- lo globalize/generalice todos los obstaculos
--obstaculoSuperableSi condicion efecto tiroOriginal
--  | condicion tiroOriginal = efecto tiroOriginal
--  | otherwise = tiroDetenido

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal 
  | puedeSupear obstaculo tiroOriginal = efectoLuegoDeSuperar obstaculo tiroOriginal
  | otherwise = tiroDetenido


-- 4)
-- a) Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

palosUtiles :: Jugador -> Obstaculo -> [Palo] -> [Palo]
palosUtiles jugador obstaculo palos = filter (paloUtil jugador obstaculo) palos

paloUtil :: Jugador -> Obstaculo -> Palo -> Bool
paloUtil jugador obstaculo palo = puedeSupear obstaculo (golpe jugador palo) 
 