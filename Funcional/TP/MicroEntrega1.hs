--Microcontrolador
module MicroEntrega1 where

import Text.Show.Functions

--1) Lo modelamos como un data, como constructor, para respetar el orden y estructura del micro, y sumar expresividad defindiendo los tipos.
data Micro = Micro{acumuladorA :: Acumulador, acumuladorB :: Acumulador, programCounter :: PC, memoria :: [Memoria], mensajeError :: Error} deriving Show

type Acumulador = Int
type PC = Int
type Memoria = Int
type Error = String

xt8088 :: Micro
xt8088 = Micro 0 0 0 (replicate 1024 0) ""

fp20 :: Micro
fp20 = Micro 7 24 0 [] ""

at8086 :: Micro
at8086 = Micro 0 0 0 [1..20] ""

nop :: Micro -> Micro
nop (Micro acumA acumB pc memoria error) = Micro acumA acumB (pc+1) memoria error

--2) Usamos composición para aplicar funciones al micro
avanzarTresPosiciones :: Micro -> Micro
avanzarTresPosiciones = nop.nop.nop

ejecutar :: (a -> Micro) -> a -> Micro
ejecutar instruccion = nop.instruccion

ejecucionMultiple :: Foldable a => a (Micro -> Micro) -> Micro -> Micro
ejecucionMultiple instrucciones micro = foldr ejecutar micro instrucciones

lodv :: Acumulador -> Micro -> Micro
lodv valor (Micro _ acumB pc memoria error) = Micro valor acumB pc memoria error

swap :: Micro -> Micro
swap (Micro acumA acumB pc memoria error) = Micro acumB acumA pc memoria error

add :: Micro -> Micro
add (Micro  acumA acumB pc memoria error) = lodv (acumA+acumB) (Micro 0 0 pc memoria error)

sumar :: Acumulador -> Acumulador -> Micro -> Micro
sumar unValor otroValor = ejecucionMultiple [add,lodv otroValor,swap,lodv unValor]

str :: Acumulador -> Acumulador -> Micro -> Micro
str posicion valor (Micro acumA acumB pc memoria error) = Micro acumA acumB pc (insertar posicion valor memoria) error

insertar :: Int -> a -> [a] -> [a]
insertar 1 valor xs = (valor:xs)
insertar posicion valor (x:xs) = x:insertar (posicion - 1) valor xs

lod :: Acumulador -> Micro -> Micro
lod posicion (Micro  _ acumB pc memoria error) = Micro (valorEn posicion memoria) acumB pc memoria error

valorEn :: Int -> [a] -> a 
valorEn posicion lista = lista!!(posicion-1)

divide :: Int -> Int -> Micro -> Micro
divide unValor otroValor = ejecucionMultiple [nop,dividirAcum,lod 1,swap,lod 2, str 2 otroValor,str 1 unValor]

dividirAcum :: Micro -> Micro
dividirAcum (Micro  acumA acumB pc memoria error) | acumB == 0 = (Micro  acumA 0 pc memoria "DIVISION BY ZERO")
                                                  | otherwise = (Micro  (div acumA acumB) 0 pc memoria error)                                         
