--Microcontrolador
module MicroEntrega1 where

import Text.Show.Functions

--1) Lo modelamos como un data, como constructor, para respetar el orden y estructura del micro, y sumar expresividad defindiendo los tipos.
data Micro = Micro Nombre Acumulador Acumulador PC [Memoria] Error deriving Show

type Nombre = String
type Acumulador = Int
type PC = Int
type Memoria = Int
type Error = String

nombre :: Micro -> Nombre
nombre (Micro nombre _ _ _ _ _) = nombre

acumuladorA :: Micro -> Acumulador
acumuladorA (Micro _ acumA _ _ _ _) = acumA

acumuladorB :: Micro -> Acumulador
acumuladorB (Micro _ _ acumB _ _ _) = acumB

programCounter :: Micro -> PC
programCounter (Micro _ _ _ pc _ _) = pc

memoria :: Micro -> [Memoria]
memoria (Micro _ _ _ _ memoria _) = memoria

mensajeError :: Micro -> Error
mensajeError (Micro _ _ _ _ _ error) = error

xt8088 :: Micro
xt8088 = Micro "xt8088" 0 0 0 (replicate 1024 0) ""

fp20 :: Micro
fp20 = Micro "fp20" 7 24 0 [] ""

at8086 :: Micro
at8086 = Micro "at8086" 0 0 0 [1..20] ""

nop :: Micro -> Micro
nop (Micro nombre acumA acumB pc memoria error) = Micro nombre acumA acumB (pc+1) memoria error

--2) Usamos composición para aplicar funciones al micro
avanzarTresPosiciones :: Micro -> Micro
avanzarTresPosiciones = nop.nop.nop

ejecucionMultiple :: Foldable a => a (b -> b) -> b -> b
ejecucionMultiple instrucciones micro = foldr ($) micro instrucciones

lodv :: Acumulador -> Micro -> Micro
lodv valor (Micro nombre _ acumB pc memoria error) = nop (Micro nombre valor acumB pc memoria error)

swap :: Micro -> Micro
swap (Micro nombre acumA acumB pc memoria error) = nop (Micro nombre acumB acumA pc memoria error)

add :: Micro -> Micro
add (Micro nombre acumA acumB pc memoria error) = nop (Micro nombre (acumA+acumB) 0 pc memoria error)

sumar :: Acumulador -> Acumulador -> Micro -> Micro
sumar unValor otroValor = ejecucionMultiple [add,lodv otroValor,swap,lodv unValor]

str :: Acumulador -> Acumulador -> Micro -> Micro
str posicion valor (Micro nombre acumA acumB pc memoria error) = nop (Micro nombre acumA acumB pc (insertar posicion valor memoria) error)

insertar :: Int -> a -> [a] -> [a]
insertar 1 valor xs = (valor:xs)
insertar posicion valor (x:xs) = x:insertar (posicion - 1) valor xs

lod :: Acumulador -> Micro -> Micro
lod posicion (Micro nombre _ acumB pc memoria error) = nop (Micro nombre (valorEn posicion memoria) acumB pc memoria error) 

valorEn :: Int -> [a] -> a 
valorEn posicion lista = lista!!(posicion-1)

divide :: Int -> Int -> Micro -> Micro
divide unValor otroValor = ejecucionMultiple [nop,dividirAcum,lod 1,swap,lod 2, str 2 otroValor,str 1 unValor]
     
dividirAcum :: Micro -> Micro
dividirAcum (Micro nombre acumA acumB pc memoria error) | acumB == 0 = (Micro nombre acumA 0 pc memoria "DIVISION BY ZERO")
                                                        | otherwise = (Micro nombre (div acumA acumB) 0 pc memoria error)
