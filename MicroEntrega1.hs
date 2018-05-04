--Marisol
module MicroEntrega1 where

import Text.Show.Functions

--Cambio a sintáxis de registro

type Nombre = String
type Acumulador = Int
type PC = Int
type Memoria = [Int]
type Error = String

-----------NOTA: El campo "error" lo renombré falla, porque "error" es una palabra resevada (no compilaba)
data Micro = Micro { nombre :: Nombre, acumuladorA :: Acumulador, acumuladorB :: Acumulador, pc :: PC, memoria :: Memoria, falla:: Error} deriving (Show)

--------------Ver algun campo

verNombre :: Micro -> Nombre
verNombre micro = nombre micro

verAcumuladorA :: Micro -> Acumulador
verAcumuladorA micro = acumuladorA micro

verAcumuladorB :: Micro -> Acumulador
verAcumuladorB micro = acumuladorB micro

programCounter :: Micro -> PC
programCounter micro = pc micro

verMemoria :: Micro -> Memoria
verMemoria micro = memoria micro

mensajeError :: Micro -> Error
mensajeError micro = falla micro

-------------Micros

xt8088 :: Micro
xt8088 = Micro {nombre = "xt8088", acumuladorA = 0, acumuladorB = 0, pc = 0, memoria = (replicate 1024 0), falla = ""}

fp20 :: Micro
fp20 = Micro {nombre = "fp20", acumuladorA = 7, acumuladorB = 24, pc = 0, memoria = [],  falla = ""}

at8086 :: Micro
at8086 = Micro {nombre = "at8086", acumuladorA = 0, acumuladorB = 0, pc = 0, memoria = [1..20],  falla = ""}

------------Funciones

nop :: Micro -> Micro
nop micro = micro { pc = pc micro + 1}

avanzarTresPosiciones :: Micro -> Micro
avanzarTresPosiciones = nop.nop.nop

lodv :: Acumulador -> Micro -> Micro
lodv valor micro = nop (micro {acumuladorA = valor} )

swap :: Micro -> Micro
swap micro = nop (micro {acumuladorA = acumuladorB micro, acumuladorB = acumuladorA micro}) 
---(Micro nombre acumB acumA pc memoria error)

add :: Micro -> Micro
add micro = nop (micro {acumuladorA = (acumuladorA micro) + (acumuladorB micro) , acumuladorB = 0})

sumar :: Acumulador -> Acumulador -> Micro -> Micro
sumar unValor otroValor =  add.(lodv unValor).swap.(lodv otroValor)
--ejecucionMultiple [add,lodv otroValor,swap,lodv unValor]

str :: Acumulador -> Acumulador -> Micro -> Micro
str posicion valor micro = nop (micro {memoria = insertar posicion valor (memoria micro)} )

insertar :: Int -> a -> [a] -> [a]
insertar 1 valor xs = (valor:xs)
insertar posicion valor (x:xs) = x:insertar (posicion - 1) valor xs

-----NOTA: La función se llama lodo en lugar de lod porque no compilaba con ese nombre(al parecer es una función de librería)
lodo :: Acumulador -> Micro -> Micro
lodo posicion micro = nop (micro { acumuladorA = (valorEn posicion (memoria micro))})

valorEn :: Int -> [a] -> a 
valorEn posicion lista = lista!!(posicion-1)

divide :: Int -> Int -> Micro -> Micro
divide unValor otroValor =  dividirAcum.(lodo 1).swap.(lodo 2).(str 2 otroValor).(str 1 unValor)
---ejecucionMultiple [nop,dividirAcum,lod 1,swap,lod 2, str 2 otroValor,str 1 unValor]

--nop agregado en la función dividirAcum     
dividirAcum :: Micro -> Micro
dividirAcum (Micro nombre acumA acumB pc memoria error) | acumB == 0 = nop(Micro nombre acumA 0 pc memoria "DIVISION BY ZERO")
                                                        | otherwise = nop(Micro nombre (div acumA acumB) 0 pc memoria error)
