--Microcontrolador
module MicroEntrega1 where

import Text.Show.Functions

--1) Lo modelamos como un data, como constructor, para respetar el orden y estructura del micro, y sumar expresividad defindiendo los tipos.
data Micro = Micro{acumuladorA :: Acumulador, acumuladorB :: Acumulador, programCounter :: PC, programas :: [Programa], memoria :: [Memoria], mensajeError :: Error} deriving Show

type Acumulador = Int
type PC = Int
type Memoria = Int
type Error = String
type Programa = Micro -> Micro

xt8088 :: Micro
xt8088 = Micro 0 0 0 [suma10y22] (replicate 1024 0) ""

fp20 :: Micro
fp20 = Micro 7 24 0 [division2y0,suma10y22] [] ""

at8086 :: Micro
at8086 = Micro 0 0 0 [] [1..20] ""

infinity01 :: Micro
infinity01 = Micro 0 0 0 [] [0..] ""

microDesOrden :: Micro
microDesOrden = Micro 0 0 0 [] [2,5,1,0,6,9] ""

nop :: Micro -> Micro
nop micro = micro {programCounter = (+1) (programCounter micro)}

--2) Usamos composición para aplicar funciones al micro
avanzarTresPosiciones :: Micro -> Micro
avanzarTresPosiciones = nop.nop.nop

ejecutarInstruccion :: (Micro -> Micro) -> Micro -> Micro
ejecutarInstruccion instruccion micro | (mensajeError micro) /= [] = micro
                                      | otherwise = (nop.instruccion) micro

ejecucionMultiple :: Foldable a => a (Micro -> Micro) -> Micro -> Micro
ejecucionMultiple instrucciones micro = foldr ejecutarInstruccion micro instrucciones

lodv :: Acumulador -> Micro -> Micro
lodv valor micro = micro {acumuladorA = valor}

swap :: Micro -> Micro
swap micro = micro {acumuladorA = acumuladorB micro, acumuladorB = acumuladorA micro}

add :: Micro -> Micro
add (Micro acumA acumB pc programas memoria error) = lodv (acumA+acumB) (Micro 0 0 pc programas memoria error)

sumar :: Acumulador -> Acumulador -> Micro -> Micro
sumar unValor otroValor = ejecucionMultiple [add,lodv otroValor,swap,lodv unValor]

str :: Acumulador -> Acumulador -> Micro -> Micro
str posicion valor micro = micro {memoria = insertar posicion valor (memoria micro)}

insertar :: Int -> a -> [a] -> [a]
insertar 1 valor xs = (valor:xs)
insertar posicion valor (x:xs) = x:insertar (posicion - 1) valor xs

lod :: Acumulador -> Micro -> Micro
lod posicion micro = lodv (valorEn posicion (memoria micro)) micro

valorEn :: Int -> [a] -> a 
valorEn posicion lista = lista!!(posicion-1)

divide :: Int -> Int -> Micro -> Micro
divide unValor otroValor = ejecucionMultiple [dividirAcum,lod 1,swap,lod 2, str 2 otroValor,str 1 unValor]

dividirAcum :: Micro -> Micro
dividirAcum (Micro  acumA acumB pc programas memoria error) | acumB == 0 = (Micro  acumA 0 pc programas memoria "DIVISION BY ZERO")
                                                            | otherwise = (Micro  (div acumA acumB) 0 pc programas memoria error)
--3.1
cargar :: [Programa] -> Micro -> Micro   
cargar programa micro = micro {programas = programas micro ++ programa}

suma10y22 :: Micro -> Micro
suma10y22 = sumar 10 22

division2y0 :: Micro -> Micro
division2y0 = divide 2 0

--3.2
ejecutarPrograma :: Micro -> Micro
ejecutarPrograma micro = foldl (flip ($)) micro (programas micro)

--3.3
ifnz:: Micro -> Micro
ifnz micro | acumuladorA (ejecutarPrograma micro) /= 0 = ejecutarPrograma micro
           | otherwise = micro 
--3.4
depurar :: Micro -> Micro
depurar micro = micro {programas = filter (instruccionNecesaria micro) (programas micro)}

instruccionNecesaria :: a -> (a -> Micro) -> Bool
instruccionNecesaria micro instruccion = acumuladorA (instruccion micro) /= 0 && acumuladorB (instruccion micro) /= 0 && (not.all (==0)) (memoria (instruccion micro))

--3.5
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True   
estaOrdenada (x:xs) | x <= head xs = estaOrdenada xs
                    | otherwise = False                           

--3.6
-- Al querer cargarle un programa o hacer cualquier tipo de operacion que implique que el micro se muestre
-- por pantalla no va a tener fin porque intenta mostarar cada elemento de la lista. (listas infinitas)

-- Nunca vamos a poder determinar con certeza si la memoria esta ordenada pero si podemos saber si no lo esta (lazy evaluation)                                                                                    
