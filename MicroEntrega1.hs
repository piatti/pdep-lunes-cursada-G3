module MicroEntrega1 where
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
xt8088 = Micro "xt8088" 0 0 0 [] ""

fp20 :: Micro
fp20 = Micro "fp20" 7 24 0 [] ""

at8086 :: Micro
at8086 = Micro "at8086" 0 0 0 [1..20] ""

nop :: Micro -> Micro
nop (Micro nombre acumA acumB pc memoria error) = Micro nombre acumA acumB (pc+1) memoria error

avanzarTresPosiciones :: Micro -> Micro
avanzarTresPosiciones = nop.nop.nop

lodv :: Acumulador -> Micro -> Micro
lodv valor (Micro nombre _ acumB pc memoria error) = nop (Micro nombre valor acumB pc memoria error)

swap :: Micro -> Micro
swap (Micro nombre acumA acumB pc memoria error) = nop (Micro nombre acumB acumA pc memoria error)

add :: Micro -> Micro
add (Micro nombre acumA acumB pc memoria error) = nop (Micro nombre (acumA+acumB) 0 pc memoria error)

sumar :: Acumulador -> Acumulador -> Micro -> Micro
sumar unValor otroValor = add.lodv otroValor.swap.lodv unValor

str :: Acumulador -> Acumulador -> Micro -> Micro
str posicion valor (Micro nombre acumA acumB pc memoria error) = nop (Micro nombre acumA acumB pc (insertar posicion valor memoria) error)

insertar :: Int -> a -> [a] -> [a]
insertar 1 valor xs = (valor:xs)
insertar posicion valor (x:xs) = x:insertar (posicion - 1) valor xs

lod :: Acumulador -> Micro -> Micro
lod valor (Micro nombre _ acumB pc memoria error) = nop (Micro nombre (memoria!!(valor-1)) acumB pc memoria error) 

divide :: Int -> Int -> Micro -> Micro
divide unValor otroValor (Micro nombre acumA acumB pc memoria error) 
        | otroValor == 0 = (nop.lod 1.swap.lod 2.str 2 otroValor.str 1 unValor) (Micro nombre acumA acumB pc memoria "DIVISION BY ZERO")
        | otherwise = (nop.dividirAcum.lod 1.swap.lod 2.str 2 otroValor.str 1 unValor) (Micro nombre acumA acumB pc memoria error)

dividirAcum :: Micro -> Micro
dividirAcum (Micro nombre acumA acumB pc memoria error) = (Micro nombre (div acumA acumB) 0 pc memoria error)

