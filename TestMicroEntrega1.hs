import Test.Hspec
import MicroEntrega1

testMicroEntrega1 = hspec $ do 
 describe "Test Punto 1" $ do
 it "El procesador xt8088 debe tener los acumuladores, pc y memoria en 0 y no tener mensajes de error" $ do
  (acumuladorA xt8088) `shouldBe` 0
  (acumuladorB xt8088) `shouldBe` 0
  (programCounter xt8088) `shouldBe` 0
  (all (==0) (memoria xt8088)) `shouldBe` True
  (mensajeError xt8088) `shouldBe` ""


 describe "Test Punto 2" $ do
 it "La instruccion NOP debe aumentar el program counter en uno" $ do
  ((programCounter.nop) xt8088) `shouldBe` (programCounter xt8088) + 1
 
 it "La instruccion 'avanzarTresPosiciones' debe aumentar el program counter en tres, no variar acumuladores ni memoria y no generar mensajes de error" $ do
  ((programCounter.avanzarTresPosiciones) xt8088) `shouldBe` (programCounter xt8088) + 3
  ((acumuladorA.avanzarTresPosiciones) xt8088) `shouldBe` (acumuladorA xt8088)
  ((acumuladorB.avanzarTresPosiciones) xt8088) `shouldBe` (acumuladorB xt8088)
  ((memoria.avanzarTresPosiciones) xt8088) `shouldBe` (memoria xt8088)
  ((mensajeError.avanzarTresPosiciones) xt8088) `shouldBe` (mensajeError xt8088)


 describe "Test Punto 3" $ do
 it "La instruccion LODV 5  debe cargar este valor en el acumulador A sin modificar el acumulador B" $ do
  ((acumuladorA.lodv 5) xt8088) `shouldBe` 5
  ((acumuladorB.lodv 5) xt8088) `shouldBe` (acumuladorB xt8088)

 it "La instruccion SWAP intercambia los valores de los acumuladores" $ do
  ((acumuladorA.swap) fp20) `shouldBe` (acumuladorB fp20)
  ((acumuladorB.swap) fp20) `shouldBe` (acumuladorA fp20)

 it "La instruccion ADD suma, y almacena en el acumulador A, los valores de ambos acumuladores" $ do
  ((acumuladorA.add) fp20) `shouldBe` (acumuladorA fp20)+(acumuladorB fp20)
  ((acumuladorB.add) fp20) `shouldBe` 0 

 it "El procesador fp20 debe tener el acumuador A en 32, el acumulador B en 0, el program counter en 4, la memoria vacia y no tener mensajes de error luego de sumar '10+22'" $ do
  ((acumuladorA.sumar 10 22) fp20) `shouldBe` 32
  ((acumuladorB.sumar 10 22) fp20) `shouldBe` 0
  ((programCounter.sumar 10 22) fp20) `shouldBe` 4  
  ((memoria.sumar 10 22) fp20) `shouldBe` []
  ((mensajeError.sumar 10 22) fp20) `shouldBe` ""  
 
     
 describe "Test Punto 4" $ do
 it "Para el procesador at8086, la instruccion STR 2 5 deja un 5 en la posicion 2 de memoria" $ do
  ((valorEn 2.memoria.str 2 5) at8086) `shouldBe` 5 
 it "La instruccion LOD 2 para un procesador xt8088 debe dejar el acumulador A en 0" $ do
  ((acumuladorA.lod 2) xt8088) `shouldBe` 0
 it "La instruccion DIVIDE 2 0 debe devolver el mensaje de error 'DIVISION BY ZERO'" $ do
  ((mensajeError.divide 2 0) xt8088) `shouldBe` "DIVISION BY ZERO"
 it "La instruccion DIVIDE 12 4 debe dar 3 y no devolverningun mensaje de error" $ do
  ((mensajeError.divide 12 4) xt8088) `shouldBe` (mensajeError xt8088)  
  ((acumuladorA.divide 12 4) xt8088) `shouldBe` 3   


