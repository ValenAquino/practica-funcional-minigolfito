module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

-- Jugadores de ejemplo

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

correrTests :: IO ()
correrTests = hspec $ do
 
 describe "Punto 1" $ do
   describe "Putter" $ do
      it "Bart con un putter realiza tiros de velocidad 10, precision 120 y altura 0" $ do
         putter bart `shouldBe`  UnTiro 10 120 0
   describe "Madera" $ do
      it "Todd con un palo de madera realiza tiros de velocidad 100, precision 40 y altura 5" $ do
         paloDeMadera todd `shouldBe`  UnTiro 100 40 5
   describe "Hierro" $ do
      it "Rafa con un palo de Hierro y n 3 realiza tiros de velocidad 30, precision 1/3 y altura 0" $ do
         hierro 3 rafa `shouldBe`  UnTiro 30 (1/3) 0
      it "Rafa con un palo de Hierro y n 2 realiza tiros de velocidad 20, precision 1/2 y altura 0" $ do
         hierro 2 rafa `shouldBe`  UnTiro 20 (1/2) 0
      it "Rafa con un palo de Hierro y n 10 realiza tiros de velocidad 100, precision 1/10 y altura 6" $ do
         hierro 10 rafa `shouldBe`  UnTiro 100 (1/10) 7
   
 describe "Punto 2" $ do
   describe "golpe" $ do
      it "el golpe de todd con un palo de madera devuelve un tito de velocidad 100, precision 40 y altura 5" $ do
         golpe todd paloDeMadera `shouldBe` UnTiro 100 40 5

 describe "Punto 3" $ do
   describe "" $ do
      it "" $ do
         2+2 `shouldBe` 4


--------------------------------------
   describe "Punto n" $ do
      describe "" $ do
         it "" $ do
            2+2 `shouldBe` 4

escribime :: Expectation
escribime = implementame