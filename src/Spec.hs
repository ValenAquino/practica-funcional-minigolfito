module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate, AsyncException (ThreadKilled))

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
   describe "Tunel con Trampita" $ do
      it "Con una precisión de 100 y altura 1, mi tiro no pasa el tunel" $ do
         condicionDelTunelConRampita (UnTiro 100 100 1) `shouldBe` False
      it "Con una precisión de 90 y altura 0, mi tiro no pasa el tunel" $ do
         condicionDelTunelConRampita (UnTiro 100 90 0) `shouldBe` False 
      it "Con una precisión de 91 y altura 0, mi tiro pasa el tunel" $ do
         condicionDelTunelConRampita (UnTiro 100 91 0) `shouldBe` True 
   describe "Laguna" $ do
      it "Con una velocidad de 80 y altura 4, mi tiro no pasa la laguna de 5 metros de largo" $ do
          condicionDeLaLaguna (UnTiro 80 100 4) `shouldBe` False 
      it "Con una velocidad de 120 y altura 0, mi tiro no pasa la laguna" $ do
          condicionDeLaLaguna (UnTiro 120 100 0) `shouldBe` False  
      it "Con una velocidad de 81 y altura 1, mi tiro pasa la laguna de 5 metros de largo" $ do
          condicionDeLaLaguna (UnTiro 81 100 1) `shouldBe` True 
      it "Con una velocidad de 81 y altura 5, mi tiro pasa la laguna de 5 metros de largo" $ do
          condicionDeLaLaguna (UnTiro 81 100 5) `shouldBe` True 
   describe "Hoyo" $ do
      it "Un tiro con velocidad 5, precisión 9 y altura 0 no consigue superar al hoyo" $ do
         condicionDelHoyo (UnTiro 5 9 0) `shouldBe` False 
      it "Un tiro con velocidad 20, precisión 9 y altura 0 no consigue superar al hoyo" $ do
         condicionDelHoyo (UnTiro 20 9 0) `shouldBe` False 
      it "Un tiro con velocidad 5, altura 0 y presición de 96, logra superar el hoyo" $ do
         condicionDelHoyo (UnTiro 5 96 0) `shouldBe` True   


{--}
 describe "Punto 4" $ do
    describe "Palos Utiles" $ do
       it "La mejor manera de que bart supere un tunelConRampita, es con un putter" $ do
          golpe bart (head (palosUtiles bart tunelConRampita)) `shouldBe` UnTiro {velocidad = 10, precision = 120, altura = 0}  
    describe "obstaculos superables" $ do
       it "Para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo, el resultado sería 2" $ do
          obstaculosSuperables [tunelConRampita,tunelConRampita,hoyo] (UnTiro 10 95 0) `shouldBe` 2
    describe "Palo mas util" $ do
       it "el palo mas util" $ do
          golpe bart (paloMasUtil bart [tunelConRampita, tunelConRampita, hoyo]) `shouldBe` UnTiro {velocidad = 10, precision = 120, altura =0}
 describe "Punto 5" $ do
    describe "padresDeNiñosNoGanadores" $ do
       it "Homero y Gorgory perdieron la apuesta porque bart hizo 9 puentos, tafa 9 y todd 10" $ do
          padresDeNiñosNoGanadores [(bart, 9), (todd, 10), (rafa, 8)] `shouldBe` ["Homero","Gorgory"]
{--}
escribime :: Expectation
escribime = implementame
