module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
 
 describe "Tests de prueba" $ do
      it "la verdad y la verdad" $ do
         True `shouldBe` True 
 

escribime :: Expectation
escribime = implementame